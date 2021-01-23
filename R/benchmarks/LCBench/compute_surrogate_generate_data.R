# Setup script for initially setting up the benchmarks 

library(batchtools)

source("R/benchmarks/LCBench/compute_surrogate_config.R")

lapply(packages, require, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "R/LCBench/compute_surrogate_config.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_len(length(tasks))) {
  addProblem(
    name = tasks[i], 
    data = paste(TASK_LOCATION, tasks[i], "lcbench2000.csv", sep = "/"), 
    reg = reg
  )
}

for (i in 1:length(ALGORITHMS)) {
  addAlgorithm(name = names(ALGORITHMS)[i], reg = reg, fun = ALGORITHMS[[i]]$fun)  
}

addExperiments(
  reg = reg, 
  algo.designs = ades, 
  repls = 1L)


# --- 3. SUBMIT ON LRZ ---
resources.serial = list(
  walltime = 3600L * 24L * 4L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)


reg = loadRegistry(registry_name, writeable = TRUE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem"))

tosubmit = tab
tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 5L)

submitJobs(tosubmit[chunk == 1, ], resources = resources.serial)

res = reduceResultsDataTable(findDone(), function(x) {
  model_val_balanced_accuracy = x$opdf_val_balanced_accuracy[final.model.avail == TRUE, ]$model
  model_test_balanced_accuracy = x$opdf_test_balanced_accuracy[final.model.avail == TRUE, ]$model
  return(list(model_val_balanced_acc = model_val_balanced_accuracy, model_test_balanced_acc = model_test_balanced_accuracy))
})


res = ijoin(tab, res)
  
for (prob in unique(res$problem)) {
  saveRDS(res[problem == prob, ], file.path("data/runs/mlp/", prob, "surrogate.rds"))
}


# Compute the empirical minimum

library(mlr)
library(smoof)

probs = unique(tab$problem)

parallelMap::parallelStartBatchtools(bt.resources = list(
    walltime = 1 * 3600,
    memory = 2048,
    max.concurrent.jobs = 30),
    logging = FALSE)


res = parallelMap::parallelLapply(probs, function(prob) {

  print(prob)
  
  surr_val = readRDS(file.path("data/runs/mlp_new", prob, "0_objective", "surrogate.rds"))$result[[1]]$model_val_balanced_acc
  lcbench = read.csv2(file.path("data/runs/mlp_new", prob, "0_objective", "lcbench2000.csv"), sep = ",")
 
  ps = makeParamSet(
          # do early stopping instead for the bigger datasets
            makeNumericParam("batch_size", lower = log(16, 2), upper = log(512, 2), trafo = function(x) round(2^x)), 
            makeNumericParam("max_dropout", lower = 0, upper = 1), 
            makeNumericParam("max_units", lower = log(64, 2), upper = log(1024, 2), trafo = function(x) round(2^x)), 
            makeIntegerParam("num_layers", lower = 1, upper = 5),
            makeNumericParam("learning_rate", lower = 0, upper = 0.01),
            makeNumericParam("momentum", lower = 0.1, upper = 1),
            makeNumericParam("weight_decay", lower = 0, upper = 0.1)
        )

  obj = makeSingleObjectiveFunction(name = "mlp.surr.tuning",
    fn = function(x) {

      x = trafoValue(ps, x)
      x = as.data.frame(x)
      y = predict(surr_val[[1]], newdata = x)$data$response

    return(mmce = 1 - y / 100)
    },
    par.set = ps,
    noisy = TRUE,
    has.simple.signature = FALSE,
    minimize = TRUE
  )

  idx.min.lcbench = which.max(lcbench$final_val_balanced_accuracy)
  x.min.lcbench = lcbench[idx.min.lcbench, names(lcbench) %in% getParamIds(ps)]
  y.min.lcbench = 1 - as.numeric(as.character(lcbench[idx.min.lcbench, ]$final_val_balanced_accuracy)) / 100

  x.min.lcbench$batch_size = log(x.min.lcbench$batch_size, 2)
  x.min.lcbench$max_units = log(x.min.lcbench$max_units, 2)

  vals = sampleValues(n = 10^4, ps, trafo = FALSE)
  y = lapply(vals, obj)
  y = unlist(y)

  idx.min = which.min(y)
  x.min = as.data.frame(do.call(cbind, vals[[idx.min]]))
  y.min = y[idx.min]

  res = list(lcbench = list(x.min = x.min.lcbench, y.min = y.min.lcbench), randomforest = list(x.min = x.min, y.min = y.min))

  obj = makeSingleObjectiveFunction(name = "mlp.surr.tuning",
    fn = function(x) {

    ps = makeParamSet(
        # do early stopping instead for the bigger datasets
          makeNumericParam("batch_size", lower = log(16, 2), upper = log(512, 2), trafo = function(x) round(2^x)), 
          makeNumericParam("max_dropout", lower = 0, upper = 1), 
          makeNumericParam("max_units", lower = log(64, 2), upper = log(1024, 2), trafo = function(x) round(2^x)), 
          makeIntegerParam("num_layers", lower = 1, upper = 5),
          makeNumericParam("learning_rate", lower = 0, upper = 0.01),
          makeNumericParam("momentum", lower = 0.1, upper = 1),
          makeNumericParam("weight_decay", lower = 0, upper = 0.1)
      )

      x = trafoValue(ps, x)
      x = as.data.frame(x)
      y = predict(surr_val[[1]], newdata = x)$data$response

    return(mmce = 1 - y / 100)
    },
    par.set = ps,
    noisy = TRUE,
    has.simple.signature = FALSE,
    minimize = TRUE,
    local.opt.params = x.min, 
    local.opt.values = y.min
  )

  saveRDS(res, file.path("data/runs/mlp_new", prob, "0_objective", "surrogate_optima.rds"))
  saveRDS(obj, file.path("data/runs/mlp_new", prob, "0_objective", "obj.rds"))

  return(list(res = res, obj = obj))
})



# Setup script for initially setting up the benchmarks 

library(batchtools)

source("R/benchmarks/LCBench/tree_splitting_config.R")

lapply(packages, require, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "R/benchmarks/LCBench/tree_splitting_config.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_len(length(tasks))) {
  addProblem(
    name = tasks[i], 
    data = file.path(TASK_LOCATION, tasks[i]), 
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



# SUBMIT THE JOBS

resources.serial = list(
  walltime = 3600L * 24L * 2L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

reg = loadRegistry(registry_name, writeable = TRUE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem", "objective", "lambda", "grid.size", "testdata.size", "n.splits"), reg = reg)

# Submit MBO runs 
tosubmit = tab[objective %in% c("SS_sd", "SS_L1") & lambda == 2, ]
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[- which(job.id %in% findRunning()$job.id), ]
tosubmit$chunk = batchtools::chunk(tosubmit$job.id, chunk.size = 3)

submitJobs(tosubmit[2:70, ], resources = resources.serial)


# Store the results that are already ready 

tab = tab[n.splits == 6, ]
lambda = 1

reg = loadRegistry(registry_name, writeable = FALSE)

for (prob in unique(tab$prob)) {

  print(prob)

  # read models since they are needed for evaluation
  rundata = readRDS(file.path("data/runs/mlp_new", prob, "1_1_mlrmbo_runs", paste0("mlrmbo_run_lambda_", lambda, "_30repls.rds")))
  models = lapply(rundata$result, function(res) {
    mods = res$models
    mods[[length(mods)]] # get the last model 
  })

  gtdata = readRDS(file.path("data/runs/mlp_new", prob, "2_2_groundtruth_pdps", paste0("gtpdp_", grid.size, "_", testdata.size, ".rds")))$pdp_ice_groundtruth
  mbo_optima = lapply(rundata$result, function(res) {
    res$opt.path[which.min(res$opt.path$y), ]
  })

  mbo_optima = do.call(rbind, mbo_optima)
  mbo_optima$iter = seq_len(nrow(mbo_optima))

  testdata = readRDS(file.path("data/runs/mlp_new", prob, "2_1_testdata", paste0("testdata_", testdata.size, ".rds")))

  for (obj in unique(tab$objective)) {

    subres = tab[problem == prob & objective == obj, ]

    if (nrow(ijoin(findDone(), subres)) == 1) {

      grid.size = subres$grid.size
      testdata.size = subres$testdata.size

      savepath = file.path("results/tree_splitting_reduced", prob)

      if (!dir.exists(savepath))
        dir.create(savepath)

      savepath = file.path(savepath, paste0("eval_", obj, "_", grid.size, "_", testdata.size, ".rds"))

      if (!file.exists(savepath)) {

        res = reduceResultsDataTable(subres)
        res = ijoin(tab, res)

        if (is.null(res$result[[1]]$eval)) {

          x = res$result[[1]]
          out = evaluate_results(x$reslist, mbo_optima, gtdata)
                  
          res$result[[1]] = list(eval = out, runtime = x$runtime)
        } else {
          res$result[[1]]$reslist = NULL
        }

        saveRDS(res, savepath)    
      }
    }
  }
}




# Get the trees for some of the datasets

problems = c("cnae-9", "higgs", "helena", "Amazon_employee_access", "shuttle")

tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem", "objective", "lambda", "grid.size", "testdata.size", "n.splits"), reg = reg)

tab = tab[n.splits == 6, ]


for (prob in problems) {

  print(prob)

  # read models since they are needed for evaluation

  obj = "SS_L2"

  subres = tab[problem == prob & objective == obj, ]

  if (nrow(ijoin(findDone(), subres)) == 1) {

    grid.size = subres$grid.size
    testdata.size = subres$testdata.size

    savepath = file.path("results", "tmp", prob)

    if (!dir.exists(savepath))
      dir.create(savepath)

    savepath = file.path(savepath, "trees.rds")

    if (!file.exists(savepath)) {

      res = reduceResultsDataTable(subres)
      res = ijoin(tab, res)

      saveRDS(res, savepath)    
    }
  }
}

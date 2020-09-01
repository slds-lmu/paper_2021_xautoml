# Setup script for initially setting up the benchmarks 

library(batchtools)

source("R/LCBench/config.R")

lapply(packages, require, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "R/LCBench/config.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_len(length(tasks))) {
  addProblem(
    name = tasks[i], 
    data = paste(TASK_LOCATION, tasks[i], sep = "/"), 
    reg = reg
  )
}

for (i in 1:length(ALGORITHMS)) {
  addAlgorithm(name = names(ALGORITHMS)[i], reg = reg, fun = ALGORITHMS[[i]]$fun)  
}

addExperiments(
  reg = reg, 
  algo.designs = ades, 
  repls = 30L)


# --- 3. SUBMIT ON LRZ ---

resources.serial = list(
  walltime = 3600L * 24L * 4L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

reg = loadRegistry(registry_name, writeable = TRUE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem", "lambda"))


# start with some problems only
tosubmit = ijoin(tab, findNotDone())
# tosubmit = tosubmit[, .SD[which.min(job.id)], by = list(problem, lambda)]
tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 30)

submitJobs(tosubmit, resources = resources.serial)

probs = unique(tab$problem)

for (prob in probs) {

  toreduce = tab[problem %in% prob, ]
  toreduce = ijoin(toreduce, findDone())

  res = reduceResultsDataTable(toreduce, function(x) {
    
    # for reasons of storage, we just store the model that led to the best observation
    # and the last model 
    # the model that led to the best iteration: 
    # 
    opdf = as.data.frame(x$res$opt.path)
    dob.best = opdf[x$res$best.ind, ]$dob

    models = x$res$models
    
    models = models[c(dob.best, length(models))]
    # x$res$opt.path = opdf
    # x$res$final.opt.state = NULL
    # x$res$opt.path$par.set = NULL
    list(opt.path = opdf, models = models, runtime = x$runtime)
  })

  res = ijoin(tab, res)

  if (!dir.exists(file.path("data/runs/mlp/", prob))) {
    dir.create(file.path("data/runs/mlp/", prob))
  }

  saveRDS(res, file.path("data/runs/mlp", prob, "mlrmbo_30_repls.rds"))
}

bla = readRDS("data/runs/mlp/vehicle/mlrmbo_30_repls.rds")[1, ]$result[[1]]$res
bla = bla$opt.path$par.set

for (i in 1:length(bla)) {
	print(names(bla)[i])
	saveRDS(bla[[i]], paste("data/runs/mlp/vehicle/test/", names(bla)[i], ".rds", sep = ""))
}

for (prob in probs) {

  bla = readRDS(file.path("data/runs/mlp", prob, "mlrmbo_30_repls.rds"))
  bla[1, ]$result[[1]]$res$opt.path$par.set = NULL
  bla[2, ]$result[[1]]$res$opt.path$par.set = NULL
  bla[3, ]$result[[1]]$res$opt.path$par.set = NULL

  saveRDS(bla, file.path("data/runs/mlp", prob, "mlrmbo_30_repls.rds"))

}


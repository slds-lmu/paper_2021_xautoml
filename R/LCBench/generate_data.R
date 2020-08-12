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
  walltime = 3600L * 24L * 1L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

reg = loadRegistry(registry_name, writeable = TRUE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem", "lambda"))


# start with some problems only
tosubmit = ijoin(tab, findNotDone())
tosubmit = tosubmit[, .SD[which.min(job.id)], by = list(problem, lambda)]
tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 10L)

submitJobs(tosubmit, resources = resources.serial)

res = reduceResultsDataTable(findDone())
res = ijoin(tab, res)

for (prob in unique(res$problem)) {
  saveRDS(res[problem == prob, ], file.path("data/runs/mlp/", prob, "surrogate_ranger.rds"))
}


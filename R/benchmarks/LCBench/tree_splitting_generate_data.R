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
  walltime = 3600L * 24L * 4L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

reg = loadRegistry(registry_name, writeable = TRUE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem", "objective"), reg = reg)

# Submit MBO runs 
tosubmit = tab
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = ijoin(tosubmit, findNotStarted())
tosubmit$chunk = batchtools::chunk(tosubmit$job.id, chunk.size = 5)

submitJobs(tosubmit, resources = resources.serial)
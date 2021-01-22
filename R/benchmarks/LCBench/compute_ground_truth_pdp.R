# Setup script for initially setting up the benchmarks 

library(batchtools)

source("R/benchmarks/LCBench/compute_ground_truth_pdp_config.R")

lapply(packages, require, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "R/benchmarks/LCBench/compute_ground_truth_pdp_config.R")

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


resources.serial = list(
  walltime = 3600L * 24L * 4L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

reg = loadRegistry(registry_name, writeable = TRUE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem"))

probs = c("blood-transfusion-service-center", "kc1", "numerai28.6", "phoneme", "sylvine")

# Submit MBO runs 
tosubmit = tab # [problem %in% probs, ]
tosubmit = tosubmit[algorithm == "mlrmbo", ]
tosubmit = ijoin(tosubmit, findNotDone())
# Chunking because each experiment only needs ~ 45 minutes
tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 10)
submitJobs(tosubmit, resources = resources.serial)

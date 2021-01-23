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

# We want to submit the ones that are completed already 
reg_other = loadRegistry("results/mlp_mlrmbo_registry_fixed_initdes", writeable = FALSE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem"))
tab = ijoin(tab, findDone(reg = reg_other))
tab = table(tab$problem) >= 120
probs = names(tab)[tab]

source("R/benchmarks/LCBench/compute_ground_truth_pdp_config.R")

reg = loadRegistry(registry_name, writeable = TRUE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem"), reg = reg)

# Submit MBO runs 
tosubmit = tab[problem %in% probs, ]
tosubmit = ijoin(tosubmit, findNotDone())
submitJobs(4, resources = resources.serial)

# Setup script for initially setting up the benchmarks 

library(batchtools)

source("R/benchmarks/synthetic/config.R")

lapply(packages, require, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "R/benchmarks/synthetic/config.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_len(length(tasks))) {
  for (j in seq_len(length(dimensions))) {
    addProblem(
      name = paste0(tasks[i], dimensions[j], "D"), 
      data = paste("data/runs/synthetic", tasks[i], paste0(dimensions[j], "D"), sep = "/"), 
      reg = reg
    )
  } 
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
# reg$source = "R/benchmarks/LCBench/config.R"
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem", "lambda"))

# Submit MBO runs 
tosubmit = tab # [problem %in% probs, ]
tosubmit = tosubmit[algorithm == "mlrmbo", ]
tosubmit = ijoin(tosubmit, findNotDone())
# Chunking because each experiment only needs ~ 45 minutes
tosubmit$chunk = chunk(tosubmit$job.id, n.chunks = 3)
submitJobs(tosubmit, resources = resources.serial)

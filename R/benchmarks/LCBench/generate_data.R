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

probs = c("blood-transfusion-service-center", "kc1", "numerai28.6", "phoneme", "sylvine")

# Submit MBO runs 
tosubmit = tab[problem %in% probs, ]
tosubmit = tosubmit[algorithm == "mlrmbo", ]
tosubmit = ijoin(tosubmit, findNotDone())
# Chunking because each experiment only needs ~ 45 minutes
tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 30)
submitJobs(tosubmit, resources = resources.serial)

# Submit randomsearch runs 
tosubmit = tab[problem %in% probs, ]
tosubmit = tosubmit[algorithm == "randomsearch", ]
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[ ,.SD[which.min(job.id)], by = problem]

# Chunking because each experiment only needs ~ 10 minutes
tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 10)
submitJobs(tosubmit, resources = resources.serial)


# --- 4. Reduce problems 

# We do this problem-wise 
probs = c("blood-transfusion-service-center", "kc1", "numerai28.6", "phoneme", "sylvine")

for (prob in probs) {

  toreduce = tab[problem %in% prob, ]
  toreduce = ijoin(toreduce, findDone())

  res = reduceResultsDataTable(toreduce)
  res = ijoin(tab, res)

  if (!dir.exists(file.path("data/runs/mlp_new/", prob))) {
    dir.create(file.path("data/runs/mlp_new/", prob))
  }

  saveRDS(res, file.path("data/runs/mlp_new", prob, "mlrmbo30_vs_randomLHS.rds"))
}


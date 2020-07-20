# Setup script for initially setting up the benchmarks 

library(batchtools)

source("R/xgboost_config.R")

lapply(packages, library, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "R/xgboost_config.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_len(length(tasks))) {
  addProblem(
    name = tasks[i], 
    data = paste(TASK_LOCATION, tasks[i], sep = "/"), 
    fun = readProblem,
    reg = reg
  )
}

for (i in 1:length(ALGORITHMS)) {
  addAlgorithm(name = names(ALGORITHMS)[i], reg = reg, fun = ALGORITHMS[[i]]$fun)  
}

addExperiments(
  reg = reg, 
  prob.designs = pdes,
  repls = 30L)


# --- 3. SUBMIT ON LRZ ---

resources.serial = list(
	walltime = 3600L * 96L, memory = 1024L * 2L,
	clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

reg = loadRegistry(registry_name, writeable = TRUE)
tab = summarizeExperiments(
	by = c("job.id", "algorithm", "lrn", "problem"))


# filter for problems we are interested in NOW 
probs = c("blood-transfusion-service-center", "kc1")
tosubmit = tab[algorithm == "mlrmbo", ]
tosubmit = tosubmit[problem %in% probs, ]

tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 30L)

submitJobs(tosubmit, resources = resources.serial)

tosubmit = tab[algorithm == "randomsearch", ]
tosubmit = tosubmit[problem %in% probs, ]
tosubmit = tosubmit[ ,.SD[which.min(job.id)], by = problem]

submitJobs(tosubmit, resources = resources.serial)

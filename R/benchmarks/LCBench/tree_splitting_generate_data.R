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
  by = c("job.id", "algorithm", "problem", "objective", "grid.size", "testdata.size", "n.splits"), reg = reg)

# Submit MBO runs 
tosubmit = tab[n.splits == 6, ]
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[- which(job.id %in% findRunning()$job.id), ]
tosubmit$chunk = batchtools::chunk(tosubmit$job.id, chunk.size = 3)

submitJobs(tosubmit, resources = resources.serial)


# Store the results that are already ready 

reg = loadRegistry(registry_name, writeable = FALSE)

for (prob in unique(tab$prob)) {

  for (obj in unique(tab$objective)) {

    subres = tab[problem == prob & objective == obj, ]

    if (nrow(ijoin(findDone(), subres)) == 1) {

      res = reduceResultsDataTable(subres, function(x) x$eval)
      res = ijoin(tab, res)
      
      grid.size = res$grid.size
      testdata.size = res$testdata.size

      savepath = file.path("data/runs/mlp_new/", prob, "2_3_effects_and_trees", paste0("eval_", obj, "_", grid.size, "_", testdata.size, ".rds"))

      if (!file.exists(savepath))
        saveRDS(res, savepath)    
    }
  }
}


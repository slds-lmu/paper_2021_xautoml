# Setup script for initially setting up the benchmarks 

library(batchtools)

source("benchmarks/mlp/1_empirical_performance_model/config.R")

lapply(packages, require, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "benchmarks/mlp/1_empirical_performance_model/config.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_len(length(tasks))) {
  addProblem(
    name = tasks[i], 
    data = paste(TASK_LOCATION, tasks[i], "lcbench2000.csv", sep = "/"), 
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


# --- 3. SUBMIT ON LRZ ---
resources.serial = list(
  walltime = 3600L * 24L * 4L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)


reg = loadRegistry(registry_name, writeable = TRUE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem"))

tosubmit = tab
tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 5L)

submitJobs(tosubmit[chunk == 1, ], resources = resources.serial)

res = reduceResultsDataTable(findDone(), function(x) {
  model_val_balanced_accuracy = x$opdf_val_balanced_accuracy[final.model.avail == TRUE, ]$model
  model_test_balanced_accuracy = x$opdf_test_balanced_accuracy[final.model.avail == TRUE, ]$model
  return(list(model_val_balanced_acc = model_val_balanced_accuracy, model_test_balanced_acc = model_test_balanced_accuracy))
})


res = ijoin(tab, res)
  
for (prob in unique(res$problem)) {
  saveRDS(res[problem == prob, ], file.path("data/runs/mlp/", prob, "surrogate.rds"))
}


# Compute the empirical minimum


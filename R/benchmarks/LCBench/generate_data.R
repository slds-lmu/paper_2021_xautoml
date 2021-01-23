# Setup script for initially setting up the benchmarks 

library(batchtools)

source("R/benchmarks/LCBench/config.R")

lapply(packages, require, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "R/benchmarks/LCBench/config.R")

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
reg$source = "R/benchmarks/LCBench/config.R"
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem", "lambda"))

probs = c("blood-transfusion-service-center", "kc1", "numerai28.6", "phoneme", "sylvine")

# Submit MBO runs 
tosubmit = tab # [problem %in% probs, ]
tosubmit = tosubmit[algorithm == "mlrmbo", ]
tosubmit = ijoin(tosubmit, findNotDone())
# Chunking because each experiment only needs ~ 45 minutes
tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 10)
submitJobs(tosubmit, resources = resources.serial)

# Submit randomsearch runs 
tosubmit = tab[problem %in% probs, ]
tosubmit = tosubmit[algorithm == "randomsearch", ]
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[ ,.SD[which.min(job.id)], by = problem]

# Chunking because each experiment only needs ~ 10 minutes
tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 10)
submitJobs(tosubmit, resources = resources.serial)


# --- 4. Reduce problems into the right data structure

basepath = "data/runs/mlp_new"
datasets = list.dirs(basepath, full.names = FALSE, recursive = FALSE)
datasets = datasets[2:length(datasets)]

for (d in probs_all) {
  # dir.create(file.path(basepath, d))
  # dir.create(file.path(basepath, d, "0_objective"))
  # dir.create(file.path(basepath, d, "1_1_mlrmbo_runs"))
  # dir.create(file.path(basepath, d, "1_2_randomsearch"))
  # dir.create(file.path(basepath, d, "2_1_testdata"))
  # dir.create(file.path(basepath, d, "2_2_groundtruth_pdps"))
  # dir.create(file.path(basepath, d, "2_3_effects_and_trees"))

  # Copy over the surrogate model into the new location
  file.copy(from = file.path("data", "runs", "mlp", d, "lcbench2000.csv"), 
    to = file.path(basepath, d, "0_objective", "lcbench2000.csv"))



}




# We do this problem-wise 
probs = c("blood-transfusion-service-center", "kc1", "numerai28.6", "phoneme", "sylvine")

probs = unique(tab$problem)
# probs = setdiff(probs_all, probs)

for (prob in probs) {

  toreduce = tab[problem %in% prob & algorithm == "mlrmbo", ]
  toreduce = ijoin(toreduce, findDone())
  res = reduceResultsDataTable(toreduce)
  res = ijoin(tab, res)

  if (!dir.exists(file.path("data/runs/mlp_new/", prob))) {
    dir.create(file.path("data/runs/mlp_new/", prob))
  }

  for (lamb in unique(res[algorithm == "mlrmbo", ]$lambda)) {
    res_tostore = res[algorithm == "mlrmbo" & lambda == lamb, ]
   
    saveRDS(res_tostore, file.path("data/runs/mlp_new", prob, "1_1_mlrmbo_runs", paste0("mlrmbo_run_lambda_", lamb, "_30repls.rds")))
  }

  # Make randomsearch smaller 
  # toreduce = tab[problem %in% prob & algorithm == "randomsearch", ]
  # toreduce = ijoin(toreduce, findDone())
  # res = reduceResultsDataTable(toreduce, function(x) {
  #   x$models_on_randomLHS[[2]] = NULL
  #   x$perf_on_test_data_randomLHS = x$perf_on_test_data_randomLHS[1, ]
  #   x
  # })
  # res = ijoin(tab, res)
  # saveRDS(res, file.path("data/runs/mlp_new", prob, paste0("randomsearch_run_30repls.rds")))

}


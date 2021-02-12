# Setup script for initially setting up the benchmarks 

library(batchtools)

source("benchmarks/mlp/3_gt_pdp/config.R")
source("benchmarks/helper_experiments.R")

lapply(packages, require, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "benchmarks/mlp/3_gt_pdp/config.R")

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



# # SUBMIT THE JOBS

# resources.serial = list(
#   walltime = 3600L * 24L * 4L, memory = 1024L * 2L,
#   clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
# )

# # We want to submit the ones that are completed already 
# reg_other = loadRegistry("results/mlp_mlrmbo_registry_fixed_initdes", writeable = FALSE)
# tab = summarizeExperiments(
#   by = c("job.id", "algorithm", "problem"))
# tab = ijoin(tab, findDone(reg = reg_other))
# tab = table(tab$problem) >= 120
# probs = names(tab)[tab]

# source("R/benchmarks/LCBench/compute_ground_truth_pdp_config.R")

# reg = loadRegistry(registry_name, writeable = TRUE)
# tab = summarizeExperiments(
#   by = c("job.id", "algorithm", "problem"), reg = reg)

# # Submit MBO runs 
# tosubmit = tab[problem %in% probs, ]
# tosubmit = ijoin(tosubmit, findNotDone())
# tosubmit$chunk = batchtools::chunk(tosubmit$job.id, chunk.size = 10)

# submitJobs(tosubmit, resources = resources.serial)



# # STORE THE JOBS AGAIN 

# source("R/benchmarks/LCBench/compute_ground_truth_pdp_config.R")

# reg = loadRegistry(registry_name, writeable = FALSE)
# tab = summarizeExperiments(
#   by = c("job.id", "algorithm", "problem"), reg = reg)

# probs = unique(tab$problem)

# for (prob in probs) {

#   toreduce = tab[problem %in% prob, ]
#   toreduce = ijoin(toreduce, findDone())
#   res = reduceResultsList(toreduce)[[1]]

#   path = file.path("data/runs/mlp_new", prob)

#   # Store the objective function 
#   saveRDS(res["surr_optima"], file.path(path, "0_objective", "surrogate_optima.rds"))
#   saveRDS(res["obj"], file.path(path, "0_objective", "obj.rds"))

#   # extract the grid.size and the testdata size
#   gtpdp = res["pdp_ice_groundtruth"]
#   exm = gtpdp$pdp_ice_groundtruth$batch_size[[1]]
#   testdata.size = max(exm$.id, na.rm = TRUE)
#   grid.size = sum(exm$.id == 1, na.rm = TRUE)

#   saveRDS(res["pdp_ice_groundtruth"], file.path(path, "2_2_groundtruth_pdps", paste0("gtpdp_", grid.size, "_", testdata.size, ".rds")))

# }


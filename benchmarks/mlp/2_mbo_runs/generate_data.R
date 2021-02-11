# Setup script for initially setting up the benchmarks 

library(batchtools)

source("benchmarks/mlp/2_mbo_runs/config.R")
source("benchmarks/helper_experiments.R")
source("benchmarks/helper_evaluation.R")

lapply(packages, require, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "benchmarks/mlp/2_mbo_runs/config.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_len(length(tasks))) {
  addProblem(
    name = tasks[i], 
    data = file.path("data", "runs", "mlp2", tasks[i], "0_objective"),
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














# # --- 4. Reduce problems into the right data structure

# basepath = "data/runs/mlp_new"
# datasets = list.dirs(basepath, full.names = FALSE, recursive = FALSE)
# datasets = datasets[2:length(datasets)]

# for (d in probs_all) {
#   # dir.create(file.path(basepath, d))
#   # dir.create(file.path(basepath, d, "0_objective"))
#   # dir.create(file.path(basepath, d, "1_1_mlrmbo_runs"))
#   # dir.create(file.path(basepath, d, "1_2_randomsearch"))
#   # dir.create(file.path(basepath, d, "2_1_testdata"))
#   # dir.create(file.path(basepath, d, "2_2_groundtruth_pdps"))
#   # dir.create(file.path(basepath, d, "2_3_effects_and_trees"))

#   # Copy over the surrogate model into the new location
#   file.copy(from = file.path("data", "runs", "mlp", d, "lcbench2000.csv"), 
#     to = file.path(basepath, d, "0_objective", "lcbench2000.csv"))



# }




# # We do this problem-wise 
# probs = c("blood-transfusion-service-center", "kc1", "numerai28.6", "phoneme", "sylvine")

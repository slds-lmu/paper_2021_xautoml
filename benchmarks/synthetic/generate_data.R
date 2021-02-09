# Setup script for initially setting up the benchmarks 

library(batchtools)

source("benchmarks/synthetic/config.R")

lapply(packages, require, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "benchmarks/synthetic/config.R")


# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_len(length(tasks))) {
  
  path = file.path("data/runs/synthetic/", tasks[i])

  if (!dir.exists(path))
    dir.create(file.path(path), recursive = TRUE)

  for (j in seq_len(length(dimensions))) {

    subpath = file.path(path, paste0(dimensions[j], "D"))

    if (!dir.exists(subpath))
      dir.create(file.path(subpath))

    if (!file.exists(file.path(subpath, "objective.rds"))) {
      
      obj = makeSingleObjectiveFunction(name = paste0("StyblinskiTang", dimensions[j], "D"), fn = function(x) {
              1 / 2 * sum(x^4 - 16 * x^2 + 5 * x)
          }, 
          par.set = makeParamSet(makeNumericVectorParam(id = "x", len = dimensions[j], lower = - 5, upper = 5)), 
          global.opt.params = rep(-2.9035, dimensions[j])
      )

      saveRDS(obj, file.path(subpath, "objective.rds"))
    }

    addProblem(
      name = paste0(tasks[i], dimensions[j], "D"), 
      data = subpath, 
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


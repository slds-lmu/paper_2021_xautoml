# Setup script for initially setting up the benchmarks 

library(batchtools)

source("R/benchmarks/synthetic/config.R")

lapply(packages, require, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "R/benchmarks/synthetic/config.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_len(length(tasks))) {
  
  path = file.path("data/runs/synthetic/", tasks[i])

  if (!dir.exists(path))
    dir.create(file.path(path))

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


# --- 3. SUBMIT ON LRZ ---

resources.serial = list(
  walltime = 3600L * 24L * 2L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

reg = loadRegistry(registry_name, writeable = TRUE)
# reg$source = "R/benchmarks/LCBench/config.R"
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem", "lambda", "objective", "n.splits"))

# Submit MBO runs 
tosubmit = tab[n.splits == 5, ] # [problem %in% probs, ]
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit$chunk = batchtools::chunk(tosubmit$job.id, chunk.size = 200)
submitJobs(tosubmit, resources = resources.serial)


# Reduce the ones that are already through

reg = loadRegistry(registry_name, writeable = FALSE)

toreduce = ijoin(tab, findDone())

for (obj in unique(toreduce$objective)) {

  for (prob in unique(toreduce$problem)) {

    res = reduceResultsDataTable(toreduce[problem == prob & objective == obj, ], function(x) x$eval)
    res = ijoin(tab, res)
    
    savepath = file.path("data", "runs", "synthetic", prob)

    if (!dir.exists(savepath))
      dir.create(savepath)
    
    saveRDS(res, file.path(savepath, paste0("eval_", obj, ".rds")))

  }

}
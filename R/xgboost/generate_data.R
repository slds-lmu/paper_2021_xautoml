# Setup script for initially setting up the benchmarks 

library(batchtools)

source("R/xgboost_config.R")

lapply(packages, require, character.only = TRUE)


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
  algo.designs = ades, 
  repls = 30L)


# --- 3. SUBMIT ON LRZ ---

resources.serial = list(
	walltime = 3600L * 24L * 4L, memory = 1024L * 2L,
	clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)


reg = loadRegistry(registry_name, writeable = TRUE)
tab = summarizeExperiments(
	by = c("job.id", "algorithm", "lrn", "problem", "lambda"))


# filter for problems we are interested in NOW 
# probs = list.dirs("data/raw", full.names = FALSE)
# probs = probs[- which(probs %in% c("blood-transfusion-service-center", "kc1", ""))]

probs = c("blood-transfusion-service-center", "kc1", "numerai28.6", "phoneme", "sylvine")

tosubmit = tab[algorithm == "mlrmbo", ]
tosubmit = tosubmit[problem %in% probs, ]
# tosubmit = tosubmit[, .SD[which.min(job.id)], by = c("problem")]
tosubmit = ijoin(tosubmit, findExpired())

tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 10L)

submitJobs(tosubmit, resources = resources.serial)

tosubmit = tab[algorithm == "randomsearch", ]
tosubmit = tosubmit[problem %in% probs, ]
tosubmit = tosubmit[ ,.SD[which.min(job.id)], by = problem]
tosubmit = ijoin(tosubmit, findNotDone())

submitJobs(tosubmit, resources = resources.serial)


for (prob in probs) {

  toreduce = tab[problem %in% prob, ]
  toreduce = ijoin(toreduce, findDone())

  res = reduceResultsDataTable(toreduce, function(x) {
    
    # for reasons of storage, we just store the model that led to the best observation
    # and the last model 
    # the model that led to the best iteration: 
    # 
    opdf = as.data.frame(x$res$opt.path)
    dob.best = opdf[x$res$best.ind, ]$dob

    models = x$res$models
    
    x$res$models = models[c(dob.best, length(models))]
    x$res$final.opt.state = NULL
    x
  })

  res = ijoin(tab, res)

  if (!dir.exists(file.path("data/runs/xgboost/", prob))) {
    dir.create(file.path("data/runs/xgboost/", prob))
  }

  saveRDS(res, file.path("data/runs/xgboost", prob, "mlrmbo_30_repls.rds"))
}

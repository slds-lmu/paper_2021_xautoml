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
tosubmit$chunk = batchtools::chunk(tosubmit$job.id, chunk.size = 50)
submitJobs(tosubmit[chunk != 1, ], resources = resources.serial)


# Reduce the ones that are already through

library(kernlab)

cross.kernel = function(d1, d2 = NULL, sigma) {
  checkmate::assert_data_frame(d1, any.missing = FALSE)
  checkmate::assert_data_frame(d2, null.ok = TRUE, ncols = ncol(d1), any.missing = FALSE)
  checkmate::assert_number(sigma, lower = 0)
  mradial = kernlab::rbfdot(sigma = sigma)
  mm = kernlab::kernelMatrix(mradial, as.matrix(d1), as.matrix(d2))
  mean(mm)
}


get_median_dist = function(d){
  checkmate::assert_data_frame(d)
  dists = dist(d, diag = FALSE, upper = FALSE, method = "euclidean")
  median(dists)
}

mmd2 = function(d1, d2, sigma = NULL) {
  checkmate::assert_data_frame(d1, any.missing = FALSE)
  checkmate::assert_data_frame(d2, null.ok = TRUE, ncols = ncol(d1), any.missing = FALSE)
  checkmate::assert_number(sigma, lower = 0, null.ok = TRUE)
  d1 = data.frame(model.matrix(~ . -1, data = d1))
  d2 = data.frame(model.matrix(~ . -1, data = d2))
  if(is.null(sigma)) {
    sigma = get_median_dist(rbind(d1, d2))
    # Confusingly, the sigma in rbfdot is acutally the gamma param
    sigma = 1/(2 * sigma^2)
  }
  cross.kernel(d1, d1, sigma = sigma) - 2 * cross.kernel(d1, d2, sigma = sigma) + cross.kernel(d2, d2, sigma = sigma)
}


reg = loadRegistry(registry_name, writeable = FALSE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem", "lambda", "objective", "n.splits"))

toreduce = ijoin(tab, findDone())

toreduce = toreduce[objective %in% c("SS_L2", "SS_sd"), ]
toreduce = toreduce[n.splits == 5, ]

for (obj in unique(toreduce$objective)) {

  for (prob in c("StyblinskiTang2D", "StyblinskiTang3D", "StyblinskiTang5D", "StyblinskiTang8D")) {

    savepath = file.path("data", "runs", "synthetic", prob)

    if (!dir.exists(savepath))
      dir.create(savepath)

    tored = toreduce[problem == prob & objective == obj, ]

    res = reduceResultsDataTable(tored, function(x) {
      opt.path = as.data.frame(x$mbo_run$opt.path)
      ps = x$mbo_run$opt.path$par.set
      ids = getParamIds(ps, repeated = TRUE, with.nr = TRUE)
      df1 = as.data.frame(x$mbo_run$opt.path)[, ids]
      df2 = as.data.frame(generateRandomDesign(n = nrow(df1), ps))
      x$mmd2_full = mmd2(df1, df2)

      df1 = as.data.frame(x$mbo_run$opt.path)[1:50, ids]
      df2 = as.data.frame(generateRandomDesign(n = nrow(df1), ps))
      x$mmd2_50 = mmd2(df1, df2)

      x$opt.path = opt.path
      models = x$mbo_run$models 

      x$models = x$mbo_run$models[c(43, length(models))]
      x$mbo_run = NULL

      return(x)
    })

    res = ijoin(tab, res)    
    
    saveRDS(res, file.path(savepath, paste0("eval_", obj, ".rds")))
  }
}
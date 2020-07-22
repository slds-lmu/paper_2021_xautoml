readProblem = function(data, job, lrn, ...) {

  task = readRDS(file.path(data, "task.rds"))

  learner = PROBLEMS[[lrn]]$learner
  ps = PROBLEMS[[lrn]]$ps
  obj = PROBLEMS[[lrn]]$obj

  list(
    task = task, 
    learner = learner,
    ps = ps 
  )
}


safeSetupRegistry = function(registry_name, overwrite, packages, def) {
  if (file.exists(registry_name)) {
    if (overwrite) {
      unlink(registry_name, recursive = TRUE)
      reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
        packages = packages, source = def)
    } else {
      reg = loadRegistry(registry_name, writeable = TRUE)
    }
  } else {
      reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
        packages = packages, source = def)
  }
  return(reg)
}


getGroundTruth = function(res, prob) {
  newdata = res[problem == prob & algorithm == "randomsearch", ]$result[[1]]
  newdata = newdata$res$opt.path$env$path
  return(newdata)
}

getPredictions = function(res, prob, newdata) {
  preds = lapply(models, function(x) {
    preds = predict(x, newdata = newdata)
    preds
  })
  return(preds)
}

getModels = function(res, prob) {
    ressub = res[problem == prob & algorithm == "mlrmbo", ]

    models = lapply(seq_len(nrow(ressub)), function(x) {
    modls = ressub[x, ]$result[[1]]$res$models
    if (length(modls) > 0)
      modls[[length(modls)]]
    else 
      NULL
  })
    return(models)
}

getTrainData = function(res, prob) {
    ressub = res[problem == prob & algorithm == "mlrmbo", ]

    dfs = lapply(seq_len(nrow(ressub)), function(x) {
      ressub$result[[1]]$res$opt.path$env$path
    })
    return(dfs)
}



getPerformance = function(res, prob) {
  ressub = res[problem == prob & algorithm == "mlrmbo", ]

  models = lapply(seq_len(nrow(ressub)), function(x) {
    modls = ressub[x, ]$result[[1]]$res$models
    if (length(modls) > 0)
      modls[[length(modls)]]
    else 
      NULL
  })

  newdata = getGroundTruth(res, prob)

  preds = getPredictions(res, prob, newdata)
  perfs = lapply(preds, performance)

  data.frame(job.id = ressub$job.id, mse = unlist(perfs))
}


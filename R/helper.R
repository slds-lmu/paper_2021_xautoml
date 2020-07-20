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
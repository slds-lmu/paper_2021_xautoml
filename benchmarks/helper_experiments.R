safeSetupRegistry = function(registry_name, overwrite, packages, def) {

  if (!dir.exists(dirname(registry_name)))
    dir.create(dirname(registry_name), recursive = TRUE)

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


compute_ground_truth_pdps = function(obj, path, dataset, features, testdata, testdata.size, grid.size, optima) {
  
  savepath = file.path(path, dataset, paste0("gt_pdp_gridsize_", grid.size, "_testdatasize_", testdata.size, ".rds"))  

  if (file.exists(savepath)) {
    gtdata = readRDS(savepath)
  } else {
    gtdata = lapply(features, function(feature) {
      marginal_effect_mlp(obj = obj, feature = feature, data = testdata, all.features = features, grid.size = grid.size, optima = optima, method = "pdp+ice")
    })

    names(gtdata) = features
    saveRDS(gtdata, savepath)
  }

  return(gtdata)
}


compute_trees = function(n.split, models, features, testdata, grid.size, objectives) {
  
  # Compute trees for a list of models and a list of objectives on a fixed dataset.

  reslist = list()

  for (i in seq_along(models)) {

    print(paste("Model number", i))

    model = models[[i]]

    results_for_features = lapply(features, function(feature) {

      # Compute all ice curves
      mymodel = makeS3Obj("mymodel", fun = function() return(model))
      
      predict.mymodel = function(object, newdata) {
        pred = predict(object$fun(), newdata = newdata)
        pp = getPredictionSE(pred)

        return(pp)
      }

      predictor = Predictor$new(model = mymodel, data = as.data.frame(testdata)[, model$features], predict.function = predict.mymodel)
      effect_sd = FeatureEffect$new(predictor = predictor, feature = feature, method = "pdp+ice", grid.size = grid.size)
      
      predictor = Predictor$new(model = model, data = as.data.frame(testdata)[, model$features])
      effect_mean = FeatureEffect$new(predictor = predictor, feature = feature, method = "pdp+ice", grid.size = grid.size)
            
      effect_sd_d = setDT(effect_sd$results)
      names(effect_sd_d)[2] = "sd"
      effect_mean_d = setDT(effect_mean$results)
      names(effect_mean_d)[2] = "mean"

      effects_merged = batchtools::ijoin(effect_sd_d, effect_mean_d, by = c(feature, ".type", ".id"))

      sf = c(feature, "mean", "sd", ".id")
      res.pdp = effects_merged[.type == "pdp", ..sf]
      res.ice = effects_merged[.type == "ice", ..sf]

      trees = lapply(objectives, function(objective) {
        compute_tree(effect = effect_sd, testdata = testdata, objective = objective, n.split = n.split) 
      })

      list(res.pdp = res.pdp, res.ice = res.ice, trees = trees)
    })

    names(results_for_features) = features

    reslist[[i]] = results_for_features
  }

  reslist
}
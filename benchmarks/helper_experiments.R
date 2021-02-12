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


perform_random_search = function(df, search_space_ids, ps_surrogate, objective, max_evals, resampling) {

  task_data = df[, c(search_space_ids, objective)]

  # I think this is an error in the data 
  task_data$num_layers = ifelse(task_data$num_layers == "True", 5, task_data$num_layers)

  task_data = sapply(task_data, function(x) as.numeric(as.character(x)))
  task_data = as.data.table(task_data)

  task_data$num_layers = as.integer(task_data$num_layers)
  task_data$batch_size = as.integer(task_data$batch_size)
  task_data$max_units = as.integer(task_data$max_units)
  task_data$num_layers = as.integer(task_data$num_layers)

  task = makeRegrTask(data = task_data, target = objective)

  obj_fun = makeSingleObjectiveFunction(name = "ranger.surrogate", fn = function(x) {   
      if (!x$do.mtry) 
        x$mtry = getTaskNFeats(task)

      x$do.mtry = NULL
      x$splitrule = "extratrees"

    lrn = makeLearner("regr.ranger", par.vals = x)
      resample(lrn, task, resampling, show.info = FALSE)$aggr
    },
    par.set = ps_surrogate,
    noisy = TRUE,
    has.simple.signature = FALSE,
    minimize = TRUE
  )

  des = generateRandomDesign(n = max_evals, par.set = ps_surrogate)
  # des = rbind(des, data.frame(num.trees = log(10, 2), do.mtry = FALSE, min.node.size = 1, num.random.splits = getTaskNFeats(task_val)))

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, max.evals = max_evals - 1)

  res = mbo(obj_fun, design = des, control = ctrl, show.info = TRUE)

  # compute surrogate on the whole data with the best configuration
  opdf = as.data.table(as.data.frame(res$opt.path))
  opdf$final.model.avail = FALSE
  opdf$model = list()

  idx = which.min(opdf$y)
  opdf[idx, ]$final.model.avail = TRUE

  config = des[idx, ]
  config = as.list(config)
  config.trafo = trafoValue(ps_surrogate, config)

  if (!config$do.mtry) 
    config$mtry = getTaskNFeats(task)

  config.trafo$do.mtry = NULL

  lrn = makeLearner("regr.ranger", par.vals = list(splitrule = "extratrees"))
  lrn.tuned = setHyperPars2(lrn, config.trafo)

  mod.tuned = train(lrn.tuned, task)

  opdf[idx, ]$model[[1]] = mod.tuned

  return(opdf)
}



get_models <- function(data){

  models = lapply(names(data), function(dataset) {
    reslist = data[[dataset]]$result
    models = lapply(reslist, function(res) {
      models = res$models
      models[[length(models)]]
    })
    models
  })

  names(models) = names(data)

  return(models)
}

get_optima <- function(data){

  optima = lapply(names(data), function(dataset) {
    reslist = data[[dataset]]$result
    optima = lapply(reslist, function(res) {
      res$opt.path[which.min(res$opt.path$y), ]
    })
    optima
  })

  names(optima) = names(data)

  return(optima)
}



get_objective <- function(path, folder_mlp){
  data.list = lapply(folder_mlp, function(folder) {
    surrogate_model = readRDS(file.path(path, folder, "0_objective", "surrogate.rds"))
    obj = readRDS(file.path(path, folder, "0_objective", "obj.rds"))
    list(
      obj = obj,
      surrogate_model = surrogate_model$result[[1]]$model_val_balanced_acc
    )
  })

  names(data.list) = folder_mlp
  
  return(data.list)
}

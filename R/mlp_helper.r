

get_data <- function(path, folder_mlp, lambda){
  data.list = lapply(folder_mlp, function(folder) {
    data = readRDS(file.path(path, folder, paste0("mlrmbo_run_lambda_", lambda, "_30repls.rds")))
    surrogate_model = readRDS(file.path(path, folder, "surrogate.rds"))

    data
  })

  names(data.list) = folder_mlp
  
  return(data.list)
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
    surrogate_model = readRDS(file.path(path, folder, "surrogate.rds"))
    obj = readRDS(file.path(path, folder, "obj.rds"))
    list(
      obj = obj,
      surrogate_model = surrogate_model$result[[1]]$model_val_balanced_acc
    )
  })

  names(data.list) = folder_mlp
  
  return(data.list)
}


#test = get_data(path, folder_mlp)


get_objects_mlp <- function(data, lambda, iteration.train, iteration.test){
  objectname = paste0("mbo_lambda", lambda)
  object.train = data[[objectname]][[iteration.train]]
  object.test = data[[objectname]][[iteration.test]]
  model = object.train$models[[length(object.train$models)]]
  data.train = object.train$opt.path[, 1:which(colnames(object.train$opt.path) == "y")]
  data.test = object.test$opt.path[, 1:which(colnames(object.test$opt.path) == "y")]
  return(list("model" = model, "data.train" = data.train, "data.test" = data.test 
  ))
}


#test = get_objects_mlp(data = data_adult, lambda = 0.5, 1)


get_models_data <- function(data, lambda){
  model.list.mbo = vector("list", length(lambda))
  model.list.rlhs = list()
  names(model.list.mbo) = lambda
  n.rep = length(data[[1]])
  for(j in 1:length(lambda)){
    for(i in 1:n.rep){
      if (i < n.rep) object = get_objects_mlp(data, lambda[j], i, i+1)
      else object = get_objects_mlp(data, lambda[j], i, 1)
      data.train.sub = object$data.train
      data.train.sub$extrapol = lambda[j]
      data.train.sub$iteration = i
      
      data.test.sub = object$data.test
      data.test.sub$extrapol = lambda[j]
      data.test.sub$iteration = i
      
      # rlhs data
      if(j ==1){
        rlhs.train = data[["rlhs"]][[i]]
        model.list.rlhs[[i]] = rlhs.train$models[[1]]
        rlhs.train = rlhs.train$train_data_randomLHS[[1]][, 1:which(colnames(rlhs.train$train_data_randomLHS[[1]]) == "y")]
        rlhs.train$iteration = i
      }
     
      
      if(j == 1 & i == 1){
        data.train.mbo = data.train.sub
        data.test.mbo = data.test.sub
        #rlhs
        data.train.rlhs = rlhs.train
      }
      else {
        data.train.mbo = rbind(data.train.mbo, data.train.sub)
        data.test.mbo = rbind(data.test.mbo, data.test.sub)
        # rlhs
        if(j == 1) data.train.rlhs = rbind(data.train.rlhs, rlhs.train)
      }
      model.list.mbo[[j]][[i]] = object$model
      
    }
    
  }
  return(list("model.list.mbo" = model.list.mbo, "data.train.mbo" = data.train.mbo, 
              "data.test.mbo" = data.test.mbo, "model.list.rlhs" = model.list.rlhs, 
              "data.train.rlhs" = data.train.rlhs))
}



compute_ground_truth_pdps = function(obj, path, dataset, features, testdata, testdata.size, grid.size) {
  
  savepath = file.path(path, dataset, paste0("gt_pdp_gridsize_", grid.size, "_testdatasize_", testdata.size, ".rds"))  

  if (file.exists(savepath)) {
    gtdata = readRDS(savepath)
  } else {
    gtdata = lapply(features, function(feature) {
      marginal_effect_mlp(obj = obj, feature = feature, data = testdata, all.features = features, grid.size = grid.size, method = "pdp+ice")
    })

    names(gtdata) = features
    saveRDS(gtdata, savepath)
  }

  return(gtdata)
}



get_data <- function(path, folder_mlp){
  for (i in folder_mlp) {
    data = readRDS(paste0(path,i, "/mlrmbo30_vs_randomLHS.rds"))
    data.list = list(
      "mbo_lambda0.5" = data$result[1:30],
      "mbo_lambda1" = data$result[31:60],
      "mbo_lambda2" = data$result[61:90],
      "mbo_lambda10" = data$result[91:120],
      "rlhs" = data$result[121:150],
      # "rlhs_train_data" = data$result[[121]]$train_data_randomLHS[[1]],
      "rlhs_test_data" = data$result[[121]]$test_data_randomLHS, 
      # "rlhs_model" = data$result[[121]]$models_on_randomLHS[[1]],
      "surrogate_data" = data$result[[121]]$lcbench_data,
      "surrogate_model" = data$result[[121]]$surrogate_on_lcbench_GT
    )
    assign(paste0("data_", tolower(i)), data.list)
    
  }
  return(mget(paste0("data_", tolower(folder_mlp))))
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




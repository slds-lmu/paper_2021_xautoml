# mlrMBO
library("mlrMBO")
library("mlr")



get_data <- function(path, folder){
  for (i in folder) {
    data.list = list(
      "mbo_lambda0.10" = readRDS(paste0(path,i, "/mlrmbo_run_lambda_0.1.rds")),
      "mbo_lambda1.00" = readRDS(paste0(path,i, "/mlrmbo_run_lambda_1.rds")),
      "mbo_lambda2.00" = readRDS(paste0(path,i, "/mlrmbo_run_lambda_2.rds")),
      "mbo_lambda10.0" = readRDS(paste0(path,i, "/mlrmbo_run_lambda_10.rds"))
    )
    assign(paste0("data_", tolower(i)), data.list)
    
  }
  return(mget(paste0("data_", tolower(folder))))
}


#test = get_data(path, folder_mlp)

get_models <- function(data){
  data = data[[1]]
  model.list.mbo = list()
  for(name in names(data)){
    model.list.mbo[[substr(name, nchar(name)-3,nchar(name))]] = data[[name]]$final.opt.state$models$models[[1]]
  }
  return(model.list.mbo)
}

get_objective <- function(dimension){
  obj = makeSingleObjectiveFunction(name = paste0(dimension, "D"), fn = function(x) {
    1 / 2 * sum(x^4 - 16 * x^2 + 5 * x)
  }, 
  par.set = makeParamSet(makeNumericVectorParam(id = "x", len = dimension, lower = - 5, upper = 5))
  )
  return(obj)
}

get_testset <- function(n, objective){
  testset = generateRandomDesign(n, getParamSet(objective))
  return(testset)
}


get_all = function(path, folder, n){
  data.all = list()
  for(i in folder){
    data = get_data(path, i)
    model = get_models(data)
    objective = get_objective(dimension = as.numeric(substr(i, 1, (nchar(i)-1))))
    testset = get_testset(n, objective)
    data.all[[i]] = list("model" = model, "objective" = objective, "testset" = testset)
  }
  return(data.all)
}

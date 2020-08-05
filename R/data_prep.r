# create data lists
data_btss = list(
  "mbo_lambda0.5" = readRDS("data/BTSS/mbo_lambda0.5.rds"),
  "mbo_lambda1" = readRDS("data/BTSS/mbo_lambda1.rds"),
  "mbo_lambda2" = readRDS("data/BTSS/mbo_lambda2.rds"),
  "test" = readRDS("data/BTSS/randomLHS_1000.rds")
)

data_kc1 = list(
  "mbo_lambda0.5" = readRDS("data/kc1/mbo_lambda0.5.rds"),
  "mbo_lambda1" = readRDS("data/kc1/mbo_lambda1.rds"),
  "mbo_lambda2" = readRDS("data/kc1/mbo_lambda2.rds"),
  "test" = readRDS("data/kc1/randomLHS_1000.rds")
)

get_objects <- function(data, lambda){
  objectname = paste0("mbo_lambda", lambda)
  object = data[[objectname]]
  model = object$models$models
  data_train = object$tasks[[1]]$env$data
  data_test = data$test
  return(list("model" = model, "data_train" = data_train, "data_test" = data_test))
}


#test = get_objects(data = data_btss, lambda = 0.5)
get_objects(data_btss, 1.0)

library(iml)
library(data.table)

folder = list.files("data/")
exclude =  which(folder %in% c("data.RData", "numerai28.6", "mlp"))
folder = folder[-exclude]

for (i in folder) {
  data = readRDS(paste0("data/",i, "/mlrmbo_30_repls.rds"))
  data.list = list(
    "mbo_lambda0.5" = data$result[1:30],
    "mbo_lambda1" = data$result[31:60],
    "mbo_lambda2" = data$result[61:90],
    "test" = data$result[91]
  )
  assign(paste0("data_", tolower(i)), data.list)
  
}


get_objects <- function(data, lambda, iteration){
  objectname = paste0("mbo_lambda", lambda)
  object = data[[objectname]][[iteration]]$res
  model = object$models[[2]]
  model.best = object$models[[1]]
  test.holdout = object$opt.path$env$path[-(1:object$best.ind),]
  data.train = object$opt.path$env$path
  data.test = data$test[[1]]$res$opt.path$env$path
  return(list("model" = model, "data_train" = data.train, "data_test" = data.test 
              ,"model_best" = model.best, "test_holdout" = test.holdout
  ))
}



n_rep = 30
lambda = unique(na.omit(data$lambda))
model.list = vector("list", 3)
names(model.list) = lambda
# calculate and save ggplots
for(j in 1:length(lambda)){
  for(i in 1:n_rep){
    object = get_objects(data_btsc, lambda[j], i)
    data.train.sub = object$data_train
    data.train.sub$extrapol = lambda[j]
    data.train.sub$iteration = i
    if(j == 1 & i == 1){
      data.train = data.train.sub
    }
    else {
      data.train = rbind(data.train, data.train.sub)
    }
    model.list[[j]][[i]] = object$model
    
  }
  data.test.sub = object$data_test
  data.test.sub$extrapol = lambda[j]
  if(j == 1) data.test = data.test.sub
  else data.test = rbind(data.test, data.test.sub)
  
}

set.seed(1234)
test.sample = sample(1:(nrow(data.test)/length(lambda)), 2000)





# pdp and ale
load("~/RProjects/paper_2020_xautoml/df_pdpale.Rdata")


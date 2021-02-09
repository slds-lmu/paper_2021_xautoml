


library(iml)
library(data.table)
library(ranger)



folder_mlp = list.files("data/mlp/")[4]
for (i in folder_mlp) {
  data = readRDS(paste0("data/mlp/",i, "/mlrmbo30_vs_randomLHS.rds"))
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
n.rep = 30
lambda = unique(na.omit(data$lambda))
model.list.mbo = vector("list", length(lambda))
names(model.list.mbo) = lambda
# calculate and save ggplots
f = folder_mlp
for(j in 1:length(lambda)){
  for(i in 1:n.rep){
    if (i < n.rep) object = get_objects_mlp(get(paste0("data_", f)), lambda[j], i, i+1)
    else object = get_objects_mlp(get(paste0("data_", f)), lambda[j], i, 1)
    data.train.sub = object$data.train
    data.train.sub$extrapol = lambda[j]
    data.train.sub$iteration = i
    
    data.test.sub = object$data.test
    data.test.sub$extrapol = lambda[j]
    data.test.sub$iteration = i
    if(j == 1 & i == 1){
      data.train.mbo = data.train.sub
      data.test.mbo = data.test.sub
    }
    else {
      data.train.mbo = rbind(data.train.mbo, data.train.sub)
      data.test.mbo = rbind(data.test.mbo, data.test.sub)
    }
    model.list.mbo[[j]][[i]] = object$model
    
  }
  
}


# extract data and models for rlhs
model.list.rlhs = list()
  for(i in 1:n.rep){
    data = get(paste0("data_", f))
    object.train = data[["rlhs"]][[i]]
    model = object.train$models[[1]]
    data.train = object.train$train_data_randomLHS[[1]][, 1:which(colnames(object.train$train_data_randomLHS[[1]]) == "y")]
    
    data.train$iteration = i
    
    if(i == 1){
      data.train.rlhs = data.train
    }
    else {
      data.train.rlhs = rbind(data.train.rlhs, data.train)
    }
    model.list.rlhs[[i]] = model
    
  }
  



# Effects

# max_depth as numeric (otherwise differing grid sizes for ale)
#data.train$max_depth = as.numeric(data.train$max_depth)
#data.test$max_depth = as.numeric(data.test$max_depth)
features = colnames(data.train[,1:(which(colnames(data.train)=="y")-1)])
data = data_phoneme
surrogate = data$surrogate_model
#############################################################################################
# calculate ground truth for pdp and ale
surrogate.data = data$surrogate_data[,features]
surrogate.data$max_dropout = as.numeric(as.character(surrogate.data$max_dropout))
surrogate.data$num_layers = as.numeric(as.character(surrogate.data$num_layers))
surrogate.data$num_layers[which(is.na(surrogate.data$num_layers))] = 1
surrogate.data$learning_rate = as.numeric(as.character(surrogate.data$learning_rate))
surrogate.data$momentum = as.numeric(as.character(surrogate.data$momentum))
surrogate.data$weight_decay = as.numeric(as.character(surrogate.data$weight_decay))
surrogate.data$y = 1 - (as.numeric(as.character(data$surrogate_data$final_val_balanced_accuracy))/100)
predictor.surrogate = Predictor$new(model = surrogate, data = surrogate.data)
predictor.surrogate.rLHS = Predictor$new(model = surrogate, data = data$rlhs_test_data[test.sample,1:(length(features)+1)])

for(k in features) {
  surrogate.pdp.sub = setnames(FeatureEffect$new(predictor = predictor.surrogate, feature = k, method = "pdp")$results[,c(k, ".value")], c("feature", ".value"))
  surrogate.ale.sub = setnames(FeatureEffect$new(predictor = predictor.surrogate, feature = k, method = "ale")$results[,c(k, ".value")], c("feature", ".value"))
  
  surrogate.rLHS.pdp.sub = setnames(FeatureEffect$new(predictor = predictor.surrogate.rLHS, feature = k, method = "pdp")$results[,c(k, ".value")], c("feature", ".value"))
  surrogate.rLHS.ale.sub = setnames(FeatureEffect$new(predictor = predictor.surrogate.rLHS, feature = k, method = "ale")$results[,c(k, ".value")], c("feature", ".value"))
  
  if (k == features[1]) {
    surrogate.pdp = surrogate.pdp.sub
    surrogate.ale = surrogate.ale.sub
    surrogate.rLHS.pdp = surrogate.rLHS.pdp.sub
    surrogate.rLHS.ale = surrogate.rLHS.ale.sub
  }
  else{
    surrogate.pdp = rbind(surrogate.pdp, surrogate.pdp.sub)
    surrogate.ale = rbind(surrogate.ale, surrogate.ale.sub)
    surrogate.rLHS.pdp = rbind(surrogate.rLHS.pdp, surrogate.rLHS.pdp.sub)
    surrogate.rLHS.ale = rbind(surrogate.rLHS.ale, surrogate.rLHS.ale.sub)
  }
}
##############################################################################################

# sample test data - size = 2000 (one random sample that is taken in each loop - or better to sample eacht time randomly?)
set.seed(1234)
test.sample = sample(1:(nrow(data$rlhs_test_data)), 500)

features = colnames(data.train.mbo)[-which(colnames(data.train.mbo) %in% c("y", "extrapol", "iteration"))]
lambda = as.character(lambda)
df.pdp.mbo = data.frame("feature" = character(), "lambda" = character(), "grid.train" = numeric(), "mean.train" = numeric(), "sd.train" = numeric(), 
                    "grid.test" = numeric(), "mean.test" = numeric(), "sd.test" = numeric(), 
                    "grid.sur" = numeric(), "mean.sur" = numeric(), "sd.sur" = numeric(),
                    "hyper.q25.train" = numeric(), "hyper.mean.train" = numeric(), "hyper.q75.train" = numeric(), 
                    "hyper.q25.test" = numeric(), "hyper.mean.test" = numeric(), "hyper.q75.test" = numeric())

df.pdp.rLHS = data.frame("feature" = character(), "lambda" = character(), "grid.train" = numeric(), "mean.train" = numeric(), "sd.train" = numeric(), 
                         "grid.test" = numeric(), "mean.test" = numeric(), "sd.test" = numeric(), 
                         "grid.test.mbo" = numeric(), "mean.test.mbo" = numeric(), "sd.test.mbo" = numeric(),
                         #"grid.sur" = numeric(), "mean.sur" = numeric(), "sd.sur" = numeric(),
                         "hyper.q25.train" = numeric(), "hyper.mean.train" = numeric(), "hyper.q75.train" = numeric(), 
                         "hyper.q25.test" = numeric(), "hyper.mean.test" = numeric(), "hyper.q75.test" = numeric())

#data.train.rLHS = data$rlhs_train_data
data.test.rLHS = data$rlhs_test_data[test.sample,]
#model.list.rLHS = data$rlhs_model


# Todo: Schleife über Datensätze
for (l in lambda) {
  for (k in features) {
  
    data.all = calculate_iterations(data.train.mbo, data.test.mbo, data.train.rlhs, data.test.rLHS, l, i, k, features, n.rep, test.sample, model.list.mbo, model.list.rlhs, surrogate)
    
    
    
    aggr.pdp.mbo = data_aggr(data.all[["pdp.mbo"]], k)
    aggr.pdp.rLHS = data_aggr(data.all[["pdp.rLHS"]], k)
    #aggr.ale = data_aggr(data.all[["ale"]], k)
    best.config.mbo.train = as.numeric(summary(data.all[["best.config.mbo"]][[1]])[c(2,4,5)])
    best.config.mbo.test = as.numeric(summary(data.all[["best.config.mbo"]][[2]])[c(2,4,5)])
    best.config.rLHS.train = as.numeric(summary(data.all[["best.config.rLHS"]][[1]])[c(2,4,5)])
    best.config.rLHS.test = as.numeric(summary(data.all[["best.config.rLHS"]][[2]])[c(2,4,5)])
    
    
    df.pdp.mbo = rbind(df.pdp.mbo, setnames(data.frame(k, l, aggr.pdp.mbo[[1]][[1]], aggr.pdp.mbo[[1]][[2]], aggr.pdp.mbo[[1]][[3]], 
                                               aggr.pdp.mbo[[2]][[1]], aggr.pdp.mbo[[2]][[2]], aggr.pdp.mbo[[2]][[3]], 
                                               aggr.pdp.mbo[[3]][[1]], aggr.pdp.mbo[[3]][[2]], aggr.pdp.mbo[[3]][[3]],
                                               best.config.mbo.train[1], best.config.mbo.train[2], best.config.mbo.train[3],
                                                best.config.mbo.test[1], best.config.mbo.test[2], best.config.mbo.test[3]),
                                    names(df.pdp.mbo)))
    df.pdp.rLHS = rbind(df.pdp.rLHS, setnames(data.frame(k, l, aggr.pdp.rLHS[[1]][[1]], aggr.pdp.rLHS[[1]][[2]], aggr.pdp.rLHS[[1]][[3]], 
                                                   aggr.pdp.rLHS[[2]][[1]], aggr.pdp.rLHS[[2]][[2]], aggr.pdp.rLHS[[2]][[3]], 
                                                   aggr.pdp.rLHS[[3]][[1]], aggr.pdp.rLHS[[3]][[2]], aggr.pdp.rLHS[[3]][[3]],
                                                   #aggr.pdp.rLHS[[4]][[1]], aggr.pdp.rLHS[[4]][[2]], aggr.pdp.rLHS[[4]][[3]],
                                                   best.config.rLHS.train[1], best.config.rLHS.train[2], best.config.rLHS.train[3],
                                                   best.config.rLHS.test[1], best.config.rLHS.test[2], best.config.rLHS.test[3]),
                                        names(df.pdp.rLHS)))
    
    df.ale.mbo.train.sub = data.frame("data" = "train", "feature" = k, "lambda" = l, "grid" = data.all$ale.mbo[[1]][,3], data.all$ale.mbo[[1]][,c(".value", "iteration")],  best.config.mbo.train[1], best.config.mbo.train[2], best.config.mbo.train[3])
    df.ale.mbo.test.sub  = data.frame("data" = "test", "feature" = k, "lambda" = l, "grid" = data.all$ale.mbo[[2]][,3], data.all$ale.mbo[[2]][,c(".value", "iteration")], best.config.mbo.test[1], best.config.mbo.test[2], best.config.mbo.test[3])
    
    df.ale.rLHS.train.sub = data.frame("data" = "train", "feature" = k, "lambda" = l, "grid" = data.all$ale.rLHS[[1]][,3], data.all$ale.rLHS[[1]][,c(".value", "iteration")],  best.config.rLHS.train[1], best.config.rLHS.train[2], best.config.rLHS.train[3])
    df.ale.rLHS.test.sub  = data.frame("data" = "test", "feature" = k, "lambda" = l, "grid" = data.all$ale.rLHS[[2]][,3], data.all$ale.rLHS[[2]][,c(".value", "iteration")], best.config.rLHS.test[1], best.config.rLHS.test[2], best.config.rLHS.test[3])
    
    #df.ale.rLHS.mbo.train.sub = data.frame("data" = "train", "feature" = k, "lambda" = l, "grid" = data.all$ale.rLHS.mbo[[1]][,3], data.all$ale.rLHS.mbo[[1]][,c(".value", "iteration")],  best.config.rLHS.train[1], best.config.rLHS.train[2], best.config.rLHS.train[3])
    df.ale.rLHS.mbo.test.sub  = data.frame("data" = "test", "feature" = k, "lambda" = l, "grid" = data.all$ale.rLHS[[3]][,3], data.all$ale.rLHS[[3]][,c(".value", "iteration")], best.config.rLHS.test[1], best.config.rLHS.test[2], best.config.rLHS.test[3])
    
    if(!exists("df.ale.mbo.train")) {
      df.ale.mbo.train = df.ale.mbo.train.sub
      df.ale.mbo.test = df.ale.mbo.test.sub
      
      df.ale.rLHS.train = df.ale.rLHS.train.sub
      df.ale.rLHS.test = df.ale.rLHS.test.sub
      
      #df.ale.rLHS.mbo.train = df.ale.mbo.train.sub
      df.ale.rLHS.mbo.test = df.ale.rLHS.mbo.test.sub
    }
    
    else {
      df.ale.mbo.train = rbind(df.ale.mbo.train, df.ale.mbo.train.sub)
      df.ale.mbo.test = rbind(df.ale.mbo.test, df.ale.mbo.test.sub)
      
      df.ale.rLHS.train = rbind(df.ale.rLHS.train, df.ale.rLHS.train.sub)
      df.ale.rLHS.test = rbind(df.ale.rLHS.test, df.ale.rLHS.test.sub)
      
      #df.ale.rLHS.mbo.train = rbind(df.ale.rLHS.mbo.train, df.ale.rLHS.mbo.train.sub)
      df.ale.rLHS.mbo.test = rbind(df.ale.rLHS.mbo.test, df.ale.rLHS.mbo.test.sub)
    }
    
  }
    
  
  # calculate Feature Importance
  pfi = calculate_feature_importance(data.train.mbo, data.test.mbo, data.train.rlhs, data.test.rLHS, l, n.rep, model.list.mbo, model.list.rlhs)
  
  if(!exists("df.pfi.mbo")) {
    df.pfi.mbo = pfi$pfi.mbo
    df.pfi.rLHS = pfi$pfi.rLHS
    df.pfi.rLHS.mbo = pfi$pfi.rLHS.mbo
  }
  
  else {
    df.pfi.mbo = rbind(df.pfi.mbo, pfi$pfi.mbo)
    df.pfi.rLHS = rbind(df.pfi.rLHS, pfi$pfi.rLHS)
    df.pfi.rLHS.mbo = rbind(df.pfi.rLHS.mbo, pfi$pfi.rLHS.mbo)
  }
  
}


# for (l in lambda){
#   pfi = calculate_feature_importance(data.train.mbo, data.test.mbo, data.train.rLHS, data.test.rLHS,
#                                          l, n.rep, model.list.mbo, model.list.rLHS)
#   
#   if(l == lambda[1]) {
#     df.pfi.mbo = pfi$pfi.mbo
#     df.pfi.rLHS = pfi$pfi.rLHS
#     df.pfi.rLHS.mbo = pfi$pfi.rLHS.mbo
#   }
#   
#   else {
#     df.pfi.mbo = rbind(df.pfi.mbo, pfi$pfi.mbo)
#     df.pfi.rLHS = rbind(df.pfi.rLHS, pfi$pfi.rLHS)
#     df.pfi.rLHS.mbo = rbind(df.pfi.rLHS.mbo, pfi$pfi.rLHS.mbo)
#   }
# }


calculate_feature_importance = function(data.train.mbo, data.test.mbo, data.train.rlhs, data.test.rlhs, l, n.rep, model.list.mbo, model.list.rlhs) {
  for(i in 1:n.rep) {
    # define datasets and predictors for those - adjust with new data / model
    data.test.sub =  data.test.mbo[which(data.test.mbo$extrapol == l), c(1:(length(features) + 1))]
    predictor.test.mbo = Predictor$new(model = model.list.mbo[[l]][[i]], data = data.test.sub)
    predictor.test.rLHS = Predictor$new(model = model.list.rlhs[[i]], data = data.test.rLHS)
    predictor.test.rLHS.mbo = Predictor$new(model = model.list.mbo[[l]][[i]], data.test.rLHS)
    
    # PFI on test set of mbo data
    pfi.mbo.sub = data.frame("run" = i, "lambda" = l, FeatureImp$new(predictor = predictor.test.mbo, loss = "rmse")$results[,c("feature", "importance")])
    if (i == 1) pfi.mbo = pfi.mbo.sub
    else pfi.mbo = rbind(pfi.mbo, pfi.mbo.sub)
    
    # PFI on test set of rLHS data
    pfi.rLHS.sub = data.frame("run" = i, "lambda" = l, FeatureImp$new(predictor = predictor.test.rLHS, loss = "rmse")$results[,c("feature", "importance")])
    if (i == 1) pfi.rLHS = pfi.rLHS.sub
    else pfi.rLHS = rbind(pfi.rLHS, pfi.rLHS.sub)
    
    # PFI on test set of rLHS data
    pfi.rLHS.mbo.sub = data.frame("run" = i, "lambda" = l, FeatureImp$new(predictor = predictor.test.rLHS.mbo, loss = "rmse")$results[,c("feature", "importance")])
    if (i == 1) pfi.rLHS.mbo = pfi.rLHS.mbo.sub
    else pfi.rLHS.mbo = rbind(pfi.rLHS.mbo, pfi.rLHS.mbo.sub)
  }
  return(list("pfi.mbo" = pfi.mbo, "pfi.rLHS" = pfi.rLHS, "pfi.rLHS.mbo" = pfi.rLHS.mbo))
}

calculate_feature_effects <- function(predictor, feature, method, iteration){
  effect = FeatureEffect$new(predictor = predictor, feature = feature, method = method, grid.size = 50)$results
  effect$iteration = iteration
  effect$grid = 1:nrow(effect)
  return(effect)
}


calculate_iterations <- function(data.train.mbo, data.test.mbo, data.train.rLHS, data.test.rLHS, l, iteration, feat, features, n.rep, test.sample, model.list.mbo, model.list.rLHS, model.list.surrogate){
  for (i in 1:n.rep) {
    
    # mbo dataset
    
    # define datasets and predictors for those - adjust with new data / model
    data.train.sub = data.train.mbo[which(data.train.mbo$extrapol == l & data.train.mbo$iteration == i), c(1:(length(features) + 1))]
    data.test.sub =  data.test.mbo[which(data.test.mbo$extrapol == l & data.test.mbo$iteration == i), c(1:(length(features) + 1))]
    predictor.train.mbo = Predictor$new(model = model.list.mbo[[l]][[i]], data = data.train.sub)
    predictor.test.mbo = Predictor$new(model = model.list.mbo[[l]][[i]], data = data.test.sub)
    predictor.train.sur = Predictor$new(model = model.list.surrogate, data = data.train.sub)
    
    # pdp on train set of mbo data
    data.train.pdp.sub = calculate_feature_effects(predictor.train.mbo, k, "pdp", i)
    if (i == 1) data.train.mbo.pdp = data.train.pdp.sub
    else data.train.mbo.pdp = rbind(data.train.mbo.pdp, data.train.pdp.sub)
    
    # pdp on test set of mbo data
    data.test.pdp.sub = calculate_feature_effects(predictor.test.mbo, k, "pdp", i)
    if (i == 1) data.test.mbo.pdp = data.test.pdp.sub
    else data.test.mbo.pdp = rbind(data.test.mbo.pdp, data.test.pdp.sub)
    
    # surrogate pdp on mbo train data
    data.train.pdp.sub = calculate_feature_effects(predictor.train.sur, k, "pdp", i)
    if (i == 1) data.train.sur.pdp = data.train.pdp.sub
    else data.train.sur.pdp = rbind(data.train.sur.pdp, data.train.pdp.sub)
    
    # ale on train set of mbo data
    data.train.ale.sub = calculate_feature_effects(predictor.train.mbo, k, "ale", i)
    if (i == 1) data.train.mbo.ale = data.train.ale.sub
    else data.train.mbo.ale = rbind(data.train.mbo.ale, data.train.ale.sub)
    
    # ale on test set of mbo data
    data.test.ale.sub = calculate_feature_effects(predictor.test.mbo, k, "ale", i)
    if (i == 1) data.test.mbo.ale = data.test.ale.sub
    else data.test.mbo.ale = rbind(data.test.mbo.ale, data.test.ale.sub)
    
    # surrogate ale on mbo train data
    data.train.ale.sub = calculate_feature_effects(predictor.train.sur, k, "ale", i)
    if (i == 1) data.train.sur.ale = data.train.ale.sub
    else data.train.sur.ale = rbind(data.train.sur.ale, data.train.ale.sub)
    
    
    # best config - train
    if (i == 1) best.config.mbo.train = data.train.sub[which.min(data.train.sub$y), k]
    else best.config.mbo.train = c(best.config.mbo.train, data.train.sub[which.min(data.train.sub$y), k])
    
    # best config - text
    if (i == 1) best.config.mbo.test = data.test.sub[which.min(data.test.sub$y), k]
    else best.config.mbo.test = c(best.config.mbo.test, data.test.sub[which.min(data.test.sub$y), k])
    
    
    
    
    
    # rLHS dataset
    
    # define datasets and predictors for those - adjust with new data / model
    data.train.sub = data.train.rlhs[which(data.train.rlhs$iteration == i), c(1:(length(features) + 1))]
    data.test.sub =  data.test.rLHS#[which(data.test.mbo$extrapol == l), c(1:(length(features) + 1))]
    predictor.train.rLHS = Predictor$new(model = model.list.rlhs[[i]], data = data.train.sub)
    predictor.test.rLHS = Predictor$new(model = model.list.rlhs[[i]], data = data.test.sub)
    predictor.test.mbo.rLHS = Predictor$new(model = model.list.mbo[[l]][[i]], data.test.sub)
    
    # pdp on train set of rLHS data 
    data.train.pdp.sub = calculate_feature_effects(predictor.train.rLHS, k, "pdp", i)
    if (i == 1) data.train.rLHS.pdp = data.train.pdp.sub
    else data.train.rLHS.pdp = rbind(data.train.rLHS.pdp, data.train.pdp.sub)
    
    # pdp on test set of rLHS data with mbo model
    data.test.pdp.sub = calculate_feature_effects(predictor.test.mbo.rLHS, k, "pdp", i)
    if (i == 1) data.test.rLHS.mbo.pdp = data.test.pdp.sub
    else data.test.rLHS.mbo.pdp = rbind(data.test.rLHS.mbo.pdp, data.test.pdp.sub)
    
    # pdp on test set of rLHS data 
    data.test.pdp.sub = calculate_feature_effects(predictor.test.rLHS, k, "pdp", i)
    if (i == 1) data.test.rLHS.pdp = data.test.pdp.sub
    else data.test.rLHS.pdp = rbind(data.test.rLHS.pdp, data.test.pdp.sub)
    
    # ale on train set of rLHS data 
    data.train.ale.sub = calculate_feature_effects(predictor.train.rLHS, k, "ale", i)
    if (i == 1) data.train.rLHS.ale = data.train.ale.sub
    else data.train.rLHS.ale = rbind(data.train.rLHS.ale, data.train.ale.sub)
    
    # ale on test set of rLHS data with mbo model
    data.test.ale.sub = calculate_feature_effects(predictor.test.mbo.rLHS, k, "ale", i)
    if (i == 1) data.test.rLHS.mbo.ale = data.test.ale.sub
    else data.test.rLHS.mbo.ale = rbind(data.test.rLHS.mbo.ale, data.test.ale.sub)
    
    # ale on test set of rLHS data 
    data.test.ale.sub = calculate_feature_effects(predictor.test.rLHS, k, "ale", i)
    if (i == 1) data.test.rLHS.ale = data.test.ale.sub
    else data.test.rLHS.ale = rbind(data.test.rLHS.ale, data.test.ale.sub)
    
    
    # best config - train
    if (i == 1) best.config.rLHS.train = data.train.sub[which.min(data.train.sub$y), k]
    else best.config.rLHS.train = c(best.config.rLHS.train, data.train.sub[which.min(data.train.sub$y), k])
    
    # best config - text
    if (i == 1) best.config.rLHS.test = data.test.sub[which.min(data.test.sub$y), k]
    else best.config.rLHS.test = c(best.config.rLHS.test, data.test.sub[which.min(data.test.sub$y), k])
  }
  return(list("pdp.mbo" = list("train" = data.train.mbo.pdp, "test" = data.test.mbo.pdp, "train.sur" = data.train.sur.pdp), "ale.mbo" = list("train" = data.train.mbo.ale, "test" = data.test.mbo.ale, "train.sur" = data.train.sur.ale), "best.config.mbo" = list(best.config.mbo.train, best.config.mbo.test),
              "pdp.rLHS" = list("train" = data.train.rLHS.pdp, "test" = data.test.rLHS.pdp, "test.mbo" = data.test.rLHS.mbo.pdp), "ale.rLHS" = list("train" = data.train.rLHS.ale, "test" = data.test.rLHS.ale, "test.mbo" = data.test.rLHS.mbo.ale), "best.config.rLHS" = list(best.config.rLHS.train, best.config.rLHS.test)))
}


data_aggr = function(data, feat){
  l.aggr = list()
  for(m in 1:length(data)){
    data.sub = data[[m]]
    mean = aggregate(data.sub[,c(k, ".value")], list(data.sub$grid), mean)
    sd = aggregate(data.sub[,".value"], list(data.sub$grid), sd)[,2]
    l.aggr[[m]] = list(mean[2], mean[3], sd)
  }
  return(l.aggr)
}


save(df.ale.mbo.train, df.ale.mbo.test, df.ale.rLHS.mbo.test, df.ale.rLHS.train, df.ale.rLHS.test,
     df.pdp.mbo, df.pdp.rLHS, df.pfi.mbo, df.pfi.rLHS, df.pfi.rLHS.mbo, data.train.mbo, data.test.mbo, data.train.rlhs,
     data.test.rLHS, model.list.mbo, model.list.rlhs, file = "effectsPfi_phoneme.RData")


#test = get_objects(data_btsc, 1, 1)
# 
# 
# 
# 
# best.params = readRDS("../moc/saved_objects/best_configs.rds")  # generated by irace in folder appendix_irace
# USE_TRAINED_MODEL = TRUE
# PARALLEL = TRUE
# 
# x.interest = test$data_train[200,]
# data = test$data_train[-200,]
# 
# pred = Predictor$new(model = test$model, data = data,
#                      conditional = FALSE)
# ctr = partykit::ctree_control(maxdepth = 5L)
# 
# set.seed(1234)
# pred$conditionals = fit_conditionals(pred$data$get.x(), ctrl = ctr)
# 
# ###---- Compute counterfactuals ----
# prediction = pred$predict(x.interest)$.prediction
# 
# set.seed(1000)
# credit.cf = Counterfactuals$new(predictor = pred, x.interest = x.interest,
#                                 target = c(min(data$y), prediction+0.05*prediction),
#                                 epsilon = 0, generations = list(mosmafs::mosmafsTermStagnationHV(10),
#                                 mosmafs::mosmafsTermGenerations(200)),
#                                 # mu = best.params$mu,
#                                 # p.mut = best.params$p.mut, p.rec = best.params$p.rec,
#                                 # p.mut.gen = best.params$p.mut.gen,
#                                 # p.mut.use.orig = best.params$p.mut.use.orig,
#                                 # p.rec.gen = best.params$p.rec.gen,
#                                 initialization = "icecurve"#,
#                                 # p.rec.use.orig = best.params$p.rec.use.orig
#                                 )
# 
# # Number of counterfactuals
# nrow(credit.cf$results$counterfactuals)
# id = credit.cf$results$counterfactuals$dist.target < 0.01
# sum(id)
# 
# # Focus counterfactuals that met target
# credit.cf$results$counterfactuals = credit.cf$results$counterfactuals[which(id), ]
# credit.cf$results$counterfactuals.diff = credit.cf$results$counterfactuals.diff[which(id), ]
# 
# # Get relative frequency of feature changes
# credit.cf$get_frequency()
# 
# 
# ###---- Plots ----
# a = credit.cf$plot_parallel(features = c("lambda", "colsample_bytree", "max_depth"), plot.x.interest = TRUE)
# a = a + scale_x_discrete(expand = c(0.1, 0.1), labels= c("lambda", "colsample_bytree", "max_depth"))
# a
# b = credit.cf$plot_surface(features = c("lambda", "colsample_bytree"))
# b
# c = credit.cf$plot_hv()
# c



library("cowplot")
tmp = data.frame(first = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n'), 
                 second = c(2,3,4,5,2,3,4,5,6,3,4,4,6, 7))

plot_tmp = ggplot(tmp, aes(first, second)) + geom_bar(stat = 'identity') 
dev.new()

if("png" %in% installed.packages()){
  library(png)
}else{
  install.packages("png")
  library(png)
}


save_plot("~/plot_tmp.png", plot_tmp, base_height = NULL, base_aspect_ratio = 1.618, 
          base_width = 6)

library(OpenImageR)

img<-OpenImageR::readImage("~/plot_tmp.png")
imageShow(img)

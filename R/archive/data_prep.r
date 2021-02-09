# create data lists
# data_btss = list(
#   "mbo_lambda0.5" = readRDS("data/BTSS/mbo_lambda0.5.rds"),
#   "mbo_lambda1" = readRDS("data/BTSS/mbo_lambda1.rds"),
#   "mbo_lambda2" = readRDS("data/BTSS/mbo_lambda2.rds"),
#   "test" = readRDS("data/BTSS/randomLHS_1000.rds")
# )
# 
# data_kc1 = list(
#   "mbo_lambda0.5" = readRDS("data/kc1/mbo_lambda0.5.rds"),
#   "mbo_lambda1" = readRDS("data/kc1/mbo_lambda1.rds"),
#   "mbo_lambda2" = readRDS("data/kc1/mbo_lambda2.rds"),
#   "test" = readRDS("data/kc1/randomLHS_1000.rds")
# )


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

folder_mlp = list.files("data/mlp/")
# for (i in folder_mlp) {
#   data = readRDS(paste0("data/mlp/",i, "/mlrmbo_30_repls.rds"))
#   data.list = list(
#     "mbo_lambda0.5" = data$result[1],
#     "mbo_lambda1" = data$result[2],
#     "mbo_lambda2" = data$result[3],
#     "test" = read.csv(paste0("data/mlp/",i, "/lcbench2000.csv"), sep = ",", dec = ".", header = TRUE),
#     "surrogate" = readRDS(paste0("data/mlp/",i, "/surrogate.rds"))
#   )
#   assign(paste0("data_", tolower(i)), data.list)
#   
# }




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

get_objects_mlp <- function(data, lambda, iteration){
  objectname = paste0("mbo_lambda", lambda)
  object = data[[objectname]][[iteration]]
  model = object$models[[2]]
  model.best = object$models[[1]]
  test.holdout = object$opt.path[-(1:(nrow(object$opt.path) - as.numeric(model$task.desc$size) + as.numeric(model.best$task.desc$size))),1:which(colnames(object$opt.path) == "y")]
  data.train = object$opt.path[, 1:which(colnames(object$opt.path) == "y")]
  data.test = data$test[, which(colnames(data$test) %in% c(data$surrogate$result[[1]]$model_val_balanced_acc[[1]]$features, "final_val_balanced_accuracy"))]
  data.test = rename(data.test, "y" = "final_val_balanced_accuracy")
  data.test$num_layers[which(data.test$num_layers == "True")] = 1
  data.test$num_layers = as.numeric(data.test$num_layers)
  surrogate = data$surrogate$result[[1]]$model_val_balanced_acc[[1]]
  return(list("model" = model, "data_train" = data.train, "data_test" = data.test 
              ,"model_best" = model.best, "test_holdout" = test.holdout, "surrogate" = surrogate
  ))
}


#test = get_objects_mlp(data = data_adult, lambda = 0.5, 1)
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


# Effects

# max_depth as numeric (otherwise differing grid sizes for ale)
data.train$max_depth = as.numeric(data.train$max_depth)
data.test$max_depth = as.numeric(data.test$max_depth)

# sample test data - size = 2000 (one random sample that is taken in each loop - or better to sample eacht time randomly?)
set.seed(1234)
test.sample = sample(1:(nrow(data.test)/length(lambda)), 2000)

features = colnames(data.train)[-which(colnames(data.train) %in% c("y", "extrapol", "iteration"))]
lambda = as.character(lambda)
df_pdp = data.frame("feature" = character(), "lambda" = character(), "grid.train" = numeric(), "mean.train" = numeric(), "sd.train" = numeric(), "grid.test" = numeric(), "mean.test" = numeric(), "sd.test" = numeric())
df_ale = data.frame("feature" = character(), "lambda" = character(), "grid.train" = numeric(), "mean.train" = numeric(), "sd.train" = numeric(), "grid.test" = numeric(), "mean.test" = numeric(), "sd.test" = numeric())
for (k in feature) {
  for (l in lambda) {
    for (i in 1:n_rep) {
      data.train.sub = data.train[which(data.train$extrapol == l & data.train$iteration == i), c(1:(length(features) + 1))]
      data.test.sub =  data.test[which(data.test$extrapol == l)[test.sample], c(1:(length(features) + 1))]
      
      # pdp on train set
      predictor = Predictor$new(model = model.list[[l]][[i]], data = data.train.sub)
      data.train.pdp.sub = FeatureEffect$new(predictor = predictor, feature = k, method = "pdp")$results
      data.train.pdp.sub$iteration = i
      data.train.pdp.sub$grid = 1:nrow(data.train.pdp.sub)
      if (i == 1) data.train.pdp = data.train.pdp.sub
      else data.train.pdp = rbind(data.train.pdp, data.train.pdp.sub)
      
      # pdp on test set
      predictor.test = Predictor$new(model = model.list[[l]][[i]], data = data.test.sub)
      data.test.pdp.sub = FeatureEffect$new(predictor = predictor.test, feature = k, method = "pdp")$results
      data.test.pdp.sub$iteration = i
      data.test.pdp.sub$grid = 1:nrow(data.test.pdp.sub)
      if (i == 1) data.test.pdp = data.test.pdp.sub
      else data.test.pdp = rbind(data.test.pdp, data.test.pdp.sub)
      
      # ale on train set
      data.train.ale.sub = FeatureEffect$new(predictor = predictor, feature = k, method = "ale")$results
      data.train.ale.sub$iteration = i
      data.train.ale.sub$grid = 1:nrow(data.train.ale.sub)
      if (i == 1) data.train.ale = data.train.ale.sub
      else data.train.ale = rbind(data.train.ale, data.train.ale.sub)
      
      # ale on test set
      data.test.ale.sub = FeatureEffect$new(predictor = predictor.test, feature = k, method = "ale")$results
      data.test.ale.sub$iteration = i
      data.test.ale.sub$grid = 1:nrow(data.test.ale.sub)
      if (i == 1) data.test.ale = data.test.ale.sub
      else data.test.ale = rbind(data.test.ale, data.test.ale.sub)
      
    }
    # mean and sd of pdp - train
    aggr.train.pdp = aggregate(data.train.pdp[,c(k, ".value")], list(data.train.pdp$grid), mean)
    sd.train.pdp = aggregate(data.train.pdp[,".value"], list(data.train.pdp$grid), sd)[,2]
    
    # mean and sd of pdp - test
    aggr.test.pdp = aggregate(data.test.pdp[,c(k, ".value")], list(data.test.pdp$grid), mean)
    sd.test.pdp = aggregate(data.test.pdp[,".value"], list(data.test.pdp$grid), sd)[,2]
    
    # mean and sd of ale - train
    aggr.train.ale = aggregate(data.train.ale[,c(k, ".value")], list(data.train.ale$grid), mean)
    sd.train.ale = aggregate(data.train.ale[,".value"], list(data.train.ale$grid), sd)[,2]
    
    # mean and sd of ale - test
    aggr.test.ale = aggregate(data.test.ale[,c(k, ".value")], list(data.test.ale$grid), mean)
    sd.test.ale = aggregate(data.test.ale[,".value"], list(data.test.ale$grid), sd)[,2]
    
    
    
    
    df_pdp = rbind(df_pdp, setnames(data.frame(k, l, aggr.train.pdp[2], aggr.train.pdp[3], sd.train.pdp, aggr.test.pdp[2], aggr.test.pdp[3], sd.test.pdp), names(df_pdp)))
    df_ale = rbind(df_ale, setnames(data.frame(k, l, aggr.train.ale[2], aggr.train.ale[3], sd.train.ale, aggr.test.ale[2], aggr.test.ale[3], sd.test.ale), names(df_ale)))
    
  }
}



df_pdp_5to9 = df_pdp
df_ale_5to9 = df_ale

save(df_pdp_1to4, df_pdp_5to9, df_ale_1to4, df_ale_5to9, file = "df_pdpale.Rdata")



df_pdp_1to4 = df_pdp
df_ale_1to4 = df_ale
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

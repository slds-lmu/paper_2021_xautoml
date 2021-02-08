source("R/mlp_helper.r")
library(iml)
library(data.table)
library(ranger)
library(mlr)

library(mlrMBO)
library(BBmisc)
library(customtrees)
library(ggplot2)

library(gridExtra)

source("R/pdp_helpers2.R")
source("R/tree_splitting.R")
source("R/helper_evaulation.R")




# Examples with phoneme data
path = "data/mlp/"
folder_mlp = "phoneme"
data = get_data(path, folder_mlp)
data = data[["data_phoneme"]]

# get mbo and rlhs data and models
lambda = c("1")
mbo_rlhs_data = get_models_data(data, lambda)
features = colnames(mbo_rlhs_data$data.train.mbo[,1:(which(colnames(mbo_rlhs_data$data.train.mbo)=="y")-1)])


# surrogate model and data
surrogate = data$surrogate_model

surrogate.data = data$surrogate_data[,features]
surrogate.data$max_dropout = as.numeric(as.character(surrogate.data$max_dropout))
surrogate.data$num_layers = as.numeric(as.character(surrogate.data$num_layers))
surrogate.data$num_layers[which(is.na(surrogate.data$num_layers))] = 1
surrogate.data$learning_rate = as.numeric(as.character(surrogate.data$learning_rate))
surrogate.data$momentum = as.numeric(as.character(surrogate.data$momentum))
surrogate.data$weight_decay = as.numeric(as.character(surrogate.data$weight_decay))
surrogate.data$y = 1 - (as.numeric(as.character(data$surrogate_data$final_val_balanced_accuracy))/100)

predictor.surrogate = Predictor$new(model = surrogate, data = surrogate.data)

# "true" optimum
# optimum = surrogate.data[which.min(surrogate.data$y),features]
# optimum$batch_size = log(optimum$batch_size, 2)
# optimum$max_units = log(optimum$max_units, 2)


# training data and optimum from training data
l = "1"
i = 1
trainingdata = mbo_rlhs_data$data.train.mbo[which(mbo_rlhs_data$data.train.mbo$extrapol==1 & mbo_rlhs_data$data.train.mbo$iteration==1),1:8]
optimum = trainingdata[which.min(trainingdata$y),1:7]

# rlhs test data
set.seed(123)
testdata =  data$rlhs_test_data[sample(1:3000, 1000),features]


# mbo model and prediction of sd
model = mbo_rlhs_data$model.list.mbo[[l]][[i]]
mymodel = makeS3Obj("mymodel", fun = function() return(model))
predict.mymodel = function(object, newdata) {
  pred = predict(object$fun(), newdata = newdata)
  pp = getPredictionSE(pred)
  return(pp)
}



# test splitting

# feature of interest
feat = "learning_rate"



# calculate initial ice curves and pdp and groundtruth
df = testdata
res = marginal_effect_sd_over_mean(model, feat, df, 20, "pdp_sd", alpha = 0.05, correction = NULL)
res.pdp = res$pdp
res.ice = res$ice
q = qnorm(1 - 0.05 / 2)
res.pdp$lower = res.pdp$mean - q * res.pdp$sd
res.pdp$upper = res.pdp$mean + q * res.pdp$sd 
data.groundtruth = df
data.groundtruth$batch_size = 2^data.groundtruth$batch_size
data.groundtruth$max_units = 2^data.groundtruth$max_units

gt = predicted_marginal_effect(surrogate, feat, data.groundtruth, 20)
gt.pdp = gt$pdp
gt.ice = gt$ice
if (feat %in% c("batch_size", "max_units")) {
  gt.pdp[feat] = log(gt.pdp[feat], 2)
  gt.ice[feat] = log(gt.ice[feat], 2)
}
gt.pdp$mean = (100-gt.pdp$mean)/100
gt.ice$.value = (100-gt.ice$.value)/100




# plot function
library(dplyr)
pdp_plot = function(res.ice, gt.ice, idx, feat){
  res.ice = res.ice[which(res.ice$.id %in% idx),]
  res.pdp = res.ice %>% group_by(feat = res.ice[,feat]) %>% summarise(mean = mean(.value), sd = mean(sd))
  names(res.pdp)[which(names(res.pdp)=="feat")] = feat
  q = qnorm(1 - 0.05 / 2)
  res.pdp$lower = res.pdp$mean - q * res.pdp$sd
  res.pdp$upper = res.pdp$mean + q * res.pdp$sd 
  
  gt.ice = gt.ice[which(gt.ice$.id %in% idx),]
  gt.pdp = gt.ice %>% group_by(feat = gt.ice[,feat]) %>% summarise(mean = mean(.value))
  names(gt.pdp)[which(names(gt.pdp)=="feat")] = feat
  p = ggplot() + geom_ribbon(data = res.pdp, aes_string(x = feat, ymin = "lower", ymax = "upper"), alpha = 0.2)
  p = p + geom_line(data = res.pdp, aes_string(x = feat, y = "mean"), colour = "blue") 
  p = p + geom_line(data = gt.pdp, aes_string(x = feat, y = "mean")) 
  return(p)
}



# plot on all testdata
node.idx = 1:1000
pdp_plot(res.ice, gt.ice, node.idx, feat)

# calculate tree on test data and compare plot for optimal node
tree = compute_tree(model, df, feat, objective = "SS_area", n.split = 6, grid.size = 20, addMean = FALSE)
node.idx = find_optimal_node(tree, optimum)$subset.idx
pdp_plot(res.ice, gt.ice, node.idx, feat)


# calculate splitting on training data and plot on testdata

df = trainingdata[,1:7]
tree = compute_tree(model, df, feat, objective = "SS_L2", n.split = 8, grid.size = 20, addMean = FALSE)
split.criteria = find_split_criteria(tree, optimum = optimum)
testset = find_optimal_subset(testdata, split.criteria)
pdp_plot(res.ice, gt.ice, testset$id, feat)


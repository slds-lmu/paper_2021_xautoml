source("R/mlp_helper.r")
library(iml)
library(data.table)
library(ranger)
library(mlr)

library(mlrMBO)
library(BBmisc)
library(customtrees)
library(Rfast)
library(ggplot2)
library(gridExtra)

source("R/pdp_helpers.R")
source("R/tree_splitting_mlp.R")
source("R/benchmarks/synthetic/mbo_helpers.R")


# Examples with phoneme data
path = "data/runs/mlp/"
folder_mlp = "phoneme"
data = get_data(path, folder_mlp)
data = data[["data_phoneme"]]

# get mbo and rlhs data and models
lambda = c("0.5", "1","2","10")
mbo_rlhs_data = get_models_data(data, lambda)


# surrogate model and data
features = colnames(mbo_rlhs_data$data.train.mbo[,1:(which(colnames(mbo_rlhs_data$data.train.mbo)=="y")-1)])
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




# test splitting
l = "0.5"
i = 1
feat = "momentum"

data.test.sub =  data$rlhs_test_data[sample(1:3000, 1000),features]
model = mbo_rlhs_data$model.list.mbo[[l]][[i]]
df = data.test.sub

mymodel = makeS3Obj("mymodel", fun = function() return(model))
predict.mymodel = function(object, newdata) {
  pred = predict(object$fun(), newdata = newdata)
  pp = getPredictionSE(pred)
  return(pp)
}

predictor = Predictor$new(model = model, data = df)
predictor = Predictor$new(model = mymodel, data = df, predict.function = predict.mymodel)
effect = FeatureEffect$new(predictor = predictor, feature = feat, method = "ice")
eff = effect$results[effect$results$.id %in% test,]
plot(effect)
ggplot(eff, aes(x = momentum, y = .value, group = .id)) + geom_line()

# define objective
SS_L1 = function(y, x, requires.x = FALSE, ...) {
  require(Rfast)
  y = y *100
  #ypred = Rfast::colMedians(as.matrix(y))
  #sum(t(abs(t(y) - ypred)))
  ypred = colMeans(as.matrix(y))
  sum(t((t(y) - ypred)^2))
  #sum(t(abs(t(y)-rep(0,ncol(y)))))
}


# Compute tree
tree = compute_tree(effect, SS_L1, n.split = 3)
test = tree[[4]]$right.child$subset.idx


lapply(seq_len(length(tree) - 1), function(depth) {
  nodes = tree[[depth]]
  plist = lapply(nodes, function(node) {
    plot_pdp_for_node(effect, node, model, feat, objective.groundtruth = surrogate, method = "pdp_sd") + ylim(0.23,0.32)
  })
  do.call(grid.arrange, c(plist, nrow = 1))
})



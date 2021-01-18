source("R/mlp_helper.r")

library(iml)
library(data.table)
library(ranger)
library(mlr)

library(mlrMBO)
library(BBmisc)
library(customtrees)
#library(Rfast)
library(ggplot2)

library(gridExtra)

source("R/pdp_helpers2.R")
source("R/tree_splitting.R")
source("R/helper_evaulation.r")
#source("R/benchmarks/synthetic/mbo_helpers.R")


# -- 1. INPUT 

path = "data/mlp/"
path = "data/runs/mlp_new/"

# datasets we want to take into account for analysis 
folder_mlp = c("phoneme")

# lambda we want to do the analysis for 
lambda = 1


# -- 2. READ ALL THE DATA 
data = get_data(path, folder_mlp, lambda = lambda)
all_models = get_models(data)
objectives = get_objective(path, folder_mlp)
all_optima = get_optima(data)


# -- 3. Perform detailed analysis for a specific dataset 

dataset = "phoneme"
grid.size = 20
testdata.size = 1000

# Ground-truth
obj = objectives[[dataset]]$obj
surr_val = objectives[[dataset]]$surrogate_model
ps = getParamSet(obj)

# Surrogate models of the different mbo runs
models = all_models[[dataset]]
features = all_models[[dataset]][[1]]$features

# Found optimal values for the different mbo runs 
optima = all_optima[[dataset]]

# Compute some test we do our computations on 
testdata_path = file.path(path, dataset, paste0("testdata_", testdata.size, ".rds"))

if (file.exists(testdata_path)) {
  testdata = readRDS(testdata_path)
} else {
  testdata = generateRandomDesign(n = testdata.size, par.set = ps) # TODO: n must be much higher, this is just for testing
  saveRDS(testdata, testdata_path)
}

# Compute the ground truth data (we compute it once, and later on just store it)
gtdata = compute_ground_truth_pdps(obj = obj, path = path, dataset = dataset, features = features, testdata = testdata, testdata.size = testdata.size, grid.size = grid.size)

storepath = file.path(path, dataset, paste0("effects_and_trees2.rds"))

reslist = compute_trees(n.split = 6, models = models, features = features, optima = optima, testdata = testdata, storepath = storepath)

# reslist_backup = reslist

# Compute first all trees and then do the analysis afterwards
compute_trees = function(n.split, models, features, optima, testdata, storepath, plot = FALSE) {
    
  grid.size = 20
  objectives = c("SS_L2","SS_area")

  alpha = 0.05

  # Iterate over all models we have 

  reslist = list()

  for (i in seq_along(models[1:10])) {

    print(i)

    model = models[[i]]
    optimum = optima[[i]]

    results_for_features = list()

    for (feature in features) {

      # Compute all ice curves
      mymodel = makeS3Obj("mymodel", fun = function() return(model))
      
      predict.mymodel = function(object, newdata) {
        pred = predict(object$fun(), newdata = newdata)
        pp = getPredictionSE(pred)
        # if(addMean == TRUE) 
        #   pp = getPredictionResponse(pred) - 2 * pp
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

      q = qnorm(1 - alpha / 2)
      res.pdp$lower = res.pdp$mean - q * res.pdp$sd
      res.pdp$upper = res.pdp$mean + q * res.pdp$sd 

      # Get the ground-truth
      gt = gtdata[[feature]]
      gt.pdp = setDT(gt)[.type == "pdp", ]
      gt.ice = setDT(gt)[.type == "ice", ]

      if (plot) {
        p = ggplot(data = gt.pdp, aes_string(x = feature, y = "mean")) + geom_line(colour = "blue")
        p = p + geom_ribbon(data = res.pdp, aes_string(x = feature, ymin = "lower", ymax = "upper"), alpha = 0.2) + geom_line(data = res.pdp, aes_string(x = feature, y = "mean"))
        p = p + geom_vline(data = optimum, aes_string(xintercept = feature), colour = "orange", lty = 2)
        p
      }

      trees = list()

      for (objective in objectives) {

        trees[[objective]] = compute_tree(effect_sd = effect_sd, testdata = testdata, objective = objective, n.split = n.split) 
        end_t = Sys.time()

      }

      results_for_features[[feature]] = list(effects = effects_merged, res.pdp = res.pdp, res.ice = res.ice, gt.pdp = gt.pdp, gt.ice = gt.ice, trees = trees)
    }

    reslist[[i]] = results_for_features
  }

  saveRDS(reslist, storepath)

  reslist
}


test = function(n.split, data, testdata, surrogate, optimum, run) {
  
  df = data.frame()

  # Iterate over all models we have 
  for (i in seq_along(reslist)) {

    resmod = reslist[[i]]

    for (feature in names(resmod)) {

      res = reslist[[i]][[feature]]

      effects_merged = res$effects

      sf = c(feature, "mean", "sd", ".id")
      res.pdp = effects_merged[.type == "pdp", ..sf]
      res.ice = effects_merged[.type == "ice", ..sf]

      q = qnorm(1 - alpha / 2)
      res.pdp$lower = res.pdp$mean - q * res.pdp$sd
      res.pdp$upper = res.pdp$mean + q * res.pdp$sd 

      # Get the ground-truth
      gt.pdp = res$gt.pdp
      gt.ice = res$gt.ice

      if (plot) {
        p = ggplot(data = gt.pdp, aes_string(x = feature, y = "mean")) + geom_line(colour = "blue")
        p = p + geom_ribbon(data = res.pdp, aes_string(x = feature, ymin = "lower", ymax = "upper"), alpha = 0.2) + geom_line(data = res.pdp, aes_string(x = feature, y = "mean"))
        p = p + geom_vline(data = optimum, aes_string(xintercept = feature), colour = "orange", lty = 2)
        p
      }

      for (objective in names(res$trees)) {

        tree = res$trees[[objective]]

        source_node = tree[[1]][[1]]
        eval.source_node = get_eval_measures_mlp(res.ice, gt.ice, source_node$subset.idx, feature, optimum[feature])
        names(eval.source_node) = paste0("source.", names(eval.source_node))

        for (depth in seq(2, length(tree))) {
          
          subtree = tree[seq_len(depth)]
          node = find_optimal_node(subtree, optimum)
          eval.opt = get_eval_measures_mlp(res.ice, gt.ice, node$subset.idx, feature, optimum[feature])
          values = cbind(model = i, objective = objective, feature = feature, depth = depth, depth.actual = node$depth + 1, eval.opt, eval.source_node)
     
          if (is.null(df)) {
            df = values
          } else {
            df = rbind(df, values)
          }
        }
      }
    }
  }

  df$conf.rel = (df$source.conf.diff - df$conf.diff) / df$source.conf.diff
  df$gt.rel = (df$source.gt.diff.abs - df$gt.diff.abs) / df$source.gt.diff.abs
  df$conf.rel.opt = (df$source.conf.diff.opt - df$conf.diff.opt) / df$source.conf.diff.opt
  df$gt.rel.opt = (df$source.gt.diff.abs.opt - df$gt.diff.abs.opt) / df$source.gt.diff.abs.opt

  return(df)
}


p = ggplot(data = df, aes(x = objective, y = conf.rel)) + geom_boxplot(aes(fill = as.factor(depth))) + facet_grid(. ~ feature)
ggsave(file.path(path, dataset, "conf_diff.png"), p, width = 12, height = 3)

p = ggplot(data = df, aes(x = objective, y = conf.rel.opt)) + geom_boxplot(aes(fill = as.factor(depth))) + facet_grid(. ~ feature)
ggsave(file.path(path, dataset, "conf_opt_diff.png"), p, width = 12, height = 3)


p = ggplot(data = df, aes(x = objective, y = neg_loglik)) + geom_boxplot(aes(fill = as.factor(depth))) + facet_grid(. ~ feature)
ggsave(file.path(path, dataset, "neg_loglik.png"), p, width = 12, height = 3)

p = ggplot(data = setDT(df)[feature != "momentum", ], aes(x = objective, y = gt.rel)) + geom_boxplot(aes(fill = as.factor(depth)))+ facet_grid(. ~ feature)
ggsave(file.path(path, dataset, "gt_rel.png"), p, width = 12, height = 3)













set.seed(123)
testdata =  data$rlhs_test_data[sample(1:3000, 1000),features]


# library(foreach)
# library(doParallel)


for(i in 1:10){
  data.new = mbo_rlhs_data
  data.train = data.new$data.train.mbo[which(data.new$data.train.mbo$extrapol == 1 & data.new$data.train.mbo$iteration == i),1:8]
  optimum = data.train[which.min(data.train$y),1:7]
  for(n.split in c(2,4,6)){
    result = test(n.split, data.new, testdata, surrogate, optimum, i)
    result$n.split = n.split
    result$run = i
    if(i == 1 & n.split == 2){
      result.all = result
    }
    else result.all = rbind(result.all, result)
  }
  
}

saveRDS(result.all, "resultsMLPTest10.rds")













train = function(n.split, data, traindata, testdata, surrogate, optimum, run){
  df = data.frame("id" = NA, "model" = NA, "objective" = NA, "feature" = NA, "par.conf" = NA, "par.gt.abs" = NA, "par.gt.sd" = NA,
                  "opt.conf" = NA, "opt.gt.abs" = NA, "opt.gt.sd" = NA,
                  "par.conf.opt" = NA, "par.gt.abs.opt" = NA, "par.gt.sd.opt" = NA,
                  "opt.conf.opt" = NA, "opt.gt.abs.opt" = NA, "opt.gt.sd.opt" = NA)
  
  id = 1
  grid.size = 20
  objectives = c("SS_L2")
  
  #browser() 
  #for(i in 1:length(data)){
  #testdata = data[[i]]$testset
  features = names(testdata)
  objective = surrogate
  optimum = optimum
  #names(optimum) = features
  
  for(j in 1:length(data$model.list.mbo)){
    model = data$model.list.mbo[[j]][[run]]
    
    for(k in objectives){
      
      for(f in features) {
        # calculate initial ice curves and pdp and groundtruth
        res = marginal_effect_sd_over_mean(model, f, testdata, grid.size, "pdp_sd", alpha = 0.05, correction = NULL)
        res.pdp = res$pdp
        res.ice = res$ice
        q = qnorm(1 - 0.05 / 2)
        res.pdp$lower = res.pdp$mean - q * res.pdp$sd
        res.pdp$upper = res.pdp$mean + q * res.pdp$sd 
        data.groundtruth = testdata
        data.groundtruth$batch_size = 2^data.groundtruth$batch_size
        data.groundtruth$max_units = 2^data.groundtruth$max_units
        
        gt = predicted_marginal_effect(surrogate, f, data.groundtruth, grid.size)
        gt.pdp = gt$pdp
        gt.ice = gt$ice
        if (f %in% c("batch_size", "max_units")) {
          gt.pdp[f] = log(gt.pdp[f], 2)
          gt.ice[f] = log(gt.ice[f], 2)
        }
        gt.pdp$mean = (100-gt.pdp$mean)/100
        gt.ice$.value = (100-gt.ice$.value)/100
        
        
        
        
        
        tree = compute_tree(model, traindata, f, objective = k, n.split = n.split, grid.size = grid.size, addMean = FALSE) # n.splits anpassen
        
        
        # parent node
        idx = 1:nrow(testdata)
        eval.par = get_eval_measures_mlp(res.ice, gt.ice, idx, f, optimum[f])
        
        # node with optimum
        split.criteria = find_split_criteria(tree, optimum)
        subset = find_optimal_subset(testdata, split.criteria)
        eval.opt = get_eval_measures_mlp(res.ice, gt.ice, subset$id, f, optimum[f])
        
        df = rbind(df, data.frame("id" = id, "model" = names(data$model.list.mbo)[j], "objective" = k, "feature" = f, 
                                  "par.conf" = eval.par$conf.diff, "par.gt.abs" = eval.par$gt.diff.abs, "par.gt.sd" = eval.par$gt.diff.sd,
                                  "opt.conf" = eval.opt$conf.diff, "opt.gt.abs" = eval.opt$gt.diff.abs, "opt.gt.sd" = eval.opt$gt.diff.sd,
                                  "par.conf.opt" = eval.par$conf.diff.opt, "par.gt.abs.opt" = eval.par$gt.diff.abs.opt, "par.gt.sd.opt" = eval.par$gt.diff.sd.opt,
                                  "opt.conf.opt" = eval.opt$conf.diff.opt, "opt.gt.abs.opt" = eval.opt$gt.diff.abs.opt, "opt.gt.sd.opt" = eval.opt$gt.diff.sd.opt))
        
        id = id + 1
      }
      
      
      
    }
  }
  
  return(df)
}


for(i in 1:10){
  data.new = mbo_rlhs_data
  data.train = data.new$data.train.mbo[which(data.new$data.train.mbo$extrapol == 1 & data.new$data.train.mbo$iteration == i),1:8]
  optimum = data.train[which.min(data.train$y),1:7]
  for(n.split in c(2,4,6,8)){
    result = train(n.split, data.new, data.train, testdata, surrogate, optimum, i)
    result$n.split = n.split
    result$run = i
    if(i == 1 & n.split == 2){
      result.all = result
    }
    else result.all = rbind(result.all, result)
  }
  
}

saveRDS(result.all, "resultsMLPTrain10.rds")




############################
#improvements from parent node to final node containing optimum

# for entire x_s
res$conf.diff = res$par.conf-res$opt.conf
res$gt.abs.diff = res$par.gt.abs-res$opt.gt.abs
res$gt.sd.diff = res$par.gt.sd-res$opt.gt.sd

# for area around optimum in x_s
res$conf.diff.opt = res$par.conf.opt-res$opt.conf.opt
res$gt.abs.diff.opt = res$par.gt.abs.opt-res$opt.gt.abs.opt
res$gt.sd.diff.opt = res$par.gt.sd.opt-res$opt.gt.sd.opt

# precentage improvement
res$conf.rel = res$conf.diff/res$par.conf
res$gt.rel = res$gt.abs.diff/res$par.gt.abs
res$conf.rel.opt = res$conf.diff.opt/res$par.conf.opt
res$gt.rel.opt = res$gt.abs.diff.opt/res$par.gt.abs.opt



#write.csv2(res, "resultsMLP3.csv")

nsplits = c(2,4,6)
resultsEval = res
resultsEval = resultsEval[!is.na(resultsEval$model),]
target = "conf.rel"

plist = lapply(nsplits, function(n.split) {
  result = resultsEval[which(resultsEval$n.split == n.split),]
  ggplot(data = result, aes(x = feature, y = result[,target], fill = objective)) + geom_boxplot() +
    ggtitle(paste0("nsplit = ", n.split)) + ylim(-0, 0.6)
})
do.call(grid.arrange, c(plist, nrow = 1))












# test splitting
l = "1"
i = 1
feat = "momentum"


model = mbo_rlhs_data$model.list.mbo[[l]][[i]]
trainingdata = mbo_rlhs_data$data.train.mbo[which(mbo_rlhs_data$data.train.mbo$extrapol==1&mbo_rlhs_data$data.train.mbo$iteration ==1),1:7]
df = trainingdata

mymodel = makeS3Obj("mymodel", fun = function() return(model))
predict.mymodel = function(object, newdata) {
  pred = predict(object$fun(), newdata = newdata)
  pp = getPredictionSE(pred)
  return(pp)
}

predictor = Predictor$new(model = model, data = df)
predictor = Predictor$new(model = mymodel, data = df, predict.function = predict.mymodel)
effect = FeatureEffect$new(predictor = predictor, feature = feat, method = "ice")
res = marginal_effect_sd_over_mean(model, feat, df, 20, "pdp_sd", alpha = 0.05, correction = NULL)
q = qnorm(1 - 0.05 / 2)
res$lower = res$mean - q * res$sd
res$upper = res$mean + q * res$sd 
data.groundtruth = df
data.groundtruth$batch_size = 2^data.groundtruth$batch_size
data.groundtruth$max_units = 2^data.groundtruth$max_units

gt = predicted_marginal_effect(surrogate, feat, df, 20)
if (pdp.feature %in% c("batch_size", "max_units")) {
  pp.gt[pdp.feature] = log(pp.gt[pdp.feature], 2)
}
gt$mean = (100-gt$mean)/100


p = ggplot() + geom_ribbon(data = res, aes_string(x = "momentum", ymin = "lower", ymax = "upper"), alpha = 0.2)
p = p + geom_line(data = res, aes_string(x = "momentum", y = "mean"), colour = "blue") 
p + geom_line(data = gt, aes(momentum, mean))

# define objective
split.objective = function(y, x, requires.x = FALSE, ...) {
  
  row_means = rowMeans(y) # area of individual ice curves
  ypred = mean(row_means) # area of pdp
  sum((row_means - ypred)^2)
} 


# Compute tree
trainingdata = mbo_rlhs_data$data.train.mbo[which(mbo_rlhs_data$data.train.mbo$extrapol==1 & mbo_rlhs_data$data.train.mbo$iteration==1),1:8]
optimum = trainingdata[which.min(trainingdata$y),1:7]
plot(effect)

tree = compute_tree(model, trainingdata, feat, objective = "SS_L2", n.split = 9, grid.size = 20, addMean = FALSE)
node.idx = find_optimal_node(tree, optimum)$subset.idx
eff = effect$results[effect$results$.id %in% node.idx,]
plot(effect)
ggplot(eff, aes(x = momentum, y = .value, group = .id)) + geom_line()
split.criteria = find_split_criteria(tree, optimum = optimum)
testset = find_optimal_subset(testdata, split.criteria)
predictor = Predictor$new(model = mymodel, data = testdata, predict.function = predict.mymodel)
effect = FeatureEffect$new(predictor = predictor, feature = feat, method = "ice")
eff = effect$results[effect$results$.id %in% testset$id,]

lapply(seq_len(length(tree) - 1), function(depth) {
  nodes = tree[[depth]]
  plist = lapply(nodes, function(node) {
    plot_pdp_for_node_mlp(effect, node, model, feat, objective.groundtruth = surrogate, method = "pdp_sd") + ylim(0.23,0.32)
  })
  do.call(grid.arrange, c(plist, nrow = 1))
})



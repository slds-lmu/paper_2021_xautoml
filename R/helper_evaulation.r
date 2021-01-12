

get_ice_curves <- function(model, data, feature, grid.size, mean = FALSE){
  mymodel = makeS3Obj("mymodel", fun = function() return(model))
  predict.mymodel = function(object, newdata) {
    pred = predict(object$fun(), newdata = newdata)
    pp = getPredictionSE(pred)
    if(mean == TRUE) pp = getPredictionResponse(pred) - 2*pp
    return(pp)
  }
  
  predictor = Predictor$new(model = mymodel, data = data, predict.function = predict.mymodel)
  effect = FeatureEffect$new(predictor = predictor, feature = feature, grid.size = grid.size, method = "ice")
}


# define objective
SS_L2 = function(y, x, requires.x = FALSE, ...) {
  require(Rfast)
  ypred = colMeans(as.matrix(y))
  sum(t((t(y) - ypred)^2))
  
}

# Using the area
SS_area = function(y, x, requires.x = FALSE, ...) {
  row_means = rowMeans(y) # area of individual ice curves
  ypred = mean(row_means) # area of pdp
  sum((row_means - ypred)^2)
}



get_eval_measures = function(effect, node, model, pdp.feature, optimum, grid.size, objective.groundtruth = NULL, method = "pdp_var_gp") {
  data = effect$predictor$data$X[node$subset.idx, ]
  data = as.data.frame(data)
  pp = marginal_effect_sd_over_mean(model = model, feature = pdp.feature, data = data, grid.size = grid.size, method = method)
  pp$lower = pp$mean - 2 * pp$sd
  pp$upper = pp$mean + 2 * pp$sd
  
  pp.gt = marginal_effect(objective.groundtruth, pdp.feature, data, model, grid.size)
  
  conf.diff = sum(pp$upper-pp$lower)
  gt.diff.abs = sum(abs(pp.gt$mean-pp$mean))
  gt.diff.sd = sd(pp.gt$mean-pp$mean)
  
  # values around optimum
  pp["dist.opt"] = abs(pp[,pdp.feature]-optimum)
  pp.opt = pp[order(pp$dist.opt),][1,] # adjust number of grid points to evaluate?
  
  conf.diff.opt = sum(pp.opt$upper-pp.opt$lower)
  gt.diff.abs.opt = sum(abs(pp.gt$mean[which(pp.opt[,pdp.feature] %in% pp.gt[,pdp.feature])]-pp.opt$mean))
  gt.diff.sd.opt = sd(pp.gt$mean[which(pp.opt[,pdp.feature] %in% pp.gt[,pdp.feature])]-pp.opt$mean)
  
  return(list("conf.diff" = conf.diff, "gt.diff.abs" = gt.diff.abs, "gt.diff.sd" = gt.diff.sd,
              "conf.diff.opt" = conf.diff.opt, "gt.diff.abs.opt" = gt.diff.abs.opt, "gt.diff.sd.opt" = gt.diff.sd.opt))
}

find_optimal_node = function(tree, optimum){
  node = tree[[1]][[1]]
  id = c()
  
  for(depth in seq_len(length(tree) - 1)){
    
    
    
    split.feature = node$split.feature
    split.value = node$split.value
    if(optimum[split.feature] <= split.value){
      id = c(id, 1)
    }
    else id = c(id,2)
    node = tree[[depth+1]]
    for(i in id){
      if(i == 1) node = node[1:(0.5*length(node))]
      else node = node[(0.5*length(node)+1):length(node)]
      
    }
    node = node[[1]]
    
  }
  
  
  return(node)
}




# mlp separate until surrogate data problem fixed
get_eval_measures_mlp = function(effect, node, model, pdp.feature, optimum, grid.size, objective.groundtruth = NULL, method = "pdp_var_gp") {
  data = effect$predictor$data$X[node$subset.idx, ]
  data = as.data.frame(data)
  pp = marginal_effect_sd_over_mean(model = model, feature = pdp.feature, data = data, grid.size = grid.size, method = method)
  pp$lower = pp$mean - 2 * pp$sd
  pp$upper = pp$mean + 2 * pp$sd
  
  
  data.groundtruth = data
  data.groundtruth$batch_size = 2^data.groundtruth$batch_size
  data.groundtruth$max_units = 2^data.groundtruth$max_units
  
  pp.gt = predicted_marginal_effect(objective.groundtruth, pdp.feature, data.groundtruth, grid.size)
  if (pdp.feature %in% c("batch_size", "max_units")) {
    pp.gt[pdp.feature] = log(pp.gt[pdp.feature], 2)
  }
  pp.gt$mean = (100-pp.gt$mean)/100
  
  
  conf.diff = sum(pp$upper-pp$lower)
  gt.diff.abs = sum(abs(pp.gt$mean-pp$mean))
  gt.diff.sd = sd(pp.gt$mean-pp$mean)
  
  # values around optimum
  pp["dist.opt"] = abs(pp[,pdp.feature]-optimum)
  pp.opt = pp[order(pp$dist.opt),][1,] # adjust number of grid points to evaluate?
  
  conf.diff.opt = sum(pp.opt$upper-pp.opt$lower)
  gt.diff.abs.opt = sum(abs(pp.gt$mean[which(pp.opt[,pdp.feature] %in% pp.gt[,pdp.feature])]-pp.opt$mean))
  gt.diff.sd.opt = sd(pp.gt$mean[which(pp.opt[,pdp.feature] %in% pp.gt[,pdp.feature])]-pp.opt$mean)
  
  return(list("conf.diff" = conf.diff, "gt.diff.abs" = gt.diff.abs, "gt.diff.sd" = gt.diff.sd,
              "conf.diff.opt" = conf.diff.opt, "gt.diff.abs.opt" = gt.diff.abs.opt, "gt.diff.sd.opt" = gt.diff.sd.opt))
}

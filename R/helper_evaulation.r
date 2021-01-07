

get_ice_curves <- function(model, data, feature, mean = FALSE){
  mymodel = makeS3Obj("mymodel", fun = function() return(model))
  predict.mymodel = function(object, newdata) {
    pred = predict(object$fun(), newdata = newdata)
    pp = getPredictionSE(pred)
    if(mean == TRUE) pp = getPredictionResponse(pred) - 2*pp
    return(pp)
  }
  
  predictor = Predictor$new(model = mymodel, data = data, predict.function = predict.mymodel)
  effect = FeatureEffect$new(predictor = predictor, feature = feature, method = "ice")
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



get_eval_measures = function(effect, node, model, pdp.feature, objective.groundtruth = NULL, method = "pdp_var_gp") {
  data = effect$predictor$data$X[node$subset.idx, ]
  data = as.data.frame(data)
  pp = marginal_effect_sd_over_mean(model = model, feature = pdp.feature, data = data, method = method)
  pp$lower = pp$mean - 2 * pp$sd
  pp$upper = pp$mean + 2 * pp$sd
  
  pp.gt = marginal_effect(objective.groundtruth, pdp.feature, data)
  
  conf.diff = sum(pp$upper-pp$lower)
  gt.diff.abs = sum(abs(pp.gt$mean-pp$mean))
  gt.diff.sd = sd(pp.gt$mean-pp$mean)
  
  
  return(list("conf.diff" = conf.diff, "gt.diff.abs" = gt.diff.abs, "gt.diff.sd" = gt.diff.sd))
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


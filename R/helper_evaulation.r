

get_ice_curves <- function(model, data, feature, grid.size, mean = FALSE){
  mymodel = makeS3Obj("mymodel", fun = function() return(model))
  predict.mymodel = function(object, newdata) {
    pred = predict(object$fun(), newdata = newdata)
    pp = getPredictionSE(pred)
    if(mean == TRUE) pp = getPredictionResponse(pred) 
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

  max_depth = length(tree) - 1
  depth = 0

  while(node$depth < max_depth) {

    split.feature = node$split.feature
    split.value = node$split.value
    
    if(optimum[split.feature] <= split.value){
      node = node$children$left.child
    } else {
      node = node$children$right.child
    }
  }

  # id = c()
  
  # for(depth in seq_len(length(tree) - 1)){
    
  #   split.feature = node$split.feature
  #   split.value = node$split.value
  #   if(optimum[split.feature] <= split.value){
  #     id = c(id, 1)
  #   }
  #   else id = c(id,2)
  #   node = tree[[depth+1]]
  #   for(i in id){
  #     if(i == 1) node = node[1:(0.5*length(node))]
  #     else node = node[(0.5*length(node)+1):length(node)]
      
  #   }
  #   node = node[[1]]
    
  # }
  
  return(node)
}


find_split_criteria = function(tree, optimum){
  node = tree[[1]][[1]]
  id = c()
  features = c()
  values = c()
  
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
    features = c(features, split.feature)
    values = c(values, split.value)
    
  }
  
  
  return(list("features" = features, "values" = values, "id" = id))
}

find_optimal_subset = function(testdata, split.criteria){
  testdata$id = 1:nrow(testdata)
  for(i in length(split.criteria$features)){
    if(split.criteria$id[i] == 1){
      testdata = testdata[which(testdata[,split.criteria$features[i]] <= split.criteria$values[i]),]
    }
    else{
      testdata = testdata[which(testdata[,split.criteria$features[i]] > split.criteria$values[i]),]
    }
    
  }
  return(testdata)
}




# mlp separate until surrogate data problem fixed
get_eval_measures_mlp = function(res.ice, gt.ice, idx, pdp.feature, optimum, method = "pdp_var_sd") {
  
  res.ice = res.ice[which(res.ice$.id %in% idx),]

  # Attention with computation of the sd - this needs to be adapted
  if (method == "pdp_sd") {
    res.pdp = setDT(res.ice)[, .(mean = mean(mean), sd = mean(sd)), by = pdp.feature] 
  } else {
    res.pdp = setDT(res.ice)[, .(mean = mean(mean), sd = sqrt(mean(sd^2))), by = pdp.feature]     
  }

  gt.ice = gt.ice[which(gt.ice$.id %in% idx),]
  # Attention with computation of the sd - this needs to be adapted
  res.gt = setDT(gt.ice)[, .(mean = mean(mean)), by = pdp.feature] 

  q = qnorm(1 - 0.05 / 2)
  res.pdp$lower = res.pdp$mean - q * res.pdp$sd
  res.pdp$upper = res.pdp$mean + q * res.pdp$sd 
  
  pp = res.pdp
  pp.gt = res.gt

  neg_loglik = sum(unlist(lapply(seq_row(pp), function(i) {
      - dnorm(pp.gt[i, ]$mean, mean = pp[i, ]$mean, sd = pp[i, ]$sd, log = TRUE) 
  })))
  
  conf.diff = sum(pp$upper - pp$lower)
  gt.diff.abs = sum(abs(pp.gt$mean - pp$mean))
  gt.diff.sd = sd(pp.gt$mean - pp$mean)
  
  # values at the optimum
  pp_dist_opt = abs(pp[, ..pdp.feature] - as.numeric(optimum[, pdp.feature]))
  best_idx = order(pp_dist_opt)[1] # adjust number of grid points to evaluate?

  pp.opt = pp[best_idx, ] 
  pp.gt.opt = pp.gt[best_idx, ]

  conf.diff.opt = sum(pp.opt$upper - pp.opt$lower)
  gt.diff.abs.opt = sum(abs(pp.gt.opt$mean - pp.opt$mean))
  gt.diff.sd.opt = sd(pp.gt.opt$mean - pp.opt$mean)
    
  return(data.frame("conf.diff" = conf.diff, "gt.diff.abs" = gt.diff.abs, "gt.diff.sd" = gt.diff.sd,
              "conf.diff.opt" = conf.diff.opt, "gt.diff.abs.opt" = gt.diff.abs.opt, "gt.diff.sd.opt" = gt.diff.sd.opt, neg_loglik = neg_loglik))
}



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
  
  conf.diff = sum(pp$upper - pp$lower)
  gt.diff.abs = sum(abs(pp.gt$mean - pp$mean))
  gt.diff.sd = sd(pp.gt$mean - pp$mean)
  
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

  max_depth = length(tree) 

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

  if (method == "pdp_sd") {
    res.pdp = setDT(res.ice)[, .(mean = mean(mean), sd = mean(sd)), by = pdp.feature] 
  } else {
    res.pdp = setDT(res.ice)[, .(mean = mean(mean), sd = sqrt(mean(sd^2))), by = pdp.feature]     
  }

  gt.ice = gt.ice[which(gt.ice$.id %in% idx),]
  res.gt = setDT(gt.ice)[, .(mean = mean(mean)), by = pdp.feature] 

  pp = res.pdp
  pp.gt = res.gt

  neg_loglik = mean(unlist(lapply(seq_row(pp), function(i) {
      - dnorm(pp.gt[i, ]$mean, mean = pp[i, ]$mean, sd = pp[i, ]$sd, log = TRUE) 
  })))
  
  conf.diff = sum(pp$sd)
  gt.diff.abs = sum(abs(pp.gt$mean - pp$mean))
  gt.diff.sd = sd(pp.gt$mean - pp$mean)
  gt.diff.abs.sd = sd(abs(pp.gt$mean - pp$mean))

  df = data.frame(conf.diff, gt.diff.abs, gt.diff.sd, gt.diff.abs.sd, neg_loglik)
  
  # values at the optimum
  pp_dist_opt = abs(pp[, ..pdp.feature] - as.numeric(optimum[, pdp.feature]))

  df.opt = lapply(1:3, function(nn) {
    best_idx = order(pp_dist_opt)[seq_len(nn)] # adjust number of grid points to evaluate?

    pp.opt = pp[best_idx, ] 
    pp.gt.opt = pp.gt[best_idx, ]

    conf.diff.opt = sum(pp.opt$sd)
    gt.diff.abs.opt = sum(abs(pp.gt.opt$mean - pp.opt$mean))

    df.opt = data.frame(conf.diff.opt, gt.diff.abs.opt)
    names(df.opt) = paste0(names(df.opt), ".", nn)

    df.opt
  })

  df.opt = do.call(cbind, df.opt)

  df = cbind(df, df.opt)
    
  return(df)
}




evaluate_results = function(reslist, optima, gtdata) {
  
  df = data.frame()

  # Iterate over all models we have 
  for (i in seq_along(reslist)) {

    resmod = reslist[[i]]
    optimum = optima[i, ]

    for (feature in names(resmod)) {

      res = resmod[[feature]]

      res.pdp = res$res.pdp
      res.ice = res$res.ice

      # Get the ground-truth
      gt.pdp = setDT(gtdata[[feature]][[1]])[.type == "pdp", ]
      gt.ice = setDT(gtdata[[feature]][[1]])[.type == "ice", ]

      tree = res$trees[[1]]

      source_node = tree[[1]][[1]]

      eval.source_node = get_eval_measures_mlp(res.ice, gt.ice, source_node$subset.idx, feature, optimum[feature])   

      names(eval.source_node) = paste0("source.", names(eval.source_node))

      for (depth in seq(2, length(tree))) {
        
        subtree = tree[seq_len(depth)]
        node = find_optimal_node(subtree, optimum)

        eval.opt = get_eval_measures_mlp(res.ice, gt.ice, node$subset.idx, feature, optimum[feature])

        values = cbind(model = i, feature = feature, depth = depth, eval.opt, eval.source_node)

        if (is.null(df)) {
          df = values
        } else {
          df = rbind(df, values)
        }
      }
    }
  }

  df$conf.rel = (df$source.conf.diff - df$conf.diff) / df$source.conf.diff
  df$gt.rel = (df$source.gt.diff.abs - df$gt.diff.abs) / df$source.gt.diff.abs
  df$gt.abs = df$source.gt.diff.abs - df$gt.diff.abs

  for (nn in 1:3) {
    df[, paste0("conf.rel.opt.", nn)] = (df[, paste0("source.conf.diff.opt.", nn)] - df[, paste0("conf.diff.opt.", nn)]) / df[, paste0("source.conf.diff.opt.", nn)]
    df[, paste0("gt.abs.opt.", nn)] = (df[, paste0("source.gt.diff.abs.opt.", nn)] - df[, paste0("gt.diff.abs.opt.", nn)]) 
  }

  df$neg_loglik.rel = (df$source.neg_loglik - df$neg_loglik) / df$source.neg_loglik

  return(df)
}


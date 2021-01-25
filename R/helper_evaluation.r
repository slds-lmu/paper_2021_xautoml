

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

    if (!is.null(split.value) & !is.null(split.value) & !is.null(optimum[split.feature])) {
      if(optimum[split.feature] <= split.value){
        node = node$children$left.child
      } else {
        node = node$children$right.child
      }
    } else {
      break
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
get_eval_measures_mlp = function(res.ice, gt.ice, testdata, idx, pdp.feature, optimum, method = "pdp_var_sd", model) {
  
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

  if (!is.null(model)) {

    res.pdp.cov = get_gp_uncertainty(res.pdp, model, pdp.feature, testdata[idx, ])

    # Also get the data with regards to the other uncertainty estimate   
    conf.diff.cov = sum(res.pdp.cov$sd)
    gt.diff.abs.cov = sum(abs(pp.gt$mean - res.pdp.cov$mean))
    gt.diff.sd.cov = sd(pp.gt$mean - res.pdp.cov$mean)
    gt.diff.abs.sd.cov = sd(abs(pp.gt$mean - res.pdp.cov$mean))

    neg_loglik.cov = mean(unlist(lapply(seq_row(res.pdp.cov), function(i) {
        - dnorm(pp.gt[i, ]$mean, mean = res.pdp.cov[i, ]$mean, sd = res.pdp.cov[i, ]$sd, log = TRUE) 
    })))

    df = cbind(df, data.frame(conf.diff.cov, gt.diff.abs.cov, gt.diff.sd.cov, gt.diff.abs.sd.cov, neg_loglik.cov))
  }

  
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




evaluate_results = function(reslist, optima, gtdata, testdata, models = NULL) {
  
  df = data.frame()

  # Iterate over all models we have 
  for (i in seq_along(reslist)) {

    print(paste0("Iteration number ", i, "/", length(reslist)))

    resmod = reslist[[i]]
    optimum = optima[i, ]

    if (!is.null(models)) {
      model = models[[i]]
    } else {
      model = NULL
    }


    st = Sys.time()

    for (feature in names(resmod)) {

      res = resmod[[feature]]

      res.pdp = res$res.pdp
      res.ice = res$res.ice

      # Get the ground-truth
      gt.pdp = setDT(gtdata[[feature]][[1]])[.type == "pdp", ]
      gt.ice = setDT(gtdata[[feature]][[1]])[.type == "ice", ]

      tree = res$trees[[1]]

      source_node = tree[[1]][[1]]

      eval.source_node = get_eval_measures_mlp(
        res.ice = res.ice, 
        gt.ice = gt.ice, 
        testdata = testdata, 
        idx = source_node$subset.idx, 
        pdp.feature = feature, optimum = optimum[feature], model = model)   

      names(eval.source_node) = paste0("source.", names(eval.source_node))

      for (depth in seq(2, length(tree))) {
        
        subtree = tree[seq_len(depth)]
        node = find_optimal_node(subtree, optimum)

        eval.opt = get_eval_measures_mlp(
          res.ice = res.ice, 
          gt.ice = gt.ice, 
          testdata = testdata, 
          idx = node$subset.idx, 
          pdp.feature = feature, optimum = optimum[feature], model = model)   

        values = cbind(model = i, feature = feature, depth = depth, eval.opt, eval.source_node)

        if (is.null(df)) {
          df = values
        } else {
          df = rbind(df, values)
        }
      }
    }

    et = Sys.time()
    print(et - st)

  }

  df$conf.rel = (df$source.conf.diff - df$conf.diff) / df$source.conf.diff
  df$gt.rel = (df$source.gt.diff.abs - df$gt.diff.abs) / df$source.gt.diff.abs
  df$gt.abs = df$source.gt.diff.abs - df$gt.diff.abs

  df$conf.rel.cov = (df$source.conf.diff.cov - df$conf.diff.cov) / df$source.conf.diff.cov
  df$gt.rel.cov = (df$source.gt.diff.abs.cov - df$gt.diff.abs.cov) / df$source.gt.diff.abs.cov
  df$gt.abs.cov = df$source.gt.diff.abs.cov - df$gt.diff.abs.cov

  for (nn in 1:3) {
    df[, paste0("conf.rel.opt.", nn)] = (df[, paste0("source.conf.diff.opt.", nn)] - df[, paste0("conf.diff.opt.", nn)]) / df[, paste0("source.conf.diff.opt.", nn)]
    df[, paste0("gt.abs.opt.", nn)] = (df[, paste0("source.gt.diff.abs.opt.", nn)] - df[, paste0("gt.diff.abs.opt.", nn)]) 
  }

  df$neg_loglik.rel = (df$source.neg_loglik - df$neg_loglik) / df$source.neg_loglik

  return(df)
}



get_gp_uncertainty = function(res.pdp, model, feature, testdata) {
  
  gridvalues = res.pdp[, ..feature]

  # Extract the learned GP 
  km = model$learner.model

  # Covtype is needed later to extract the covariance 
  covtype = attr(km, "covariance")

  res = lapply(seq_row(gridvalues), function(i) {

    gv = as.vector(gridvalues[i, ])

    # Create vector along the gridvalue gv by combining it with the "test dataset"
    gg = testdata
    gg[, feature] = gv

    # Compute the posterior mean and covariance of the predictions at points in gg
    pred = predict(object = km, newdata = gg, type = "SK", cov.compute = TRUE)
    C = pred$cov
    m = pred$mean 

    # To boil everything down we have to compute now the mean of this vector, as well as the 
    # variance over the mean
    mean_pdp = mean(m)
    sd_pdp = 1 / nrow(gg) * sqrt(sum(C)) # see Wikipedia "Variance#Sum_of_correlated_variables#Matrix notation" 

    df = data.frame(gv, mean = mean_pdp, sd = sd_pdp)
    names(df)[1] = feature

    return(df)
  })

  res = do.call(rbind, res)

  return(res)

}


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
get_eval_measures_mlp = function(res.ice, gt.ice, idx, pdp.feature, optimum, method = "pdp_var_sd", plot = FALSE) {
  
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

  p = NULL

  if (plot) {
    p = ggplot(data = res.gt, aes_string(x = pdp.feature, y = "mean")) + geom_line(colour = "blue")
    p = p + geom_ribbon(data = res.pdp, aes_string(x = pdp.feature, ymin = "lower", ymax = "upper"), alpha = 0.2) + geom_line(data = res.pdp, aes_string(x = pdp.feature, y = "mean"))
    p = p + geom_vline(data = optimum, aes_string(xintercept = pdp.feature), colour = "orange", lty = 2)
  }

  
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
    
  return(list(df = data.frame("conf.diff" = conf.diff, "gt.diff.abs" = gt.diff.abs, "gt.diff.sd" = gt.diff.sd,
              "conf.diff.opt" = conf.diff.opt, "gt.diff.abs.opt" = gt.diff.abs.opt, "gt.diff.sd.opt" = gt.diff.sd.opt, neg_loglik = neg_loglik), p = p))
}


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


evaluate_results = function(reslist, plotpath = NULL, alpha = 0.05, optima) {
  
  df = data.frame()

  if (!is.null(plotpath))
    plot = TRUE

  # Iterate over all models we have 
  for (i in seq_along(reslist)) {

    resmod = reslist[[i]]
    optimum = optima[[i]]

    for (feature in names(resmod)) {

      res = reslist[[i]][[feature]]

      effects_merged = res$effects

      sf = c(feature, "mean", "sd", ".id")
      res.pdp = effects_merged[.type == "pdp", ..sf]
      res.ice = effects_merged[.type == "ice", ..sf]

      q = qnorm(1 - alpha / 2)
      res.pdp$lower = res.pdp$mean - q * res.pdp$sd
      res.pdp$upper = res.pdp$mean + q * res.pdp$sd 

      ymax = max(res.pdp$upper) + 0.01
      ymin = min(res.pdp$lower) - 0.01

      # Get the ground-truth
      gt.pdp = res$gt.pdp
      gt.ice = res$gt.ice

      for (objective in names(res$trees)) {

        tree = res$trees[[objective]]

        source_node = tree[[1]][[1]]
        eval.source_node = get_eval_measures_mlp(res.ice, gt.ice, source_node$subset.idx, feature, optimum[feature], plot = TRUE)
        
        if (plot) {
          plist = list()
          plist[[1]] = eval.source_node$p + ylim(c(ymin, ymax))
        }

        names(eval.source_node$df) = paste0("source.", names(eval.source_node$df))

        for (depth in seq(2, length(tree))) {
          
          subtree = tree[seq_len(depth)]
          node = find_optimal_node(subtree, optimum)
          eval.opt = get_eval_measures_mlp(res.ice, gt.ice, node$subset.idx, feature, optimum[feature], plot = TRUE)

          if (!is.null(plotpath)) {
            plist[[depth]] = eval.opt$p + ylim(c(ymin, ymax))
          }

          values = cbind(model = i, objective = objective, feature = feature, depth = depth, depth.actual = node$depth + 1, eval.opt$df, eval.source_node$df)

          if (is.null(df)) {
            df = values
          } else {
            df = rbind(df, values)
          }
        }

        if(!exists(file.path(plotpath, "individual_pdps")))
          dir.create(file.path(plotpath, "individual_pdps"))

        if(!exists(file.path(plotpath, "individual_pdps", objective)))
          dir.create(file.path(plotpath, "individual_pdps", objective))

        if(!exists(file.path(plotpath, "individual_pdps", objective, i)))
          dir.create(file.path(plotpath, "individual_pdps", objective, i))

        ggsave(file.path(plotpath, "individual_pdps", objective, i, paste0(feature, ".png")), do.call(grid.arrange, c(plist[c(1, length(plist) - 1)], nrow = 1)), width = 8, height = 4)
      }
    }
  }

  df$conf.rel = (df$source.conf.diff - df$conf.diff) / df$source.conf.diff
  df$gt.rel = (df$source.gt.diff.abs - df$gt.diff.abs) / df$source.gt.diff.abs
  df$conf.rel.opt = (df$source.conf.diff.opt - df$conf.diff.opt) / df$source.conf.diff.opt
  df$gt.rel.opt = (df$source.gt.diff.abs.opt - df$gt.diff.abs.opt) / df$source.gt.diff.abs.opt

  return(df)
}


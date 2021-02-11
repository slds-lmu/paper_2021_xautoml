library("plot3D")
library("ggplot2")
library("GGally")
library("gridExtra")
library("dplyr")


# create table for relative improvement on dataset level (HPO on DL)
create_table_datasets = function(data.list, objective, target, depth){
  # data.list: list with evaluation data for each dataset
  # objective: character string with name of objective e.g. "SS_L2"
  # target:    name of column of evaluation data to be aggregated e.g. "conf.rel"
  # depth:     tree depth 
  
  # output:    table with results aggregated on dataset level 
  
  df = lapply(1:length(data.list), function(i){
    
    data = data.list[[i]]
    data = data[which(data$objective == objective & data$depth == depth),]
    if(target == "neg_loglik.rel"){
      data$neg_loglik.rel = (data$source.neg_loglik - data$neg_loglik)/abs(data$source.neg_loglik)
    }
    if(target == "gt.diff.sd"){
      data$gt.diff.sd = (data$source.gt.diff.sd - data$gt.diff.sd)/data$source.gt.diff.sd
    }
    data.features = data %>% group_by(feature) %>% summarise(mean = mean(get(target)))
    data.frame("dataset" = names(data.list)[i], "mean" = mean(data[,target]), "sd" = sd(data[,target]),
               "best.feat" = data.features[which.max(data.features$mean), "feature"], "best.mean" = data.features[which.max(data.features$mean), "mean"],
               "worst.feat" = data.features[which.min(data.features$mean), "feature"], "worst.mean" = data.features[which.min(data.features$mean), "mean"])
    
  })
  return(do.call("rbind", df))
}

# create table for relative improvement on hyperparameter level (HPO on DL)
create_table_features = function(data.list, objective, target, depth){
  # data.list: list with evaluation data for each dataset
  # objective: character string with name of objective e.g. "SS_L2"
  # target:    name of column of evaluation data to be aggregated e.g. "conf.rel"
  # depth:     tree depth 
  
  # output:    table with results aggregated on hyperparameter level 
  
  df = lapply(1:length(data.list), function(i){
    
    data = data.list[[i]]
    data = data[which(data$objective == objective & data$depth == depth),]
    if(target == "neg_loglik.rel"){
      data$neg_loglik.rel = (data$source.neg_loglik - data$neg_loglik)/abs(data$source.neg_loglik)
    }
    if(target == "neg_loglik"){
      data$neg_loglik = (data$source.neg_loglik - data$neg_loglik)
    }
    if(target == "gt.diff.sd"){
      data$gt.diff.sd = (data$source.gt.diff.sd - data$gt.diff.sd)/data$source.gt.diff.sd
    }
    data.features = data %>% group_by(feature) %>% summarise(mean = median(get(target)))
    data.features$dataset = names(data.list)[i]
    data.features
    
  })
  df = do.call("rbind", df)
  df %>% group_by(feature) %>% summarise(mean.feat = mean(mean), sd = sd(mean))
}


# determine optimal node of last split of tree
find_optimal_node = function(tree, optimum){
  # tree:     return value of compute_tree function
  # optimum:  named vector with values of best configuration found during training
  
  # output:   node of final split containing optimum
  
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
  
  return(node)
}


# extract split criteria of tree for node in final split that contains the optimum
find_split_criteria = function(tree, optimum){
  # tree:     return value of compute_tree function
  # optimum:  named vector with values of best configuration found during training
  
  # output:   list that contains split features, split values and direction
  
  
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




plot_function = function(obj) {
  ps = getParamSet(obj)
  x1 <- seq(from = ps$pars$x$lower[1], to = ps$pars$x$upper[1], length.out = 100)
  x2 <- seq(from = ps$pars$x$lower[2], to = ps$pars$x$upper[2], length.out = 100)
  gg <- expand.grid(x1 = x1, x2 = x2)
  gg$y = apply(gg, 1, obj)
  
  p = ggplot() + geom_tile(data = gg, aes(x = x1, y = x2, fill = y))
  p = p + scale_fill_distiller(palette = "Spectral")
  p
}



concatenate_runs = function(runs) {
  opdf = lapply(runs, function(x) {
    df = as.data.table(as.data.frame(x$opt.path))
    names(df)[ncol(df)] = "cb.lambda"
    df$type = attr(x, "type")
    return(df)
  }
  )
  do.call(rbind, opdf)
}

get_type_of_run = function(x) {
  type = attr(x, "type")
  lambda = x$control$infill.crit$params$cb.lambda
  if (type == "MBO")
    type = paste(type, lambda, sep = "_")            
  return(type)    
}

get_types_of_runs = function(runs) {
  types = lapply(runs, get_type_of_run)
  unlist(types)
}

extract_models = function(runs) {
  models = lapply(seq_along(runs), function(i) {
    type = get_type_of_run(runs[[i]])
    models = runs[[i]]$models
    model = models[[length(models)]]
    if (type == "LHS")
      model = models[[1]]
    return(model)
  })
}

extract_performances = function(runs) {
  lapply(seq_along(runs), function(i) {
    performance = runs[[i]]$model.performances
  })
}


predict_on_grid = function(model, grid) {
  pred = predict(model, newdata = grid)$data
  pred$residual = pred$response - grid$y
  pred$abs_error = abs(pred$response - grid$y)
  cbind(grid, pred)
} 

concatenate_runs = function(runs) {
  opdf = lapply(runs, function(x) {
    df = as.data.table(as.data.frame(x$opt.path))
    names(df)[ncol(df)] = "cb.lambda"
    df$type = get_type_of_run(x)
    return(df)
  }
  )
  do.call(rbind, opdf)
}

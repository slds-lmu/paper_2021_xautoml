

create_table_datasets = function(data.list, objective, target, depth){
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


create_table_features = function(data.list, objective, target, depth){
  
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

compute_tree = function(effect, df, objective, n.splits) {
  
  ice_feat = effect$feature.name
  all_feats = effect$predictor$data$feature.names
  split_feats = setdiff(all_feats, ice_feat)
  
  print(split_feats)
  
  effects = data.table(effect$results)
  effects = merge(effects, df[, c(".id", split_feats)], by = ".id")
  
  Y = tidyr::spread(effects, ice_feat, .value)
  Y = as.data.frame(Y)
  Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]
  
  X = df[, split_feats, drop = FALSE]
  split = split_parent_node(Y = Y, X = X, objective = objective, optimizer = find_best_binary_split)
  node_idx = generate_node_index(Y = Y, X = X, result = split)
  
  results = list()
  
  results[[1]] = list(list(splitfeat = split$feature, splitpoint = split$split.points[[1]], idx = node_idx))
  
  for (depth in seq_len(n.splits)) {
    
    results[[depth + 1]] = list()
    
    for (node in results[[depth]]) {
      browser()
      #node = node[[1]]
      effects = as.data.frame(effects)
      
      # Level d, node n
      idx = node$idx 
      splitpoint = node$splitpoint
      splitfeat = node$splitfeat
      
      # Split Left child
      idx_sub = which(effects[, splitfeat] <= splitpoint)
      Y = tidyr::spread(effects[idx_sub, c(ice_feat, ".value", ".type", ".id")], ice_feat, .value)
      Y = as.data.frame(Y)
      Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]
      
      idx_full = seq_len(nrow(df))
      idx_full[idx$index[[1]] ]
      
      X = df[idx$index[[1]], ]
      X = X[, c(split_feats), drop = FALSE]
      
      split = split_parent_node(Y = Y, X = X, objective = objective, optimizer = find_best_binary_split)
      split = split[best.split == TRUE, ]
      
      node_idx = generate_node_index(Y = Y, X = X, result = split)
      
      node_idx$index[[1]] = idx$index[[1]][node_idx$index[[1]]]
      node_idx$index[[2]] = idx$index[[1]][node_idx$index[[2]]]
      
      splitpoint_new = split$split.points[[1]]
      splitfeat_new = split$feature
      
      results[[depth + 1]] = c(results[[depth + 1]], list(splitfeat = splitfeat_new, splitpoint = splitpoint_new, idx = node_idx))
      
      
      
      # Split Right child 
      idx_sub = which(effects[, splitfeat] > splitpoint)
      Y = tidyr::spread(effects[idx_sub, c(ice_feat, ".value", ".type", ".id")], ice_feat, .value)
      Y = as.data.frame(Y)
      Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]
      
      X = df[idx$index[[2]], ]
      X = X[, c(split_feats), drop = FALSE]
      
      split = split_parent_node(Y = Y, X = X, objective = objective, optimizer = find_best_binary_split)
      split = split[best.split == TRUE, ]
      
      node_idx = generate_node_index(Y = Y, X = X, result = split)
      
      node_idx$index[[1]] = idx$index[[2]][node_idx$index[[1]]]
      node_idx$index[[2]] = idx$index[[2]][node_idx$index[[2]]]
      
      splitpoint_new = split$split.points[[1]]
      splitfeat_new = split$feature
      
      results[[depth + 1]] = c(results[[depth + 1]], list(splitfeat = splitfeat_new, splitpoint = splitpoint_new, idx = node_idx))
    }
  }
  
  return(results)
  
}
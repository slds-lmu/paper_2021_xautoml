

# check again if we need this function
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



################################################################################
# helper functions to extract information of optimal node


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



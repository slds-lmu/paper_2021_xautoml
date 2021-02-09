#' @title Performs a single tree based on ICE curves
#'
#' @description
#' Uses functions in customtrees.r for splitting effect curves according to defined objective.
#'
#' @param effect effect object of IML method FeatureEffect$new()
#' @param testdata dataset to use for splitting (data.frame with features in columns)
#' @param objective character string with objective function to use (so far: 'SS_L1', 'SS_L2', 'SS_area', 'SS_sd' and 'var_gp' are defined)
#' @param n.split number of splits to be performed


# Definition of Class Node
Node <- R6Class("Node", list(
    id = NULL,
    
    # on which depth is the node
    depth = NULL,

    # ids of the instances of data that are in this node
    subset.idx = NULL,
    objective.value = NULL, # objective value in a node

    # Parent information
    id.parent = NULL, 
    child.type = NULL, # left or right type

    # Split information (if splitting has already taken place)
    split.feature = NULL,
    split.value = NULL,

    # Append the children of this node
    children = list(),

    stop.criterion.met = FALSE, 

    initialize = function(id, depth = NULL, subset.idx, id.parent = NULL, child.type = NULL) {
      
      assert_numeric(id, len = 1)
      assert_numeric(depth, len = 1, null.ok = TRUE)

      assert_numeric(subset.idx, min.len = 1)
      assert_numeric(id.parent, len = 1, null.ok = TRUE)
      assert_character(child.type, null.ok = TRUE)

      self$id = id
      self$depth = depth
      self$subset.idx = subset.idx
      self$id.parent = id.parent
      self$child.type = child.type

      self$stop.criterion.met = FALSE
    },

    computeSplit = function(X, Y, objective, optimizer, min.split = 10) {
      
      require("customtrees")

      if (length(self$subset.idx) < min.split) {
        self$stop.criterion.met = TRUE
      } else {
        # Just for information purposes
        self$objective.value = objective(y = Y[self$subset.idx, ], x = X[self$subset.idx, ])

        tryCatch({
          split = split_parent_node(Y = Y[self$subset.idx, ], X = X[self$subset.idx, ], objective = objective, optimizer = find_best_binary_split, min.node.size = min.split)
          self$split.feature = split$feature[split$best.split][1]
          self$split.value = unlist(split$split.points[split$best.split])[1]
        }, 
        error = function(cond) {
          # message(paste0("Min.node.size is reached in node ", self$id))
          self$stop.criterion.met = TRUE
        })
      }
    },

    computeChildren = function(X, Y) {

      if (self$stop.criterion.met) {
        # no further split is performed
        self$children = list("left.child" = NULL, "right.child" = NULL)
      } else {
        if(is.null(self$split.feature))
          stop("Please compute the split first via computeSplit().")


        idx.left = which(X[self$subset.idx, self$split.feature, with = FALSE] <= self$split.value)
        idx.right = which(X[self$subset.idx, self$split.feature, with = FALSE] > self$split.value)

        idx.left = self$subset.idx[idx.left]
        idx.right = self$subset.idx[idx.right]

        left.child = Node$new(id = 1, depth = self$depth + 1, subset.idx = idx.left, id.parent = self$id, child.type = "<=")
        right.child = Node$new(id = 2, depth = self$depth + 1, subset.idx = idx.right, id.parent = self$id, child.type = ">")

        self$children = list("left.child" = left.child, "right.child" = right.child)
      }
    }
  )
)



# compute single tree based on Class 'Node' 
compute_tree = function(effect, testdata, objective, n.split) {

  if (objective == "SS_L1") {

    split.objective = function(y, x, requires.x = FALSE, ...) {
      require(Rfast)
      ypred = colMeans(as.matrix(y))
      min(t((t(y) - ypred)^2))    
    } 

    input.data = compute_data_for_ice_splitting(effect, testdata = testdata)
  } 

  else if (objective == "SS_L2") {

    split.objective = function(y, x, requires.x = FALSE, ...) {
      ypred = colMeans(as.matrix(y))
      sum(t((t(y) - ypred)^2))
    } 
    
    input.data = compute_data_for_ice_splitting(effect, testdata = testdata)
  }
  
  else if (objective == "SS_area") {

    split.objective = function(y, x, requires.x = FALSE, ...) {
      row_means = rowMeans(y) # area of individual ice curves
      ypred = mean(row_means) # area of pdp
      sum((row_means - ypred)^2)
    } 
    
    input.data = compute_data_for_ice_splitting(effect, testdata = testdata)

  }
  
  
  else if (objective == "SS_sd") {

    pdp.feat = effect$feature.name
    split.feats = setdiff(names(testdata), pdp.feat)

    # The ys are the predictions (in this case, the standard deviation)
    X = setDT(testdata)
    Y = setDT(effect$predictor$predict(X))
    
    split.objective = function(y, x, requires.x = FALSE, ...) {
      y = y$pred
      sum((y - mean(y))^2)
    } 

    split.feats = setdiff(names(testdata), pdp.feat)
    input.data = list(X = X[, ..split.feats, drop = FALSE], Y = Y)
  }

  else if (objective == "var_gp") {

    pdp.feat = effect$feature.name
    split.feats = setdiff(names(testdata), pdp.feat)

    # The ys are the predictions (in this case, the standard deviation)
    X = setDT(testdata)
    Y = data.table(.id = seq_row(testdata))

    split.objective = function(y, x, requires.x = FALSE, ...) {

      y = y$.id

      mean(get_gp_uncertainty(gg, y))
    } 

    input.data = list(X = X[, ..split.feats, drop = FALSE], Y = Y)
  }
  
  else {
    stop(paste("Objective", objective, "is not supported."))
  } 

  # Initialize the parent node of the tree
  parent = Node$new(id = 0, depth = 1, subset.idx = seq_len(nrow(input.data$X)))
  
  # Perform splitting for the parent
  tree = list(list(parent))

  for (depth in seq_len(n.split)) {

    leaves = tree[[depth]]

    tree[[depth + 1]] = list()

    for (node.idx in seq_along(leaves)) {

      node.to.split = leaves[[node.idx]]

        if (!is.null(node.to.split)) {
          node.to.split$computeSplit(X = input.data$X, Y = input.data$Y, objective = split.objective, optimizer = find_best_binary_split, min.split = 2)
          node.to.split$computeChildren(input.data$X, input.data$Y)

          tree[[depth + 1]] = c(tree[[depth + 1]], node.to.split$children)        
        } else {
          tree[[depth + 1]] = c(tree[[depth + 1]], list(NULL, NULL))                
        }
      }
    }

  return(tree)
}




compute_data_for_ice_splitting = function(effect, testdata) {
  
  # effect: effect object of IML method FeatureEffect
  
  # Output: A data.frame where each row corresponds to a ice curve 
  
  df = setDT(testdata)
  df$.id = seq_row(df)
  
  ice.feat = effect$feature.name
  features = names(testdata)
  
  # Features we consider splitting 
  split.feats = setdiff(features, ice.feat)
  df.sub = df[, c(".id", split.feats), with = FALSE]  
  
  effectdata = effect$results
  effectdata = effectdata[effectdata$.type=="ice",]
  
  Y = tidyr::spread(effectdata, ice.feat, .value)
  Y = setDT(Y)[, setdiff(colnames(Y), c(".type", ".id")), with = FALSE]
  
  X = df[, split.feats, with = FALSE]
  
  return(list(X = X, Y = Y))
}



################################################################################
# move to evaulation?


compute_trees = function(n.split, models, features, testdata, grid.size, objectives) {
  
  # Compute trees for a list of models and a list of objectives on a fixed dataset.

  reslist = list()

  for (i in seq_along(models)) {

    print(paste("Model number", i))

    model = models[[i]]

    results_for_features = lapply(features, function(feature) {

      # Compute all ice curves
      mymodel = makeS3Obj("mymodel", fun = function() return(model))
      
      predict.mymodel = function(object, newdata) {
        pred = predict(object$fun(), newdata = newdata)
        pp = getPredictionSE(pred)

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

      trees = lapply(objectives, function(objective) {
        compute_tree(effect = effect_sd, testdata = testdata, objective = objective, n.split = n.split) 
      })

      list(res.pdp = res.pdp, res.ice = res.ice, trees = trees)
    })

    names(results_for_features) = features

    reslist[[i]] = results_for_features
  }

  reslist
}








compute_trees_pdp_cor = function(n.split, models, features, testdata, grid.size, objectives) {
  
  # Compute trees for a list of models and a list of objectives on a fixed dataset.

  reslist = list()

  for (i in seq_along(models)) {

    print(paste("Model number", i))

    model = models[[i]]

    results_for_features = lapply(features, function(feature) {

      # Compute an effect just to get the grid points 
      predictor = Predictor$new(model = model, data = as.data.frame(testdata)[, model$features])
      effect_mean = FeatureEffect$new(predictor = predictor, feature = feature, method = "pdp+ice", grid.size = grid.size)
            
      effect_mean_d = setDT(effect_mean$results)
      names(effect_mean_d)[2] = "mean"

      sf = c(feature, "mean", ".id")
      res.pdp = effect_mean_d[.type == "pdp", ..sf]
      res.ice = effect_mean_d[.type == "ice", ..sf]

      split.feats = setdiff(names(testdata), feature)





      trees = lapply(objectives, function(objective) {
        compute_tree(effect = effect_sd, testdata = testdata, objective = objective, n.split = n.split) 
      })

      list(res.pdp = res.pdp, res.ice = res.ice, trees = trees)
    })

    names(results_for_features) = features

    reslist[[i]] = results_for_features
  }

  reslist
}



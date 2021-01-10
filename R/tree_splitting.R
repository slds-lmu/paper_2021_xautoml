library(R6)

Node <- R6Class("Node", list(
    id = NULL,
    
    # on which depth is the node
    depth = NULL,

    # ids of the instances of data that are in this node
    X = NULL,
    Y = NULL,
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

    initialize = function(id, depth = NULL, X, Y, subset.idx, id.parent = NULL, child.type = NULL) {
      
      assert_numeric(id, len = 1)
      assert_class(X, c("data.table"))
      assert_class(Y, c("data.table"))
      assert_numeric(depth, len = 1, null.ok = TRUE)

      assert_numeric(subset.idx, min.len = 1, max.len = nrow(X))
      assert_numeric(id.parent, len = 1, null.ok = TRUE)
      assert_character(child.type, null.ok = TRUE)

      self$id = id
      self$X = X
      self$Y = Y
      self$depth = depth
      self$subset.idx = subset.idx
      self$id.parent = id.parent
      self$child.type = child.type
      self$objective.value = 

      self$stop.criterion.met = FALSE
    },

    computeSplit = function(objective, optimizer, min.split = 10) {
      
      if (length(self$subset.idx) < min.split) {
        self$stop.criterion.met = TRUE
      } else {
        # Just for information purposes
        self$objective.value = objective(y = self$Y[self$subset.idx, ], x = self$X[self$subset.idx, ])

        tryCatch({
          split = split_parent_node(Y = self$Y[self$subset.idx, ], X = self$X[self$subset.idx, ], objective = objective, optimizer = find_best_binary_split, min.node.size = min.split)
          self$split.feature = split$feature[split$best.split]
          self$split.value = unlist(split$split.points[split$best.split])
        }, 
        error = function(cond) {
          message(paste0("Min.node.size is reached in node ", self$id))
          self$stop.criterion.met = TRUE
        })
      }
    },

    computeChildren = function() {

      if (self$stop.criterion.met) {
        # no further split is performed
        self$children = list("left.child" = NULL, "right.child" = NULL)
      } else {
        if(is.null(self$split.feature))
          stop("Please compute the split first via computeSplit().")


        idx.left = which(self$X[self$subset.idx, self$split.feature, with = FALSE] <= self$split.value)
        idx.right = which(self$X[self$subset.idx, self$split.feature, with = FALSE] > self$split.value)

        idx.left = self$subset.idx[idx.left]
        idx.right = self$subset.idx[idx.right]

        left.child = Node$new(id = 1, depth = self$depth + 1, X = self$X, Y = self$Y, subset.idx = idx.left, id.parent = self$id, child.type = "<=")
        right.child = Node$new(id = 2, depth = self$depth + 1, X = self$X, Y = self$Y, subset.idx = idx.right, id.parent = self$id, child.type = ">")

        self$children = list("left.child" = left.child, "right.child" = right.child)
      }
    }
  )
)



compute_tree = function(model, testdata, feature, objective, n.split, grid.size, addMean = FALSE) {

  if (objective == "SS_L1") {
    mymodel = makeS3Obj("mymodel", fun = function() return(model))
    predict.mymodel = function(object, newdata) {
      pred = predict(object$fun(), newdata = newdata)
      pp = getPredictionSE(pred)
      if(addMean == TRUE) pp = getPredictionResponse(pred) - 2*pp
      return(pp)
    }
    predictor = Predictor$new(model = mymodel, data = testdata[, model$features], predict.function = predict.mymodel)
    effect = FeatureEffect$new(predictor = predictor, feature = feature, method = "ice", grid.size = grid.size)

    # define objective
    split.objective = function(y, x, requires.x = FALSE, ...) {
      require(Rfast)
      ypred = Rfast::colMedians(as.matrix(y))
      sum(t(abs(t(y) - ypred)))
    } 

    input.data = compute_data_for_ice_splitting(effect)
  } 
  else if (objective == "SS_L2") {
    mymodel = makeS3Obj("mymodel", fun = function() return(model))
    predict.mymodel = function(object, newdata) {
      pred = predict(object$fun(), newdata = newdata)
      pp = getPredictionSE(pred)
      if(addMean == TRUE) pp = getPredictionResponse(pred) - 2*pp
      return(pp)
    }
    predictor = Predictor$new(model = mymodel, data = testdata[, model$features], predict.function = predict.mymodel)
    effect = FeatureEffect$new(predictor = predictor, feature = feature, method = "ice", grid.size = grid.size)
    
    # define objective
    split.objective = function(y, x, requires.x = FALSE, ...) {
      
      ypred = colMeans(as.matrix(y))
      sum(t((t(y) - ypred)^2))
    } 
    
    input.data = compute_data_for_ice_splitting(effect)
  }
  
  else if (objective == "SS_area") {
    mymodel = makeS3Obj("mymodel", fun = function() return(model))
    predict.mymodel = function(object, newdata) {
      pred = predict(object$fun(), newdata = newdata)
      pp = getPredictionSE(pred)
      if(addMean == TRUE) pp = getPredictionResponse(pred) - 2*pp
      return(pp)
    }
    predictor = Predictor$new(model = mymodel, data = testdata[, model$features], predict.function = predict.mymodel)
    effect = FeatureEffect$new(predictor = predictor, feature = feature, method = "ice", grid.size = grid.size)
    
    # define objective
    split.objective = function(y, x, requires.x = FALSE, ...) {
      row_means = rowMeans(y) # area of individual ice curves
      ypred = mean(row_means) # area of pdp
      sum((row_means - ypred)^2)
    } 
    
    input.data = compute_data_for_ice_splitting(effect)
  }
  
  else if (objective == "SS_area_med") {
    mymodel = makeS3Obj("mymodel", fun = function() return(model))
    predict.mymodel = function(object, newdata) {
      pred = predict(object$fun(), newdata = newdata)
      pp = getPredictionSE(pred)
      if(addMean == TRUE) pp = getPredictionResponse(pred) - 2*pp
      return(pp)
    }
    predictor = Predictor$new(model = mymodel, data = testdata[, model$features], predict.function = predict.mymodel)
    effect = FeatureEffect$new(predictor = predictor, feature = feature, method = "ice", grid.size = grid.size)
    
    # define objective
    split.objective = function(y, x, requires.x = FALSE, ...) {
      row_means = rowMeans(y) # area of individual ice curves
      ypred = median(row_means) # area of pdp
      sum((row_means - ypred)^2)
    } 
    
    input.data = compute_data_for_ice_splitting(effect)
  }
  
  else if (objective == "SS_area_quant") {
    mymodel = makeS3Obj("mymodel", fun = function() return(model))
    predict.mymodel = function(object, newdata) {
      pred = predict(object$fun(), newdata = newdata)
      pp = getPredictionSE(pred)
      if(addMean == TRUE) pp = getPredictionResponse(pred) - 2*pp
      return(pp)
    }
    predictor = Predictor$new(model = mymodel, data = testdata[, model$features], predict.function = predict.mymodel)
    effect = FeatureEffect$new(predictor = predictor, feature = feature, method = "ice", grid.size = grid.size)
    
    # define objective
    split.objective = function(y, x, requires.x = FALSE, ...) {
      row_means = rowMeans(y) # area of individual ice curves
      ypred = quantile(row_means, 0.2) # area of pdp
      sum((row_means - ypred)^2)
    } 
    
    input.data = compute_data_for_ice_splitting(effect)
  }
  
  else if (objective == "SS_sd") {
    mymodel = makeS3Obj("mymodel", fun = function() return(model))
    predict.mymodel = function(object, newdata) {
      pred = predict(object$fun(), newdata = newdata)
      pp = getPredictionSE(pred)
      return(pp)
    }
    predictor = Predictor$new(model = mymodel, data = testdata[, model$features], predict.function = predict.mymodel)
    predictions = predictor$predict(testdata[, model$features])
    
    # define objective
    split.objective = function(y, x, requires.x = FALSE, ...) {
      sum(abs(y$pred - median(y$pred)))
    } 
    split.feats = setdiff(model$features, feature)
    input.data = list(X = setDT(testdata[,split.feats, drop = FALSE]), Y = setDT(predictions) )
  }
  
  else {
    stop(paste("Objective", objective, "is not supported."))
  } 

  # Initialize the parent node of the tree
  parent = Node$new(id = 0, depth = 0, X = input.data$X, Y = input.data$Y, subset.idx = seq_len(nrow(input.data$X)))
  
  # Perform splitting for the parent
  tree = list(list(parent))

  for (depth in seq_len(n.split)) {

    leaves = tree[[depth]]

    tree[[depth + 1]] = list()

    for (node.idx in seq_along(leaves)) {

      node.to.split = leaves[[node.idx]]

      if (!is.null(node.to.split)) {
        node.to.split$computeSplit(split.objective, find_best_binary_split)
        node.to.split$computeChildren()

        tree[[depth + 1]] = c(tree[[depth + 1]], node.to.split$children)        
      } else {
        tree[[depth + 1]] = c(tree[[depth + 1]], list(NULL, NULL))                
      }
    }
  }
  return(tree)
}


get_size_of_tree = function(tree) {
  lapply(tree, function(nodes) unlist(lapply(nodes, function(node) length(node$subset.idx))))
}

get_objective_values = function(tree, depth = NULL) {
  if (is.null(depth))
    lapply(tree, function(nodes) unlist(lapply(nodes, function(node) node$objective.value)))
  else 
    unlist(lapply(tree[[depth]], function(node) node$objective.value))
}

order_nodes_by_objective = function(tree, depth) {
  order(get_objective_values(tree, depth = depth))
}

compute_pdp_for_node = function(node, testdata, model, pdp.feature, grid.size, objective.gt = NULL, method = "pdp_var_gp", alpha = 0.05) {

    data = testdata[node$subset.idx, ]
    data = as.data.frame(data)
    pp = marginal_effect_sd_over_mean(model = model, feature = pdp.feature, data = data, grid.size = grid.size, method = method)
    
    q = qnorm(1 - alpha / 2)
    pp$lower = pp$mean - q * pp$sd
    pp$upper = pp$mean + q * pp$sd 


    pp.gt = NULL
    if (!is.null(objective.gt))
      pp.gt = marginal_effect(objective.gt, pdp.feature, data, model, grid.size)

    return(list(pdp_data = pp, pdp_groundtruth_data = pp.gt))
}



plot_pdp_for_node = function(node, testdata, model, pdp.feature, grid.size, objective.gt = NULL, method = "pdp_var_gp", alpha = 0.05) {
 
    data = compute_pdp_for_node(node, testdata, model, pdp.feature, grid.size, objective.gt, method, alpha = alpha)

    pp = data$pdp_data
    pp.gt = data$pdp_groundtruth_data

    p = ggplot() + theme_bw()

    if (!is.null(pp.gt)) {
      
      p = p + geom_line(data = pp.gt, aes_string(x = pdp.feature, y = "mean"))                        

    }

    p = p + geom_ribbon(data = pp, aes_string(x = pdp.feature, ymin = "lower", ymax = "upper"), alpha = 0.2)
    p = p + geom_line(data = pp, aes_string(x = pdp.feature, y = "mean"), colour = "blue") 
    p = p + ggtitle(paste0("Obj. value: ", round(node$objective.value), "; Size = ", length(node$subset.idx)))

  return(p)
}

plot_tree_pdps = function(tree, df, model, pdp.feature, obj = NULL, depth, method = "pdp_var_gp", alpha = 0.05, best_candidate = NULL) {

    # tree object
    # df:  the tree was used to compute the pdps
    # model: Model that we want to visualize
    # obj: ground-truth objective if available
    # depth: at which depth do we want to "draw" the PDP? 
    
    depth = length(tree)
    
    # First, build a tree in partykit
    # Create partysplit objects for all splits that are performed
    splits = lapply(seq_len(depth - 1), function(i) {
      lapply(seq_len(length(tree[[i]])), function(j) {
          node = tree[[i]][[j]]
          partysplit(which(node$split.feature == ps_ids), breaks = round(node$split.value, 4))
        })
    })
    
    d = depth - 1
    
    ids = 2^(d):(2^(d + 1) - 1)

    # Create the leave nodes as partynodes (placeholder)
    nodes = lapply(ids, function(i) {
        partynode(id = i)
    })

    # Now, recursively build the tree
    for (d in seq(depth - 2, 0)) {
        ids = 2^(d):(2^(d + 1) - 1)

        # transfer all into nodes and use the correct children
        nodes = lapply(seq_along(ids), function(i) {
            partynode(i, split = splits[[d + 1]][[i]], kids = 
                nodes[(2 * i - 1):(2 * i)]
            )
        })
    }

    ntest = nrow(df)

    df_orig = df

    if (!is.null(best_candidate))
      df = rbind(df, best_candidate[, names(df)]) 

    # Modify the test data (this is just a dirty workaround)
    df$sd = NA
    df$mean = NA
    df$xs = NA
    df$lower = NA
    df$upper = NA
    df$gt = NA
    df$best = NA
    df$subset_idx = seq_len(nrow(df))

    py = party(node = nodes[[1]], data = df)
    
    # Create a ggparty object
    ggpobj = ggparty(py)
    
    stack = tree[[1]]
    i = 1

    # Now create the data for the pdps 

    while (length(stack) > 0) {

      # check the first element of the stack      
      node = stack[[1]]
      
      plotdata = compute_pdp_for_node(node = node, 
        testdata = df_orig,
        model = model, 
        pdp.feature = pdp.feature, 
        objective.gt = obj, 
        method = method, 
        alpha = alpha)

      pp = plotdata$pdp_data
      pp.gt = plotdata$pdp_groundtruth_data

      tl = length(ggpobj$data[, paste0("nodedata_", pdp.feature)][[i]])

      if (tl > nrow(pp)) {
        ggpobj$data$nodedata_xs[[i]] = c(pp[ , pdp.feature], rep(NA, tl - nrow(pp)))
        ggpobj$data$nodedata_mean[[i]] = c(pp[ , "mean"], rep(NA, tl - nrow(pp)))
        ggpobj$data$nodedata_lower[[i]] = c(pp[ , "lower"], rep(NA, tl - nrow(pp)))
        ggpobj$data$nodedata_upper[[i]] = c(pp[ , "upper"], rep(NA, tl - nrow(pp)))

        if (!is.null(obj))
            ggpobj$data$nodedata_gt[[i]] = c(pp.gt$mean, rep(NA, tl - nrow(pp)))
      
        if (!is.null(best_candidate) && (ntest + 1) %in% ggpobj$data$nodedata_subset_idx[[i]]) {
            ggpobj$data$nodedata_best[[i]][1] = best_candidate[, pdp.feature][1]            
        }

      }

      # remove it from the stack
      stack[[1]] = NULL

      # extend the stack
      stack = c(node$children$left.child, node$children$right.child, stack)

      i = i + 1
    }

    p = ggpobj +
      geom_edge() +
      geom_edge_label() +
      geom_node_splitvar() 

    if (!is.null(best_candidate) && !is.null(obj)) {

    p = p + geom_node_plot(gglist = list(
                                   geom_ribbon(aes(x = xs, ymin = lower, ymax = upper), alpha = 0.2), 
                                   geom_line(aes(x = xs, y = mean), colour = "blue"), 
                                   geom_line(aes(x = xs, y = gt)), 
                                   geom_vline(aes(xintercept = best), colour = "orange", lty = 2)
                                  )
                    )
    } 

    if (is.null(best_candidate) && !is.null(obj) ) {

      p = p + geom_node_plot(gglist = list(
                                     geom_ribbon(aes(x = xs, ymin = lower, ymax = upper), alpha = 0.2), 
                                     geom_line(aes(x = xs, y = mean), colour = "blue"), 
                                     geom_line(aes(x = xs, y = gt))
                                    )
                      )
      } 


    if (!is.null(best_candidate) && is.null(obj) ) {

      p = p + geom_node_plot(gglist = list(
                                     geom_ribbon(aes(x = xs, ymin = lower, ymax = upper), alpha = 0.2), 
                                     geom_line(aes(x = xs, y = mean), colour = "blue"), 
                                      geom_vline(aes(xintercept = best), colour = "orange", lty = 2)
                                    )
                      )
      } 


     if (is.null(best_candidate) && is.null(obj) ) {

      p = p + geom_node_plot(gglist = list(
                                     geom_ribbon(aes(x = xs, ymin = lower, ymax = upper), alpha = 0.2), 
                                     geom_line(aes(x = xs, y = mean), colour = "blue")
                                    )
                      )
      }       

    return(p)
}



compute_data_for_ice_splitting = function(effect) {
  
  # effect:     effect objected outputted by iml 

  # Output: A data.frame that where each row corresponds to a ice curve 

  df = effect$predictor$data$X
  df$.id = seq_len(nrow(df))
  
  ice.feat = effect$feature.name
  features = effect$predictor$data$feature.names

  # Features we consider splitting 
  split.feats = setdiff(features, ice.feat)
  df.sub = df[, c(".id", split.feats), with = FALSE]  
  
  effects = data.table(effect$results)
  #effects = merge(effeccompute_datats, df.sub, by = c(".id"))

  Y = tidyr::spread(effects, ice.feat, .value)
  Y = Y[, setdiff(colnames(Y), c(".type", ".id")), with = FALSE]
  
  X = df[, split.feats, with = FALSE]

  return(list(X = X, Y = Y))
}



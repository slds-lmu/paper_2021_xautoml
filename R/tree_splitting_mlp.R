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



compute_tree = function(effect, objective, n.split) {
  #browser()
  input.data = compute_data_for_ice_splitting(effect)

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
        node.to.split$computeSplit(objective, find_best_binary_split)
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

plot_pdp_for_node = function(effect, node, model, pdp.feature, objective.groundtruth = NULL, method = "pdp_var_gp") {
  data = effect$predictor$data$X[node$subset.idx, ]
    data = as.data.frame(data)
    pp = marginal_effect_sd_over_mean(model = model, feature = pdp.feature, data = data, method = method)
    pp$lower = pp$mean - 2 * pp$sd
    pp$upper = pp$mean + 2 * pp$sd


    p = ggplot() + theme_bw()

    if (!is.null(objective.groundtruth)) {
      data.groundtruth = data
      data.groundtruth$batch_size = 2^data.groundtruth$batch_size
      data.groundtruth$max_units = 2^data.groundtruth$max_units

      pp.gt = predicted_marginal_effect(objective.groundtruth, pdp.feature, data.groundtruth)
      if (pdp.feature %in% c("batch_size", "max_units")) {
        pp.gt[pdp.feature] = log(pp.gt[pdp.feature], 2)
      }
      pp.gt$mean = (100-pp.gt$mean)/100
      
      p = p + geom_line(data = pp.gt, aes_string(x = pdp.feature, y = "mean"))                        

    }

  p = p + geom_ribbon(data = pp, aes_string(x = pdp.feature, ymin = "lower", ymax = "upper"), alpha = 0.2)
  p = p + geom_line(data = pp, aes_string(x = pdp.feature, y = "mean"), colour = "blue") 
  p = p + ggtitle(paste0("Obj. value: ", round(node$objective.value), "; Size = ", length(node$subset.idx)))

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
  #effects = merge(effects, df.sub, by = c(".id"))

  Y = tidyr::spread(effects, ice.feat, .value)
  Y = Y[, setdiff(colnames(Y), c(".type", ".id")), with = FALSE]
  
  X = df[, split.feats, with = FALSE]

  return(list(X = X, Y = Y))
}


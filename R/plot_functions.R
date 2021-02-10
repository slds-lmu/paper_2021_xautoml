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

plot_tree_pdps = function(tree, df, model, pdp.feature, obj = NULL, depth, method = "pdp_var_gp", alpha = 0.05, grid.size, best_candidate = NULL) {
  
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
  
  if (depth == 2) {
    d = 0
    ids = 2^(d):(2^(d + 1) - 1)
    
    # transfer all into nodes and use the correct children
    nodes = lapply(seq_along(ids), function(i) {
      partynode(i, split = splits[[d + 1]][[i]], kids = 
                  nodes[(2 * i - 1):(2 * i)]
      )
    })
  } else {
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
                                    grid.size = grid.size,
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



compute_pdp_for_node = function(node, testdata, model, pdp.feature, grid.size, objective.gt = NULL, method = "pdp_var_gp", alpha = 0.05) {
  
  data = testdata[node$subset.idx, ]
  data = as.data.frame(data)
  pp = marginal_effect_sd_over_mean(model = model, feature = pdp.feature, data = data, grid.size = grid.size, method = method)
  
  q = qnorm(1 - alpha / 2)
  pp$lower = pp$mean - q * pp$sd
  pp$upper = pp$mean + q * pp$sd 
  
  pp.gt = NULL
  if (!is.null(objective.gt))
    pp.gt = marginal_effect_mlp(obj = objective.gt, data = data, feature = pdp.feature, model =  model, grid.size = grid.size)
  
  return(list(pdp_data = pp, pdp_groundtruth_data = pp.gt))
}

compute_pdp_for_node_with_data = function(node, pdp.ice, pdp.gt = NULL, pdp.feature, grid.size, method = "pdp_var_gp", alpha = 0.05) {
  
  data = testdata[node$subset.idx, ]
  data = as.data.frame(data)
  pp = marginal_effect_sd_over_mean(model = model, feature = pdp.feature, data = data, grid.size = grid.size, method = method)
  
  q = qnorm(1 - alpha / 2)
  pp$lower = pp$mean - q * pp$sd
  pp$upper = pp$mean + q * pp$sd 
  
  pp.gt = NULL
  if (!is.null(objective.gt))
    pp.gt = marginal_effect_mlp(obj = objective.gt, data = data, feature = pdp.feature, model =  model, grid.size = grid.size)
  
  return(list(pdp_data = pp, pdp_groundtruth_data = pp.gt))
}


library("plot3D")
library("ggplot2")
library("GGally")
library("gridExtra")

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
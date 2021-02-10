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


plot_pdp_with_uncertainty_1D = function(me, me.gt = NULL, alpha = 0.05) {

    pdp.feature = names(me)[1]

    p = ggplot() 
  
    if (!is.null(me.gt)) {
      
      p = p + geom_line(data = me.gt, aes_string(x = pdp.feature, y = "mean"))                        
      
    }
   
    if ("sd" %in% names(me)) {
      q = qnorm(1 - alpha / 2)
      me$lower = me$mean - q * me$sd
      me$upper = me$mean + q * me$sd 

      p = p + geom_ribbon(data = me, aes_string(x = pdp.feature, ymin = "lower", ymax = "upper"), alpha = 0.2)    
    } 
    p = p + geom_line(data = me, aes_string(x = pdp.feature, y = "mean"), colour = "blue") 

    return(p)
}


# helper function to create MBO runs

createMBOrun = function(fun, max.evals, lambda, type = "MBO", store_path, init_size = NULL, seed = NULL, eval_performance = FALSE, kernel = "matern3_2") {
  
  ps = getParamSet(fun)
  
  ctrl = makeMBOControl(store.model.at = seq_len(max.evals))
  ctrl = setMBOControlTermination(ctrl, max.evals = max.evals)
  ctrl = setMBOControlInfill(ctrl, makeMBOInfillCritCB(cb.lambda = lambda))
  
  if (is.null(init_size))
    init_size = 4 * getParamLengths(ps)
  
  
  if (type == "LHS") {
    init_size = max.evals - 1
    max.evals = max.evals + 1
    lambda = 100        
  }
  
  if (!is.null(seed))
    set.seed(seed)
  
  des = generateDesign(n = init_size, par.set = ps, fun = lhs::randomLHS)
  
  lrn = makeLearner("regr.km", predict.type = "se", covtype = kernel, optim.method = "gen", nugget.stability = 10^(-8))
  
  res = mbo(fun, design = des, learner = lrn, control = ctrl, show.info = FALSE)
  attr(res, "type") = type
  
  # evaluate on a big random LHS 
  if (eval_performance) {
    des = generateDesign(n = 10000, par.set = ps, fun = lhs::randomLHS)
    des$y = apply(des, 1, fun)
    perfs = lapply(res$models, function(model) {
      pred = predict(model, newdata = des)
      c(performance(pred, list(mae, rmse)), mean.se = mean(pred$data$se))
    })  
    perfs = do.call(rbind, perfs)  
    res$model.performances = perfs   
  }
  
  saveRDS(res, store_path)
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




# helper functions to extract information of tree


# determine size of a tree
get_size_of_tree = function(tree) {
  lapply(tree, function(nodes) unlist(lapply(nodes, function(node) length(node$subset.idx))))
}

# determine objective values from each node for a specific tree depth or for the entire tree
get_objective_values = function(tree, depth = NULL) {
  if (is.null(depth))
    lapply(tree, function(nodes) unlist(lapply(nodes, function(node) node$objective.value)))
  else 
    unlist(lapply(tree[[depth]], function(node) node$objective.value))
}

# order nodes of specific tree depth according to their objective values
order_nodes_by_objective = function(tree, depth) {
  order(get_objective_values(tree, depth = depth))
}


# check again if we need this function
# get_ice_curves <- function(model, data, feature, grid.size, mean = FALSE){
#   mymodel = makeS3Obj("mymodel", fun = function() return(model))
#   predict.mymodel = function(object, newdata) {
#     pred = predict(object$fun(), newdata = newdata)
#     pp = getPredictionSE(pred)
#     if(mean == TRUE) pp = getPredictionResponse(pred) 
#     return(pp)
#   }
#   
#   predictor = Predictor$new(model = mymodel, data = data, predict.function = predict.mymodel)
#   effect = FeatureEffect$new(predictor = predictor, feature = feature, grid.size = grid.size, method = "ice")
# }

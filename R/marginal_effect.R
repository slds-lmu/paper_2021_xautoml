#' @title Calculates PDPs and associated confidence estimates
#'
#' @description
#' Uses functions in customtrees.r for splitting effect curves according to defined objective.
#'
#' @param obj objective function that needs to be optimized (groundtruth)
#' @param feature feature (hyperparameter) lambda_S
#' @param all.features character vector with feature names of regarded dataset
#' @param data dataset
#' @param model a model object that inherents an own predict function
#' @param grid.size size/number of equidistant grid to calculate marginal effects for lambda_S


# calculate marginal effects for a specific feature
marginal_effect = function(obj, feature, data = NULL, grid.size, all.features, method = "pdp+ice") {
  # output: data.frame with marginal effects of regarded feature
    
    ps = getParamSet(obj)

	if (is.null(data)) {
		data = generateRandomDesign(n = 100, par.set = ps)
	}

    # create S3 model object and predict function for objective function to calculate effects    
    mymodel = makeS3Obj("mymodel", fun = function(data) {
        res = apply(setDT(data)[, ..all.features, drop = FALSE], 1, obj)
        return(res)
    })

    predict.mymodel = function(object, newdata) {
        object$fun(newdata)
    }
    
    # calculate pdp (and ice curves)                    
    predictor = Predictor$new(model = mymodel, data = setDT(data)[, ..all.features, drop = FALSE], predict.function = predict.mymodel)
    effects = FeatureEffect$new(predictor = predictor, feature = feature, grid.size = grid.size, method = method)

    res = effects$results
    names(res)[1:2] = c(feature, "mean")
                        
    return(res)
}

# we probably need only one of them?
marginal_effect_mlp = function(obj, feature, data, all.features, grid.size) {
        
    mymodel = makeS3Obj("mymodel", fun = function(data) {
        res = lapply(seq_row(data), function(i) {
            obj(as.list(data[i, all.features]))
        })
        return(unlist(res))
    })

    predict.mymodel = function(object, newdata) {
        object$fun(newdata)
    }
                        
    predictor = Predictor$new(model = mymodel, data = data[, ..all.features], predict.function = predict.mymodel)
    effects = FeatureEffect$new(predictor = predictor, feature = feature, grid.size = grid.size, method = "pdp+ice")

    res = effects$results
    names(res)[1:2] = c(feature, "mean")
                        
    return(res)
}


# calculate PDP for a given model and feature
predicted_marginal_effect = function(model, feature, data, grid.size) {
  # output: data.frame with marginal effects of regarded feature
                                
    predictor = Predictor$new(model = model, data = data)
    effects = FeatureEffect$new(predictor = predictor, feature = feature, grid.size = grid.size, method = "pdp")
    
    res = effects$results
    names(res) = c(feature, "mean")
    
    return(res)
}


# calculate marginal effects for variance function
marginal_effect_sd_over_mean = function(model, feature, data, grid.size = 20, method = "pdp_var", alpha = 0.05, correction = NULL) {

	# method:
	# - pdp_sd: 			partial dependence over the posterior standard deviation 1 / n * sum s(x_S, x_C) is computed 
	# - pdp_var: 			partial dependence over the posterior variance 1 / n * sum s^2(x_S, x_C); 
	# 						then, we device by 1 / n and take the square root to get an estimate for the standard deviation of the mean
	# - pdp_var_gp: 		this estimator takes into account the correlation structure of this variant 
	# - thompson: 			An empirical variant via thompson sampling 
  
  # output:   data.frame with marginal effects of uncertainty estimate depending on chosen method for a chosen feature
  

    # standard PDP over mean 
    res = predicted_marginal_effect(model, feature, data, grid.size)

    # compute now the PDP for the variance 
  	# helper function for a custom PDP 
    mymodel = makeS3Obj("mymodel", fun = function() return(model))
    
    if (method %in% c("pdp_sd", "pdp_var")) {
	    predict.mymodel = function(object, newdata) {
	      pred = predict(object$fun(), newdata = newdata)

	      if (method == "pdp_sd")
	      	pp = getPredictionSE(pred) 
	      if (method == "pdp_var")
	      	pp = getPredictionSE(pred)^2

	      return(pp)
	    }

	    predictor = Predictor$new(mymodel, data = data, predict.function = predict.mymodel)
	    
	    effects = FeatureEffect$new(predictor = predictor, feature = feature, grid.size = grid.size, method = "pdp")

	    if (method == "pdp_sd") {
	    	res$sd = effects$results$.value
	    } 
	    if (method == "pdp_var") {
	    	res$sd = sqrt(effects$results$.value) # we have to devide once more by n --> see formula
	    }
    	res$`NA` = NULL
    }


    if (method %in% c("pdp_var_gp")) {
    
    	# Extract the grid points from the above PDP, such that we make sure we take the same grid points
    	gridvalues = res[, feature]

    	# Extract the learned GP 
    	km = model$learner.model

    	# Covtype is needed later to extract the covariance 
    	covtype = attr(km, "covariance")
    
	    res = lapply(gridvalues, function(gv) {

	    	# Create vector along the gridvalue gv by combining it with the "test dataset"
	    	gg = merge(gv, data[, setdiff(colnames(data), feature)])
	    	names(gg) = c(feature, setdiff(colnames(data), feature))
	    	gg = gg[, names(data)]

	    	# Compute the posterior mean and covariance of the predictions at points in gg
	        pred = predict(object = km, newdata = gg, type = "SK", cov.compute = TRUE)
	        C = pred$cov
	        m = pred$mean 

	      	# To boil everything down we have to compute now the mean of this vector, as well as the 
	      	# variance over the mean
	      	mean_pdp = mean(m)
	      	sd_pdp = 1 / nrow(gg) * sqrt(sum(C)) # see Wikipedia "Variance#Sum_of_correlated_variables#Matrix notation" 

	        df = data.frame(gv, mean = mean_pdp, sd = sd_pdp)
	        names(df)[1] = feature

	        return(df)
	    })

    	res = do.call(rbind, res)
    }

    return(res)
}




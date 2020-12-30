marginal_effect = function(obj, feature, data) {
        
    mymodel = makeS3Obj("mymodel", fun = function(data) return(apply(data[, c("x1", "x2")], 1, obj)))
                        
    predict.mymodel = function(object, newdata) {
        object$fun(newdata)
    }
                        
    predictor = Predictor$new(model = mymodel, data = data[c("x1", "x2")], predict.function = predict.mymodel)
    effects = FeatureEffect$new(predictor = predictor, feature = feature, method = "pdp")

    res = effects$results
    names(res) = c(feature, "mean")
                        
    return(res)
}


predicted_marginal_effect = function(model, feature, data) {
                                
    predictor = Predictor$new(model = model, data = data)
    effects = FeatureEffect$new(predictor = predictor, feature = feature, method = "pdp")
    
    res = effects$results
    names(res) = c(feature, "mean")
    
    return(res)
}

marginal_effect_sd_over_mean = function(model, feature, data, method) {

	# Different methods to estimate the standard deviation are implemented
	# - pdp_sd: 			partial dependence over the posterior standard deviation 1 / n * sum s(x_S, x_C) is computed 
	# - pdp_var: 			partial dependence over the posterior variance 1 / n * sum s^2(x_S, x_C); 
	# 						then, we device by 1 / n and take the square root to get an estimate for the standard deviation of the mean
	# - pdp_var_gp: 		this estimator takes into account the correlation structure of this variant 
	# - thompson: 			An empirical variant via thompson sampling 

    # standard PDP over mean 
    res = predicted_marginal_effect(model, feature, data)

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
	    
	    effects = FeatureEffect$new(predictor = predictor, feature = feature, method = "pdp")

	    if (method == "pdp_sd") {
	    	res$sd = effects$results$.value
	    } 
	    if (method == "pdp_var") {
	    	res$sd = sqrt(1 / nrow(data) * effects$results$.value) # we have to devide once more by n --> see formula
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

	    	# Compute the posterior mean and covariance of the predictions at points in gg
	        pred = predict(object = km, newdata = gg, type = "SK", cov.compute = TRUE)
	        C = pred$cov
	        m = pred$mean 

	      	# To boil everything down we have to compute now the mean of this vector, as well as the 
	      	# variance over the mean
	      	mean_pdp = mean(m)
	      	var_pdp = 1 / nrow(gg)^2 * sum(C) # see Wikipedia "Variance#Sum_of_correlated_variables#Matrix notation" 

	        df = data.frame(gv, mean = mean_pdp, sd = sqrt(var_pdp))
	        names(df)[1] = feature

	        return(df)
	    })

    	res = do.call(rbind, res)
    }

    if (method == "thompson") {

		# We compute the grid we want to draw the GP's over
		# combine grid points with test data 	    
	    grid = merge(res[, feature], data[, setdiff(x = c("x1", "x2"), y = feature)])
	    names(grid) = c("x1", "x2")

	    # Sample 10000 realizations 
	    samples = DiceKriging::simulate(object = model$learner.model, newdata = grid, nsim = 10000, cond = TRUE)

	    # Compute the partial dependency for every single sample 
	    pdps = lapply(seq_len(nrow(samples)), function(i) {
	        dt = data.table(feature = grid[, feature], .value = samples[i, ])
	        dt[, .(v = mean(.value)), by = feature]
	    })

	    pdp_over_samples = do.call(rbind, pdps)
	    names(pdp_over_samples)[1] = feature

	    # Compute the mean over all PDPs over the samples as well as the standard deviation 
	    res = pdp_over_samples[, .(mean = mean(v), sd = sd(v)), by = feature]
    }

    return(res)
}

conditional_mean_sd = function(model, feature, data, method) {
	# - pdp_cond: 			c(lambda) | lambda_S = lambda_S; variance derived according to law of total variance
	# - pdp_cond_thomps: 	same as pdp_cond, but empirical variant via thomspon sampling 
	    # standard PDP over mean 

    res = predicted_marginal_effect(model, feature, data)

    if (method == "pdp_cond") {
	    mymodel = makeS3Obj("mymodel", fun = function() return(model))
	    
	    # Helper function 
	    predict.mymodel = function(object, newdata) {
	      pred = predict(object$fun(), newdata = newdata)
	      getPredictionSE(pred)^2 
	    }

	    # Get first (#1) of the estimator (i.e. the PDP over the posterior variance)
	    predictor = Predictor$new(mymodel, data = data[c("x1", "x2")], predict.function = predict.mymodel)
	    effects_1 = FeatureEffect$new(predictor = predictor, feature = feature, method = "pdp")

	    # Get (#2) (i.e., the variance over the ice curves for one grid point)
	    predictor = Predictor$new(model = model, data = data[c("x1", "x2")])
	    effects_2 = FeatureEffect$new(predictor = predictor, feature = feature, method = "pdp+ice")
	    df = setDT(effects_2$results)
	    df = df[.type == "ice", .(.value= sd(.value)^2), by = feature]
	    
	    res$sd = sqrt(df$.value + effects_1$results$.value)
	    res$`NA` = NULL
    }

    if (method == "pdp_cond_thomps") {

    	# Create all values along the grid points with the help of the test data
	    grid = merge(res[, feature], data[, setdiff(x = c("x1", "x2"), y = feature)])
	    names(grid) = c("x1", "x2")

	    # Sample realizations of the posterior process
	    samples = DiceKriging::simulate(object = model$learner.model, newdata = grid, nsim = 1000, cond = TRUE)

	    # One data table for each draw 
	    pdps = lapply(seq_len(nrow(samples)), function(i) {
	        dt = data.table(feature = grid[, feature], .value = samples[i, ])
	    })

	    pdps = do.call(rbind, pdps)
	    names(pdps)[1] = feature

	    # compute mean and standard devation 
	    res = pdps[, .(mean = mean(.value), sd = sd(.value)), by = feature]
    }

    return(res)
}


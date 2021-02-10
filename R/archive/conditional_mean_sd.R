conditional_mean_sd = function(model, feature, data, method, grid.size) {
  
  # method:
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
	    predictor = Predictor$new(mymodel, data = data, predict.function = predict.mymodel)
	    effects_1 = FeatureEffect$new(predictor = predictor, feature = feature, grid.size = grid.size, method = "pdp")

	    # Get (#2) (i.e., the variance over the ice curves for one grid point)
	    predictor = Predictor$new(model = model, data = data)
	    effects_2 = FeatureEffect$new(predictor = predictor, feature = feature, grid.size = grid.size, method = "ice")
	    df = setDT(effects_2$results)
	    df = df[, .(.value= sd(.value)^2), by = feature]
	    
	    res$sd = sqrt(df$.value + effects_1$results$.value)

    }

    if (method == "pdp_cond_thomps") {

	    grid = merge(res[, feature], data[, setdiff(x = names(data), y = feature)])
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


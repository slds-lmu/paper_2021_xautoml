source("R/pdp_helpers.R")
library(iml)
library(BBmisc)
library(mlr)
library(ggplot2)

l = "10"
i = 10
feat = "learning_rate"
features = colnames(data.train.mbo)[-which(colnames(data.train.mbo) %in% c("y", "extrapol", "iteration"))]

#data.test.sub =  data.train.mbo[which(data.train.mbo$iteration==i),features]
data.test.sub =  data_phoneme$rlhs_test_data[sample(1:3000, 1000),features]
model = model.list.mbo[[l]][[i]]
df = data.test.sub

predictor = Predictor$new(model = mymodel, data = df, predict.function = predict.mymodel)

predictor.test.mbo = Predictor$new(model = model.list.mbo[[l]][[i]], data = data.test.sub)
effect = FeatureEffect$new(predictor = predictor.test.mbo, feature = feature, method = "pdp", grid.size = 20)$results

data.test.surr = data_phoneme$rlhs_test_data[,features]
data.test.surr$batch_size = 2^data.test.surr$batch_size
data.test.surr$max_units = 2^data.test.surr$max_units
predictor.surr = Predictor$new(surrogate, data.test.surr)
effects.surr = FeatureEffect$new(predictor.surr, feature, method = "pdp")$results
effects.surr$.value = 1-(effects.surr$.value/100)
if (feature %in% c("batch_size", "max_units")) {
  effects.surr$grid = log(effects.surr[,feature], 2)
}
# else effects.surr$grid = effects.surr[,feature]

pdp_sd = marginal_effect_sd_over_mean(model.list.mbo[[l]][[i]], feature, data.test.sub, "pdp_sd")


ggplot(effect, aes(x = effect[,feature], y = .value)) + geom_line(lwd = 1.5) + geom_line(data = effects.surr, aes(grid,.value), col = "blue", lwd = 1.5) +
  geom_ribbon(data = pdp_sd, aes(x = pdp_sd[,feature], y = mean, ymin = mean - 1.96*sd, ymax = mean + 1.96*sd), alpha = 0.3, fill = "grey") + theme_bw() +
  xlab(feature) + ylab("performance") + ggtitle("PDP on training data")


pdp_var_gp = marginal_effect_sd_over_mean(model.list.mbo[[l]][[i]], feature, data.test.sub, "pdp_var_gp")
ggplot(effect, aes(x = effect[,feature], y = .value)) + geom_line() + geom_line(data = effects.surr, aes(grid,.value), col = "blue") 
  geom_ribbon(data = pdp_var_gp, aes(x = pdp_var_gp[,feature], y = mean, ymin = mean - 1.96*sd, ymax = mean + 1.96*sd), alpha = 0.3, fill = "grey") + theme_bw()

pdp_thompson = marginal_effect_sd_over_mean(model.list.mbo[[l]][[i]], feature, data.test.sub, "thompson")
ggplot(effect, aes(x = effect[,feature], y = .value)) + geom_line() + geom_line(data = effects.surr, aes(grid,.value), col = "blue") +
  geom_ribbon(data = pdp_thompson, aes(x = pdp_thompson[,feature], y = mean, ymin = mean - 1.96*sd, ymax = mean + 1.96*sd), alpha = 0.3, fill = "grey") + theme_bw()



predicted_marginal_effect = function(model, feature, data) {
  
  predictor = Predictor$new(model = model, data = data[,colnames(data) != "y"])
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
    
    predictor = Predictor$new(mymodel, data =  data[,colnames(data) != "y"], predict.function = predict.mymodel)
    
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
      gg = merge(gv, data[, setdiff(features, feature)])
      names(gg) = features

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
    grid = merge(res[, feature], data[, setdiff(x = features, y = feature)])
    names(grid) = features

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


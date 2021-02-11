#' @title Simulation comparing the PDP uncertainty estimates for correctly vs. mis- specified kernel
#'
#' @description 
#' The function simulates a Gaussian process approximation to the Styblinski-Tang function. 
#' For this approximation, the true_kernel is used. 
#' We pretend this GP approximation to be our ground-truth objective. 
#' Then, we approximate this objective by another GP that is correctly specified (true_kernel), 
#' and once with the wrong_kernel. 
#' In both cases, PDPs and the log-likelihood of the PDP of the ground-truth objective is computed.
#' 
#' The experiment is repeated n times. 

#' @param dimension problem dimension 
#' @param true_kernel true kernel function
#' @param wrong_kernel wrong kernel function
#' @param n size of the dataset the GPs are trained on 

simulate_kernel_misspecification = function(dimension, true_kernel = "matern3_2", wrong_kernel = "matern5_2", n) {

    # Use the Styblinski function as a basis
    objective = makeSingleObjectiveFunction(name = paste0("StyblinskiTang", dimension, "D"), fn = function(x) {
        1 / 2 * sum(x^4 - 16 * x^2 + 5 * x)
        }, 
        par.set = makeParamSet(makeNumericVectorParam(id = "x", len = dimension, lower = - 5, upper = 5)), 
        global.opt.params = rep(-2.9035, dimension)
    )
    ps = getParamSet(objective)
     
    # Generate some data we fit our "ground-truth" GP on   
    df = generateRandomDesign(n = n, par.set = ps)
    df$y = apply(df, 1, objective)

    tsk = makeRegrTask(id = "", data = df, target = "y")

    lrn_true = makeLearner("regr.km", covtype = true_kernel)
    model_true = train(lrn_true, tsk)

    # True objective is a GP with a true_kernel
    obj_true = makeSingleObjectiveFunction(name = "objective_true", fn = function(x) {
            x = as.data.frame(t(as.matrix(x)))
            colnames(x) = getParamIds(ps, repeated = TRUE, with.nr = TRUE)
            predict(model_true, newdata = x)$data$response[1]
        }, 
        par.set = ps
    )
    
    # Data used to do the Monte Carlo approximation in the PDPs  
    data = generateRandomDesign(n = 100, par.set = ps)

    # Compute the ground-truth PDP estimate 
    me = marginal_effect(obj = obj_true, feature = "x1", data = data, model = model_true, all.features = model_true$features, grid.size = 20, method = "pdp")
    me = setDT(me)
    
    # Now, we evaluate obj_true for some the same number of points
    df_true = df
    df_true$y = apply(df_true[, seq_len(ncol(df) - 1)], 1, obj_true)

    tsk = makeRegrTask(id = "", data = df_true, target = "y")
    
    # Fit a GP with the wrong kernel and compute its PDP
    lrn_wrong = makeLearner("regr.km", predict.type = "se", covtype = wrong_kernel, nugget.estim = FALSE)# , nugget.stability = 10^(-8))

    model_wrong = train(lrn_wrong, tsk)

    # Compute PDP for x1 with the two different variance estimates
    pdp_wrong_kernel = lapply(c("pdp_var_gp", "pdp_var"), function(method) {
        me_pdp_var = marginal_effect_sd_over_mean(model = model_wrong, "x1", data, 20, method)
        me_pdp_var$type = method
        me_pdp_var$neg_loglik = unlist(lapply(seq_row(me_pdp_var), function(i) {
            - dnorm(me[i, ]$mean, mean = me_pdp_var[i, ]$mean, sd = me_pdp_var[i, ]$sd, log = TRUE) 
        }))
        me_pdp_var$mean.gt = me$mean

        return(me_pdp_var)
    })
    pdp_wrong_kernel = do.call(rbind, pdp_wrong_kernel)
    pdp_wrong_kernel$misspec = TRUE
    
    # Fit a GP with the right kernel and compute its PDP
    lrn_true = makeLearner("regr.km", predict.type = "se", covtype = true_kernel, nugget.estim = FALSE)# , nugget.stability = 10^(-8))

    model_true = train(lrn_true, tsk)

    # Compute PDP for x1 with the two different variance estimates    
    pdp_true_kernel = lapply(c("pdp_var_gp", "pdp_var"), function(method) {
        me_pdp_var = marginal_effect_sd_over_mean(model = model_true, "x1", data, 20, method)
        me_pdp_var$type = method
        me_pdp_var$neg_loglik = unlist(lapply(seq_row(me_pdp_var), function(i) {
            - dnorm(me[i, ]$mean, mean = me_pdp_var[i, ]$mean, sd = me_pdp_var[i, ]$sd, log = TRUE) 
        }))
        me_pdp_var$mean.gt = me$mean
        return(me_pdp_var)
    })
    pdp_true_kernel = do.call(rbind, pdp_true_kernel)
    pdp_true_kernel$misspec = FALSE
    
    me_pdp = rbind(pdp_true_kernel, pdp_wrong_kernel)
    me_pdp$dimension = dimension
    
    return(me_pdp)
}
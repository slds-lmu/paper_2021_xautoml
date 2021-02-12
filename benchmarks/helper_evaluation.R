library(kernlab)

# mlp separate until surrogate data problem fixed
get_eval_measures_mlp = function(res.ice, gt.ice, testdata, idx, pdp.feature, optimum, method = "pdp_var_sd", model = NULL) {
  
  res.ice = res.ice[which(res.ice$.id %in% idx),]

  if (method == "pdp_sd") {
    res.pdp = setDT(res.ice)[, .(mean = mean(mean), sd = mean(sd)), by = pdp.feature] 
  } else {
    res.pdp = setDT(res.ice)[, .(mean = mean(mean), sd = sqrt(mean(sd^2))), by = pdp.feature]     
  }

  gt.ice = gt.ice[which(gt.ice$.id %in% idx),]
  res.gt = setDT(gt.ice)[, .(mean = mean(mean)), by = pdp.feature] 

  pp = res.pdp
  pp.gt = res.gt

  neg_loglik = mean(unlist(lapply(seq_row(pp), function(i) {
      - dnorm(pp.gt[i, ]$mean, mean = pp[i, ]$mean, sd = pp[i, ]$sd, log = TRUE) 
  })))
  
  conf.diff = sum(pp$sd)
  gt.diff.abs = sum(abs(pp.gt$mean - pp$mean))
  gt.diff.sd = sd(pp.gt$mean - pp$mean)
  gt.diff.abs.sd = sd(abs(pp.gt$mean - pp$mean))

  df = data.frame(conf.diff, gt.diff.abs, gt.diff.sd, gt.diff.abs.sd, neg_loglik) 

  if (!is.null(model)) {

    res.pdp.cov = marginal_effect_sd_over_mean(model = model, feature = pdp.feature, data = testdata[idx, ], grid.size = 20, method = "pdp_var_gp") 

    # Also get the data with regards to the other uncertainty estimate   
    conf.diff.cov = sum(res.pdp.cov$sd)
    gt.diff.abs.cov = sum(abs(pp.gt$mean - res.pdp.cov$mean))
    gt.diff.sd.cov = sd(pp.gt$mean - res.pdp.cov$mean)
    gt.diff.abs.sd.cov = sd(abs(pp.gt$mean - res.pdp.cov$mean))

    neg_loglik.cov = mean(unlist(lapply(seq_row(res.pdp.cov), function(i) {
        - dnorm(pp.gt[i, ]$mean, mean = res.pdp.cov[i, ]$mean, sd = res.pdp.cov[i, ]$sd, log = TRUE) 
    })))

    df = cbind(df, data.frame(conf.diff.cov, gt.diff.abs.cov, gt.diff.sd.cov, gt.diff.abs.sd.cov, neg_loglik.cov))
  }

  
  # values at the optimum
  pp_dist_opt = abs(pp[, ..pdp.feature] - as.numeric(optimum[, pdp.feature]))

  df.opt = lapply(1:3, function(nn) {
    best_idx = order(pp_dist_opt)[seq_len(nn)] # adjust number of grid points to evaluate?

    pp.opt = pp[best_idx, ] 
    pp.gt.opt = pp.gt[best_idx, ]

    conf.diff.opt = sum(pp.opt$sd)
    gt.diff.abs.opt = sum(abs(pp.gt.opt$mean - pp.opt$mean))

    df.opt = data.frame(conf.diff.opt, gt.diff.abs.opt)
    names(df.opt) = paste0(names(df.opt), ".", nn)

    df.opt
  })

  df.opt = do.call(cbind, df.opt)

  df = cbind(df, df.opt)
    
  return(df)
}




evaluate_results = function(reslist, optima, gtdata, testdata, models = NULL) {
  
  df = data.frame()

  # Iterate over all models we have 
  for (i in seq_along(reslist)) {

    print(paste0("Iteration number ", i, "/", length(reslist)))

    resmod = reslist[[i]]
    optimum = optima[i, ]

    if (!is.null(models)) {
      model = models[[i]]
    } else {
      model = NULL
    }


    st = Sys.time()

    for (feature in names(resmod)) {

      res = resmod[[feature]]

      res.pdp = res$res.pdp
      res.ice = res$res.ice

      # Get the ground-truth
      if (!is.vector(gtdata[[feature]][[1]])) {
        gt.pdp = setDT(gtdata[[feature]][[1]])[.type == "pdp", ]
        gt.ice = setDT(gtdata[[feature]][[1]])[.type == "ice", ]
      } else {
        gt.pdp = setDT(gtdata[[feature]])[.type == "pdp", ]
        gt.ice = setDT(gtdata[[feature]])[.type == "ice", ]        
      }

      tree = res$trees[[1]]

      source_node = tree[[1]][[1]]

      eval.source_node = get_eval_measures_mlp(
        res.ice = res.ice, 
        gt.ice = gt.ice, 
        testdata = testdata, 
        idx = source_node$subset.idx, 
        pdp.feature = feature, 
        optimum = optimum[feature], 
        model = model)   

      names(eval.source_node) = paste0("source.", names(eval.source_node))

      for (depth in seq(2, length(tree))) {
        
        subtree = tree[seq_len(depth)]
        node = find_optimal_node(subtree, optimum)

        eval.opt = get_eval_measures_mlp(
          res.ice = res.ice, 
          gt.ice = gt.ice, 
          testdata = testdata, 
          idx = node$subset.idx, 
          pdp.feature = feature, optimum = optimum[feature], model = model)   

        values = cbind(model = i, feature = feature, depth = depth, eval.opt, eval.source_node)

        if (is.null(df)) {
          df = values
        } else {
          df = rbind(df, values)
        }
      }
    }

    et = Sys.time()
    print(et - st)

  }

  df$conf.rel = (df$source.conf.diff - df$conf.diff) / df$source.conf.diff
  df$gt.rel = (df$source.gt.diff.abs - df$gt.diff.abs) / df$source.gt.diff.abs
  df$gt.abs = df$source.gt.diff.abs - df$gt.diff.abs

  if ("conf.diff.cov" %in% names(df)) {
    df$conf.rel.cov = (df$source.conf.diff.cov - df$conf.diff.cov) / df$source.conf.diff.cov
    df$gt.rel.cov = (df$source.gt.diff.abs.cov - df$gt.diff.abs.cov) / df$source.gt.diff.abs.cov
    df$gt.abs.cov = df$source.gt.diff.abs.cov - df$gt.diff.abs.cov
  }


  for (nn in 1:3) {
    df[, paste0("conf.rel.opt.", nn)] = (df[, paste0("source.conf.diff.opt.", nn)] - df[, paste0("conf.diff.opt.", nn)]) / df[, paste0("source.conf.diff.opt.", nn)]
    df[, paste0("gt.abs.opt.", nn)] = (df[, paste0("source.gt.diff.abs.opt.", nn)] - df[, paste0("gt.diff.abs.opt.", nn)]) 
  }

  df$neg_loglik.rel = (df$source.neg_loglik - df$neg_loglik) / df$source.neg_loglik

  return(df)
}

compute_sampling_bias = function(mbo.result) {
        
        opt.path = as.data.frame(mbo.result$opt.path)
        dim = which(names(opt.path) == "y") - 1
        ps = mbo.result$opt.path$par.set
        ids = getParamIds(ps, repeated = TRUE, with.nr = TRUE)
        df1 = as.data.frame(opt.path)[, ids]
        df2 = as.data.frame(generateRandomDesign(n = nrow(df1), ps))
        
        mmd2(df1, df2)
}


cross.kernel = function(d1, d2 = NULL, sigma) {
  checkmate::assert_data_frame(d1, any.missing = FALSE)
  checkmate::assert_data_frame(d2, null.ok = TRUE, ncols = ncol(d1), any.missing = FALSE)
  checkmate::assert_number(sigma, lower = 0)
  mradial = kernlab::rbfdot(sigma = sigma)
  mm = kernlab::kernelMatrix(mradial, as.matrix(d1), as.matrix(d2))
  mean(mm)
}


get_median_dist = function(d){
  checkmate::assert_data_frame(d)
  dists = dist(d, diag = FALSE, upper = FALSE, method = "euclidean")
  median(dists)
}

mmd2 = function(d1, d2, sigma = NULL) {
  checkmate::assert_data_frame(d1, any.missing = FALSE)
  checkmate::assert_data_frame(d2, null.ok = TRUE, ncols = ncol(d1), any.missing = FALSE)
  checkmate::assert_number(sigma, lower = 0, null.ok = TRUE)
  d1 = data.frame(model.matrix(~ . -1, data = d1))
  d2 = data.frame(model.matrix(~ . -1, data = d2))
  if(is.null(sigma)) {
    sigma = get_median_dist(rbind(d1, d2))
    # Confusingly, the sigma in rbfdot is acutally the gamma param
    sigma = 1/(2 * sigma^2)
  }
  cross.kernel(d1, d1, sigma = sigma) - 2 * cross.kernel(d1, d2, sigma = sigma) + cross.kernel(d2, d2, sigma = sigma)
}



get_gp_uncertainty = function(gg, idx, returnC = FALSE) {
  

  # Compute the covariance matrix at this point 

  idx.subspace = which(gg$.id %in% idx)
  idx.cond = setdiff(seq_row(gg), idx.subspace)

  # compute the latter part
  if (length(idx.cond) == 0) {
    Ccond = C
  } else {
    # Ct = solve(C[idx.cond, idx.cond], t(C[idx.subspace, idx.cond]))

    # Compute the uncertainty estimate 
    Ccond = C[idx.subspace, idx.subspace] # - crossprod(t(C[idx.subspace, idx.cond]), Ct)
  }


  ggsub = setDT(gg[idx.subspace, ])

  # At every grid point, compute the sum of all covariances 
  out = lapply(gridvalues, function(gv) {

    idxs = which(ggsub[[feature]] %in% gv)
    Ccondidx = Ccond[idxs, idxs]
    mean(Ccondidx)
  })

  out = unlist(out)

  sqrt(out)
}



# determine optimal node of last split of tree
find_optimal_node = function(tree, optimum){
  # tree:     return value of compute_tree function
  # optimum:  named vector with values of best configuration found during training
  
  # output:   node of final split containing optimum
  
  node = tree[[1]][[1]]
  
  max_depth = length(tree) 
  
  while(node$depth < max_depth) {
    
    split.feature = node$split.feature
    split.value = node$split.value
    
    if (!is.null(split.value) & !is.null(split.value) & !is.null(optimum[split.feature])) {
      if(optimum[split.feature] <= split.value){
        node = node$children$left.child
      } else {
        node = node$children$right.child
      }
    } else {
      break
    }
  }
  
  return(node)
}

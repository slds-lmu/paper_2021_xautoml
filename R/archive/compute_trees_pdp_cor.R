

compute_trees_pdp_cor = function(n.split, models, features, testdata, grid.size, objectives) {
  
  # Compute trees for a list of models and a list of objectives on a fixed dataset.

  reslist = list()

  for (i in seq_along(models)) {

    print(paste("Model number", i))

    model = models[[i]]

    results_for_features = lapply(features, function(feature) {

      # Compute an effect just to get the grid points 
      predictor = Predictor$new(model = model, data = as.data.frame(testdata)[, model$features])
      effect_mean = FeatureEffect$new(predictor = predictor, feature = feature, method = "pdp+ice", grid.size = grid.size)
            
      effect_mean_d = setDT(effect_mean$results)
      names(effect_mean_d)[2] = "mean"

      sf = c(feature, "mean", ".id")
      res.pdp = effect_mean_d[.type == "pdp", ..sf]
      res.ice = effect_mean_d[.type == "ice", ..sf]

      split.feats = setdiff(names(testdata), feature)





      trees = lapply(objectives, function(objective) {
        compute_tree(effect = effect_sd, testdata = testdata, objective = objective, n.split = n.split) 
      })

      list(res.pdp = res.pdp, res.ice = res.ice, trees = trees)
    })

    names(results_for_features) = features

    reslist[[i]] = results_for_features
  }

  reslist
}


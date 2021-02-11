reduce_results_surrogate = function(reg, problems = NULL, savedir) {

  if (!dir.exists(savedir))
    dir.create(savedir)

  tab = summarizeExperiments(
    by = c("job.id", "algorithm", "problem"))


  toreduce = ijoin(tab, findDone())

  if (is.null(problems)) {
    problems = unique(toreduce$problem)
  }

    for (prob in problems) {

      print(prob)

      savepath = file.path(savedir, prob, "0_objective")

      if (!dir.exists(savepath))
        dir.create(file.path(savepath), recursive = TRUE)

      tored = toreduce[problem == prob, ]

	  res = reduceResultsDataTable(tored, function(x) {
	    model_val_balanced_accuracy = x$opdf_val_balanced_accuracy[final.model.avail == TRUE, ]$model
	    model_test_balanced_accuracy = x$opdf_test_balanced_accuracy[final.model.avail == TRUE, ]$model
	    return(list(model_val_balanced_acc = model_val_balanced_accuracy, model_test_balanced_acc = model_test_balanced_accuracy))
	  })

      res = ijoin(tab, res)    
      
      saveRDS(res, file.path(savepath, "surrogate.rds"))

    }
  }

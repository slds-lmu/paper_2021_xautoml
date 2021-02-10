reduce_results_synthetic = function(reg, problems = NULL, savedir) {

  if (!dir.exists(savedir))
    dir.create(savedir)

  tab = summarizeExperiments(
    by = c("job.id", "algorithm", "problem", "lambda", "objective", "n.splits"))


  toreduce = ijoin(tab, findDone())

  if (is.null(problems)) {
    problems = unique(toreduce$problem)
  }

  for (obj in unique(toreduce$objective)) {

    for (prob in problems) {

      print(prob)

      savepath = file.path(savedir, prob)

      if (!dir.exists(savepath))
        dir.create(savepath)

      tored = toreduce[problem == prob & objective == obj, ]

      res = reduceResultsDataTable(tored) 
      res = ijoin(tab, res)    
      
      saveRDS(res, file.path(savepath, paste0("eval_", obj, ".rds")))
    }
  }
}

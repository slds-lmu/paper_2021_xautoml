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

      res = reduceResultsDataTable(tored, function(x) {
        opt.path = x$opt.path
        dim = which(names(opt.path) == "y") - 1
        ps = makeParamSet(makeNumericVectorParam(id = "x", len = dim, lower = - 5, upper = 5))      
        ids = getParamIds(ps, repeated = TRUE, with.nr = TRUE)
        df1 = as.data.frame(opt.path)[, ids]
        df2 = as.data.frame(generateRandomDesign(n = nrow(df1), ps))
        x$mmd2 = mmd2(df1, df2)

        return(x)
      })

      res = ijoin(tab, res)    
      
      saveRDS(res, file.path(savepath, paste0("eval_", obj, ".rds")))
    }
  }
}


toreduce = ijoin(tab, findDone())

# toreduce = toreduce[objective %in% c("SS_L2", "SS_sd"), ]
toreduce = toreduce[n.splits == 5, ]


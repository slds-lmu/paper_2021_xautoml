reduce_results_mlrmbo = function(reg, problems = NULL, savedir) {

  if (!dir.exists(savedir))
    dir.create(savedir)

  tab = summarizeExperiments(
    by = c("job.id", "algorithm", "problem", "lambda"))


  toreduce = ijoin(tab, findDone())

  if (is.null(problems)) {
    problems = unique(toreduce$problem)
  }

for (prob in problems) {

    print(prob)

    savepath = file.path(savedir, prob, "1_1_mlrmbo_runs")

    if (!dir.exists(savepath))
      dir.create(file.path(savepath), recursive = TRUE)

    tored = toreduce[problem == prob & algorithm == "mlrmbo", ]

    res = reduceResultsDataTable(tored) 
    res = ijoin(tab, res)    

    if (!dir.exists(file.path(savedir, prob))) {
      dir.create(file.path(savedir, prob))
    }

    for (lamb in unique(res[algorithm == "mlrmbo", ]$lambda)) {
      res_tostore = res[algorithm == "mlrmbo" & lambda == lamb, ]

      saveRDS(res_tostore, file.path(savedir, prob, "1_1_mlrmbo_runs", paste0("mlrmbo_run_lambda_", lamb, "_30repls.rds")))
    }
  }
}

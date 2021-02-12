reduce_results_gt_pdp = function(reg, problems = NULL, savedir) {

  tab = summarizeExperiments(
    by = c("job.id", "algorithm", "problem"), reg = reg)

  if (is.null(problems))
    problems = unique(tab$problem)

  for (prob in problems) {

    toreduce = tab[problem %in% prob, ]
    toreduce = ijoin(toreduce, findDone())

    if (nrow(toreduce) > 0) {
      res = reduceResultsDatatable(toreduce)[[1]]

      path = file.path(savedir, prob)

      if (!dir.exists(path))
        dir.create(path, recursive = TRUE)

      # Store the objective function 
      saveRDS(res["surr_optima"], file.path(path, "0_objective", "surrogate_optima.rds"))
      saveRDS(res["obj"], file.path(path, "0_objective", "obj.rds"))

      # extract the grid.size and the testdata size
      gtpdp = res["pdp_ice_groundtruth"]
      exm = gtpdp$pdp_ice_groundtruth$batch_size
      testdata.size = max(exm$.id, na.rm = TRUE)
      grid.size = length(which(exm$.type == "pdp"))

      dir.create(file.path(path, "2_2_groundtruth_pdps"), recursive = TRUE)

      if (!dir.exists(file.path(path, "2_2_groundtruth_pdps")))
        dir.create(file.path(path, "2_2_groundtruth_pdps"), recursive = TRUE)

      saveRDS(res["pdp_ice_groundtruth"], file.path(path, "2_2_groundtruth_pdps", paste0("gtpdp_", grid.size, "_", testdata.size, ".rds")))

    }
  }

}

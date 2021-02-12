reduce_trees = function(reg, problems = NULL, savedir) {

	tab = summarizeExperiments(
		by = c("job.id", "algorithm", "problem", "lambda", "objective"), reg = reg)

	if (is.null(problems))
		problems = unique(tab$problem)


	for (obj in unique(tab$objective)) {

		for (prob in unique(tab$prob)) {

		    toreduce = tab[problem %in% prob & objective == obj, ]
		    toreduce = ijoin(toreduce, findDone())

		    if (nrow(toreduce) > 0) {

		    	# We only store the evaluation metrics
				res = reduceResultsDataTable(toreduce, function(x) x$eval)
			    res = ijoin(tab, res)    

				path = file.path(savedir, prob, "2_3_effects_and_trees")

				if (!dir.exists(path))
					dir.create(path, recursive = TRUE)
	      
	      		savepath = file.path(path, paste0("eval_", obj, "_", grid.size, "_", testdata.size, ".rds"))
	       
	       		saveRDS(res, savepath)    
			}
		}
	}
}


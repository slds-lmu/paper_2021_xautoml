data_dir = "data/runs/xgboost"

library(batchtools)
library(ggplot2)
library(iml)

source("R/xgboost_config.R")
source("R/helper.R")

lapply(packages, require, character.only = TRUE)



# --- 1. REDUCE RESULTS ---

# scp -r ru59sol2@lxlogin1.lrz.de:/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru59sol2/repos/xautoml/xgboost_registry/ .

reg = loadRegistry(registry_name, writeable = FALSE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "lrn", "problem", "lambda"))

for (prob in c("blood-transfusion-service-center", "kc1")) {
	subtab = ijoin(tab[problem == prob, ], findDone())
	res = reduceResultsDataTable(subtab, function(x) {
		models = x$res$models
		list(models = models[length(models)], x$res$opt.path)
	})
	res = ijoin(tab, res, by = "job.id")
	saveRDS(res, file.path(data_dir, prob, "res.rds"))
}





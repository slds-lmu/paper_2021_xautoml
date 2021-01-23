# --- 0. SETUP ---

source("R/helper.R")

# - test or real setup for better testing - 
SETUP = "TEST"

switch(SETUP, 
	"TEST" = {
		# overwrite registry
		OVERWRITE = TRUE
		# termination criterion for each run
		RUNTIME_MAX = 60L
    # registry name for storing files on drive 
		registry_name = "results/tree_splitting_test" 
	},
	"REAL" = {
		# overwrite registry?
		OVERWRITE = FALSE
		# termination criterion for each run
		RUNTIME_MAX = 302400
    # registry name for storing files on drive     
		registry_name = "results/tree_splitting"
	}
)

# - packages - 
packages = c(
  "batchtools",  
  "mlr", 
  "mlrMBO", 
  "smoof", 
  "data.table", 
  "iml",
  "ranger",
  "BBmisc"
) 

lapply(packages, library, character.only = TRUE)


# --- 1. PROBLEM DESIGN ---

TASK_LOCATION = "data/runs/mlp_new/"

tasks = list.dirs(TASK_LOCATION, full.names = FALSE, recursive = FALSE)

pdes = data.table(tasks = tasks)


# --- 2. ALGORITHM DESIGN ---

perform_tree_splitting = function(data, job, instance, grid.size, testdata.size, objective, n.splits, lambda) {

	source("/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru59sol2/repos/paper_2020_xautoml/R/mlp_helper.r")
	source("/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru59sol2/repos/paper_2020_xautoml/R/helper_evaluation.r")

	# Get all ground-truth information
	obj = readRDS(file.path(instance, "0_objective", "obj.rds"))$obj
  	surr_val = readRDS(file.path(instance, "0_objective", "surrogate.rds"))$result[[1]]$model_val_balanced_acc
	ps = getParamSet(obj)

	# Get the model we want to perform our analysis on 
	rundata = readRDS(file.path(instance, "1_1_mlrmbo_runs", paste0("mlrmbo_run_lambda_", lambda, "_30repls.rds")))
	models = lapply(rundata$result, function(res) {
		mods = res$models
		mods[[length(mods)]] # get the last model 
	})
	features = models[[1]]$features

	# Read in the ground-truth
	gtdata = readRDS(file.path(instance, "2_2_groundtruth_pdps", paste0("gtpdp_", grid.size, "_", testdata.size, ".rds")))$pdp_ice_groundtruth
	gtdata = lapply(gtdata, function(el) {
		elt = el
		elt[[2]] = elt[[2]][method %in% c(paste0("mlrmbo_lambda", lambda)), ]
		elt
	})

	# Found optimal values for the different mbo runs 
	path = "data/runs/mlp_new/"
	folder_mlp = strsplit(instance, "/")[[1]][4]
	mbo_runs = get_data(path, folder_mlp, lambda = lambda)
	mbo_optima = get_optima(mbo_runs)[[folder_mlp]]

	mbo_optima = do.call(rbind, mbo_optima)
	mbo_optima$iter = seq_len(nrow(mbo_optima))

	# Read in the test data for the pdp 
	testdata = readRDS(file.path(instance, "2_1_testdata", paste0("testdata_", testdata.size, ".rds")))

	start_t = Sys.time()
	reslist = compute_trees(n.split = n.splits, models = models[1:2], features = features, optima = mbo_optima, testdata = testdata)
    end_t = Sys.time()


    return(list(
    	reslist = reslist, 
    	runtime = as.integer(end_t) - as.integer(start_t)
    	)
    )
}


ALGORITHMS = list(
    compute_ground_truth_pdp = list(fun = compute_ground_truth_pdp, ades = data.table(grid.size = 20, testdata.size = 1000))
)

ades = lapply(ALGORITHMS, function(x) x$ades)

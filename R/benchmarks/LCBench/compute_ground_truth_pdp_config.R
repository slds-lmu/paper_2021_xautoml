# --- 0. SETUP ---

source("R/helper.R")

# - test or real setup for better testing - 
SETUP = "REAL"

switch(SETUP, 
	"TEST" = {
		# overwrite registry
		OVERWRITE = TRUE
		# termination criterion for each run
		RUNTIME_MAX = 60L
    # registry name for storing files on drive 
		registry_name = "results/mlp_ground_truth_pdp" 
	},
	"REAL" = {
		# overwrite registry?
		OVERWRITE = FALSE
		# termination criterion for each run
		RUNTIME_MAX = 302400
    # registry name for storing files on drive     
		registry_name = "results/mlp_ground_truth_pdp_test"
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

TASK_LOCATION = "data/runs/mlp/"

tasks = list.dirs(TASK_LOCATION, full.names = FALSE)
tasks = tasks[2:length(tasks)]

pdes = data.table(tasks = tasks)


# --- 2. ALGORITHM DESIGN ---

# - A - mlrMBO tuning   

# The objective function is just the evaluation of a configuration on the surrogate meta-model. 
# The initial design is fixed, to reduce the variation in the runs. 

compute_ground_truth_pdp = function(data, job, instance, grid.size, testdata.size) {

	source("../../pdp_helpers2.R")

	# Ground-truth
	surrogate_data = readRDS(file.path(instance, "0_objective/surrogate.rds"))
	surr_val = surrogate_data$result[[1]]$model_val_balanced_acc
	surr_test = surrogate_data$result[[1]]$model_test_balanced_acc[[1]]

	obj =  readRDS(file.path(instance, "0_objective/obj.rds"))
	ps = getParamSet(obj)

	features = getParamIds(ps)

	# Compute some test we do our computations on 
	testdata_path = file.path(instance, paste0("2_1_testdata/testdata_", testdata.size, ".rds"))

	if (file.exists(testdata_path)) {
	  testdata = readRDS(testdata_path)
	} else {
	  testdata = generateRandomDesign(n = testdata.size, par.set = ps) # TODO: n must be much higher, this is just for testing
	  saveRDS(testdata, testdata_path)
	}

	# TODO: Adapt with regards to the optima that need to be returned

	st = Sys.time()
    gtdata = lapply(features, function(feature) {
      marginal_effect_mlp(obj = obj, feature = feature, data = testdata, all.features = features, grid.size = grid.size, method = "pdp+ice")
    })
    et = Sys.time()

    names(gtdata) = features

    print(et - st)


    return(list(
    	pdp_ice_groundtruth = gtdata, 
    	runtime = as.integer(end_t) - as.integer(start_t)
    	)
    )
}


ALGORITHMS = list(
    compute_ground_truth_pdp = list(fun = compute_ground_truth_pdp, ades = data.table(grid.size = 20, testdata.size = 1000))
)

ades = lapply(ALGORITHMS, function(x) x$ades)

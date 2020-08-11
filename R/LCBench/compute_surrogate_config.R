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
		registry_name = "LCBench_surrogate_registry_temp" 
	},
	"REAL" = {
		# overwrite registry?
		OVERWRITE = FALSE
		# termination criterion for each run
		RUNTIME_MAX = 302400
    # registry name for storing files on drive     
		registry_name = "LCBench_surogate_registry"
	}
)

# - packages - 
packages = c(
  "batchtools",  
  "mlr", 
  "mlrMBO", 
  "smoof", 
  "data.table"
) 

lapply(packages, library, character.only = TRUE)


# --- 1. PROBLEM DESIGN ---

TASK_LOCATION = "data/runs/mlp/"

tasks = list.dirs(TASK_LOCATION, full.names = FALSE)
tasks = tasks[2:length(tasks)]

# --- 2. ALGORITHM DESIGN ---

randomsearch = function(data, job, instance
    # algorithm-specific parameters
    ) {

	lrn = makeLearner("regr.ranger")

	# read LCBench data 
	df = read.csv2(instance, sep = ",")

	search_space_ids = c("batch_size", "max_dropout", "max_units", "num_layers", "learning_rate", 
		"momentum", "weight_decay")

	ps_surrogate = makeParamSet(
			makeNumericParam("num.trees", lower = log(10, 2), upper = log(500, 2), trafo = function(x) round(2^x)), # 2^13 = 8192	
			makeLogicalParam("do.mtry"),
			makeIntegerParam("min.node.size", lower = 1L, upper = 5L),
			makeIntegerParam("num.random.splits", lower = 1, upper = 100)
		)

	# Build two models: one for validation accuracy, on for test accuracy 
	obj = c("final_val_balanced_accuracy", "final_test_balanced_accuracy")

	
	opdf_val = perform_random_search(df = df, search_space_ids = search_space_ids, 
		ps_surrogate = ps_surrogate, objective = obj[1], max_evals = 500, resampling = cv3)
	
	opdf_test = perform_random_search(df = df, search_space_ids = search_space_ids, 
		ps_surrogate = ps_surrogate, objective = obj[2], max_evals = 500, resampling = cv3)


	return(list(opdf_val_balanced_accuracy = opdf_val, opdf_test_balanced_accuracy = opdf_test))

}

ALGORITHMS = list(
    randomsearch = list(fun = randomsearch, ades = data.table())
)

ades = lapply(ALGORITHMS, function(x) x$ades)



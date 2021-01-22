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

TASK_LOCATION = "data/runs/mlp_new/"

tasks = list.dirs(TASK_LOCATION, full.names = FALSE)
tasks = tasks[2:length(tasks)]

pdes = data.table(tasks = tasks)


# --- 2. ALGORITHM DESIGN ---

# - A - mlrMBO tuning   

# The objective function is just the evaluation of a configuration on the surrogate meta-model. 
# The initial design is fixed, to reduce the variation in the runs. 

compute_ground_truth_pdp = function(data, job, instance, grid.size, testdata.size) {

	source("/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru59sol2/repos/paper_2020_xautoml/R/pdp_helpers2.R")
	source("/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru59sol2/repos/paper_2020_xautoml/R/mlp_helper.r")

  	surr_val = readRDS(file.path(instance, "0_objective", "surrogate.rds"))$result[[1]]$model_val_balanced_acc
  	lcbench = read.csv2(file.path(instance, "0_objective", "lcbench2000.csv"), sep = ",")
 
  	ps = makeParamSet(
          # do early stopping instead for the bigger datasets
            makeNumericParam("batch_size", lower = log(16, 2), upper = log(512, 2), trafo = function(x) round(2^x)), 
            makeNumericParam("max_dropout", lower = 0, upper = 1), 
            makeNumericParam("max_units", lower = log(64, 2), upper = log(1024, 2), trafo = function(x) round(2^x)), 
            makeIntegerParam("num_layers", lower = 1, upper = 5),
            makeNumericParam("learning_rate", lower = 0, upper = 0.01),
            makeNumericParam("momentum", lower = 0.1, upper = 1),
            makeNumericParam("weight_decay", lower = 0, upper = 0.1)
    )

	 obj = makeSingleObjectiveFunction(name = "mlp.surr.tuning",
	    fn = function(x) {

	    ps = makeParamSet(
	        # do early stopping instead for the bigger datasets
	          makeNumericParam("batch_size", lower = log(16, 2), upper = log(512, 2), trafo = function(x) round(2^x)), 
	          makeNumericParam("max_dropout", lower = 0, upper = 1), 
	          makeNumericParam("max_units", lower = log(64, 2), upper = log(1024, 2), trafo = function(x) round(2^x)), 
	          makeIntegerParam("num_layers", lower = 1, upper = 5),
	          makeNumericParam("learning_rate", lower = 0, upper = 0.01),
	          makeNumericParam("momentum", lower = 0.1, upper = 1),
	          makeNumericParam("weight_decay", lower = 0, upper = 0.1)
	      )

	      x = trafoValue(ps, x)
	      x = as.data.frame(x)
	      y = predict(surr_val[[1]], newdata = x)$data$response

	    return(mmce = 1 - y / 100)
	    },
	    par.set = ps,
	    noisy = TRUE,
	    has.simple.signature = FALSE,
	    minimize = TRUE
	)



	idx.min.lcbench = which.max(lcbench$final_val_balanced_accuracy)
	x.min.lcbench = lcbench[idx.min.lcbench, names(lcbench) %in% getParamIds(ps)]
	y.min.lcbench = 1 - as.numeric(as.character(lcbench[idx.min.lcbench, ]$final_val_balanced_accuracy)) / 100

	x.min.lcbench$batch_size = log(x.min.lcbench$batch_size, 2)
	x.min.lcbench$max_units = log(x.min.lcbench$max_units, 2)

	vals = sampleValues(n = 10^2, ps, trafo = FALSE)
	y = lapply(vals, obj)
	y = unlist(y)

	idx.min = which.min(y)
	x.min = as.data.frame(do.call(cbind, vals[[idx.min]]))
	y.min = y[idx.min]


	features = getParamIds(ps)

	# Compute some test we do our computations on 
	testdata_path = file.path(instance, paste0("2_1_testdata/testdata_", testdata.size, ".rds"))

	if (file.exists(testdata_path)) {
	  testdata = readRDS(testdata_path)
	} else {
	  testdata = generateRandomDesign(n = testdata.size, par.set = ps) # TODO: n must be much higher, this is just for testing
	  saveRDS(testdata, testdata_path)
	}

	# Because we want to evaluate the PDP in particular at the location of the optimum, 
	# we need to read in the optimal values that were found during optimization

	lambdas = c(0.5, 1, 2, 10)

	optima = lapply(lambdas, function(lambda) {
		path = "data/runs/mlp_new/"
		folder_mlp = strsplit(instance, "/")[[1]][4]
		data = get_data(path, folder_mlp, lambda = lambda)
		all_optima = get_optima(data)[[folder_mlp]]

		all_optima = do.call(rbind, all_optima)
		all_optima$iter = seq_len(nrow(all_optima))

		return(all_optima)
	})

	optima = do.call(rbind, optima)
	optima$method = paste0("mlrmbo_lambda", optima$lambda)

	surr_optima = x.min
	surr_optima$method = "theoretical_optimum"

	optima = dplyr::bind_rows(optima, surr_optima)
	optima = setDT(optima)

	start_t = Sys.time()
    gtdata = lapply(features, function(feature) {
      marginal_effect_mlp(obj = obj, feature = feature, data = testdata, all.features = features, grid.size = grid.size, optima = optima, method = "pdp+ice")
    })
    end_t = Sys.time()

    names(gtdata) = features

    print(end_t - start_t)


    return(list(
    	pdp_ice_groundtruth = gtdata, 
    	surr_optima = list(lcbench = list(x.min = x.min.lcbench, y.min = y.min.lcbench), randomforest = list(x.min = x.min, y.min = y.min)),
    	obj = obj,
    	runtime = as.integer(end_t) - as.integer(start_t)
    	)
    )
}


ALGORITHMS = list(
    compute_ground_truth_pdp = list(fun = compute_ground_truth_pdp, ades = data.table(grid.size = 5, testdata.size = 100))
)

ades = lapply(ALGORITHMS, function(x) x$ades)

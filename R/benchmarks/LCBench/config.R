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
		registry_name = "results/mlp_mlrmbo_registry_temp" 
	},
	"REAL" = {
		# overwrite registry?
		OVERWRITE = FALSE
		# termination criterion for each run
		RUNTIME_MAX = 302400
    # registry name for storing files on drive     
		registry_name = "results/mlp_mlrmbo_registry_fixed_initdes"
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

TASK_LOCATION = "data/runs/mlp_new/"

tasks = list.dirs(TASK_LOCATION, full.names = FALSE)
tasks = tasks[2:length(tasks)]

pdes = data.table(tasks = tasks)


# --- 2. ALGORITHM DESIGN ---

# - A - mlrMBO tuning   

# The objective function is just the evaluation of a configuration on the surrogate meta-model. 
# The initial design is fixed, to reduce the variation in the runs. 

mlrmbo = function(data, job, instance, lambda) {

	surrogate_data = readRDS(file.path(instance, "surrogate.rds"))
	surr_val = surrogate_data$result[[1]]$model_val_balanced_acc[[1]]
	surr_test = surrogate_data$result[[1]]$model_test_balanced_acc[[1]]

	# I also samples the variables on a log scale that were originally sampled on a log scale
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
	  	x = as.data.frame(x)
		y = predict(surr_val, newdata = x)$data$response

		attr(y, "extras") = list(test_performance = predict(surr_test, newdata = x)$data$response)
		return(1 - y / 100)
	  },
	  par.set = ps,
	  noisy = TRUE,
	  has.simple.signature = FALSE,
	  minimize = TRUE
	)

	ctrl = makeMBOControl(store.model.at = 1:200)
	ctrl = setMBOControlTermination(ctrl, max.evals = 200, time.budget = RUNTIME_MAX)
	ctrl = setMBOControlInfill(ctrl, makeMBOInfillCritCB(cb.lambda = lambda))

	set.seed(1234)
	des = generateDesign(n = 2 * length(ps$pars), par.set = ps, fun = lhs::randomLHS)

    start_t = Sys.time()
	res = mbo(obj, design = des, control = ctrl, show.info = TRUE)
    end_t = Sys.time()

    opdf = as.data.frame(res$opt.path)
    dob.best = opdf[res$best.ind, ]$dob

    models = res$models
    
    models = models[c(dob.best, length(models))]
    res$final.opt.state = NULL

    return(list(
    	opt.path = opdf, 
    	models = models, 
    	runtime = as.integer(end_t) - as.integer(start_t)
    	)
    )
}


# - A - randomsearch

# We want to compare explanations we get from an MBO run to a run that was evaluated on a random grid
# This is for separating the sampling bias from the explanations. 

randomsearch = function(data, job, instance
    # algorithm-specific parameters
    ) {

	surrogate_data = readRDS(file.path(instance, "0_objective/surrogate.rds"))
	surr_val = surrogate_data$result[[1]]$model_val_balanced_acc[[1]]
	surr_test = surrogate_data$result[[1]]$model_test_balanced_acc[[1]]

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
	  	x = as.data.frame(x)
		y = predict(surr_val, newdata = x)$data$response

		attr(y, "extras") = list(test_performance = predict(surr_test, newdata = x)$data$response)
		return(1 - y / 100)
	  },
	  par.set = ps,
	  noisy = TRUE,
	  has.simple.signature = FALSE,
	  minimize = TRUE
	)

	ctrl = makeMBOControl(store.model.at = 1:201)
	ctrl = setMBOControlTermination(ctrl, max.evals = 201, time.budget = RUNTIME_MAX)

	set.seed(1234)
	des = generateRandomDesign(n = 200, par.set = ps)

    start_t = Sys.time()
	res = mbo(obj, design = des, control = ctrl, show.info = TRUE)
    end_t = Sys.time()

    opdf = as.data.frame(res$opt.path)
    opdf = opdf[seq_len(nrow(opdf) - 1), ]

    models = res$models
    
    models = models[1]
    res$final.opt.state = NULL

    return(list(
    	opt.path = opdf, 
    	models = models, 
    	runtime = as.integer(end_t) - as.integer(start_t)
    	)
    )
}

ALGORITHMS = list(
    mlrmbo = list(fun = mlrmbo, ades = data.table(lambda = c(0.5, 1, 2, 10))),
    randomsearch = list(fun = randomsearch, ades = data.table())
)

ades = lapply(ALGORITHMS, function(x) x$ades)

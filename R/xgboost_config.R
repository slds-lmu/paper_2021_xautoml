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
		registry_name = "xgboost_registry_temp" 
	},
	"REAL" = {
		# overwrite registry?
		OVERWRITE = FALSE
		# termination criterion for each run
		RUNTIME_MAX = 259200L
    # registry name for storing files on drive     
		registry_name = "xgboost_registry"
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

TASK_LOCATION = "data/raw/"

tasks = list.dirs(TASK_LOCATION, full.names = FALSE)
tasks = tasks[2:length(tasks)]

PROBLEMS = list(
    xgboost = list(
    	learner = makeLearner(
			"classif.xgboost", 
			id = "classif.xgboost", 
			eval_metric = "error", 
			objective = "binary:logistic"),
    	ps = makeParamSet(
			# do early stopping instead for the bigger datasets
		  	makeNumericParam("nrounds", lower = 0, upper = 13, trafo = function(x) round(2^x)), # 2^13 = 8192	
		  	makeNumericParam("eta", lower = -7, upper = 0, trafo = function(x) 2^x), # 2^(-7) = 0.007 < 0.01
		  	makeNumericParam("gamma", lower = -7, upper = 6, trafo = function(x) 2^x), 
		  	makeIntegerParam("max_depth", lower = 3, upper = 20),
		  	makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
		  	makeNumericParam("colsample_bylevel", lower = 0.5, upper = 1),
		  	makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
		  	makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
		  	makeNumericParam("subsample", lower = 0.5, upper = 1)
		)
	)
)

# TODO: OUTER RESAMPLING 

pdes = lapply(tasks, function(x) data.table(lrn = c("xgboost")))
names(pdes) = tasks




# --- 2. ALGORITHM DESIGN ---

# - A - mlrMBO tuning   
mlrmbo = function(data, job, instance,
    # algorithm-specific parameters
    lambda) {

	ps = instance$ps

	obj = makeSingleObjectiveFunction(name = "xgb.tuning",
	  fn = function(x) {
		lrn = instance$learner 
		task = instance$task
	    resample(lrn, task, cv10, show.info = FALSE)$aggr
	  },
	  par.set = ps,
	  noisy = TRUE,
	  has.simple.signature = FALSE,
	  minimize = TRUE
	)

	ctrl = makeMBOControl(store.model.at = 1:200)
	ctrl = setMBOControlTermination(ctrl, max.evals = 200)
	ctrl = setMBOControlInfill(ctrl, makeMBOInfillCritCB(cb.lambda = lambda))

	des = generateDesign(n = 2 * length(ps$pars), par.set = ps, fun = lhs::randomLHS)

    start_t = Sys.time()
	res = mbo(obj, design = des, control = ctrl, show.info = TRUE)
    end_t = Sys.time()

    return(list(
      res = res,
      runtime = as.integer(end_t) - as.integer(start_t)
    ))
}


# - B - create ground-truth dataset 

randomsearch = function(data, job, instance
    # algorithm-specific parameters
    ) {

	ps = instance$ps

	obj = makeSingleObjectiveFunction(name = "xgb.tuning",
	  fn = function(x) {
		lrn = instance$learner 
		task = instance$task
	    resample(lrn, task, cv10, show.info = FALSE)$aggr
	  },
	  par.set = ps,
	  noisy = TRUE,
	  has.simple.signature = FALSE,
	  minimize = TRUE
	)

	des = generateDesign(n = 10000, par.set = ps, fun = lhs::randomLHS)

	ctrl = makeMBOControl()
	ctrl = setMBOControlTermination(ctrl, max.evals = 9999)

    start_t = Sys.time()
	res = mbo(obj, design = des, control = ctrl, show.info = TRUE)
    end_t = Sys.time()

    return(list(
      res = res,
      runtime = as.integer(end_t) - as.integer(start_t)
    ))
}

ALGORITHMS = list(
    mlrmbo = list(fun = mlrmbo, ades = data.table(lambda = c(0.5, 1, 2))),
    randomsearch = list(fun = randomsearch, ades = data.table())
)

ades = lapply(ALGORITHMS, function(x) x$ades)


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
		registry_name = "mlp_mlrmbo_test" 
	},
	"REAL" = {
		# overwrite registry?
		OVERWRITE = FALSE
		# termination criterion for each run
		RUNTIME_MAX = 302400
    # registry name for storing files on drive     
		registry_name = "mlp_mlrmbo"
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

pdes = data.table(tasks = tasks)


# --- 2. ALGORITHM DESIGN ---

# - A - mlrMBO tuning   
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
		return(y)
	  },
	  par.set = ps,
	  noisy = TRUE,
	  has.simple.signature = FALSE,
	  minimize = FALSE
	)

	ctrl = makeMBOControl(store.model.at = 1:200)
	ctrl = setMBOControlTermination(ctrl, max.evals = 20, time.budget = RUNTIME_MAX)
	ctrl = setMBOControlInfill(ctrl, makeMBOInfillCritCB(cb.lambda = lambda))

	des = generateDesign(n = 2 * length(ps$pars), par.set = ps, fun = lhs::randomLHS)

    start_t = Sys.time()
	res = mbo(obj, design = des, control = ctrl, show.info = TRUE)
    end_t = Sys.time()

    opdf = as.data.frame(res$opt.path)
    dob.best = opdf[res$best.ind, ]$dob

    models = res$models
    
    res$models = models[c(dob.best, length(models))]
    res$final.opt.state = NULL

    return(list(
      res = res,
      runtime = as.integer(end_t) - as.integer(start_t)
    ))
}


ALGORITHMS = list(
    mlrmbo = list(fun = mlrmbo, ades = data.table(lambda = c(0.5, 1, 2, 10)))
)

ades = lapply(ALGORITHMS, function(x) x$ades)




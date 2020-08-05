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

	data = read.csv2(instance, sep = ",")

	search_space = c("batch_size", "max_dropout", "max_units", "num_layers", "learning_rate", 
		"momentum", "weight_decay")
	objective = "final_val_accuracy"

	task_data = data[, c(search_space, objective)]
	task_data$num_layers = ifelse(task_data$num_layers == "True", 5, task_data$num_layers)

	task_data = as.data.table(task_data)
	task_data = sapply(task_data, function(x) as.numeric(as.character(x)))
	task_data = as.data.table(task_data)
	task = makeRegrTask(data = task_data, target = objective)

	ps = makeParamSet(
			makeNumericParam("num.trees", lower = 0, upper = log(1000, 2), trafo = function(x) round(2^x)), # 2^13 = 8192	
			makeLogicalParam("do.mtry"),
			makeIntegerParam("min.node.size", lower = 1L, upper = 5L),
			makeIntegerParam("num.random.splits", lower = 1, upper = 100)
		)

	obj = makeSingleObjectiveFunction(name = "ranger.surrogate",
	  fn = function(x) {		
	  	if (!x$do.mtry) 
	  		x$mtry = getTaskNFeats(task)

	  	x$do.mtry = NULL
	  	x$splitrule = "extratrees"

		lrn = makeLearner("regr.ranger", par.vals = x)
	    resample(lrn, task, cv10, show.info = FALSE)$aggr
	  },
	  par.set = ps,
	  noisy = TRUE,
	  has.simple.signature = FALSE,
	  minimize = TRUE
	)

	des = generateRandomDesign(n = 1000, par.set = ps)

	# always add Marius proposal
	des = rbind(des, data.frame(num.trees = log(10, 2), do.mtry = FALSE, min.node.size = 1, num.random.splits = 10))

	ctrl = makeMBOControl()
	ctrl = setMBOControlTermination(ctrl, max.evals = 999)

	res = mbo(obj, design = des, control = ctrl, show.info = TRUE)

	# train a model for the best and for the last on the full dataset
	idx = c(which.min(des$y), 11)

	opdf = as.data.table(as.data.frame(res$opt.path))
	opdf$final.model.avail = FALSE
	opdf$model = list()
	opdf[idx, ]$final.model.avail = TRUE

	for (id in idx) {
		config = res$opt.path$env$path[id, ]
		config = config[, - which(names(config) %in% "y")]
		config = as.list(config)
		config.trafo = trafoValue(ps, config)
		config.trafo$do.mtry = NULL
		lrn = makeLearner("regr.ranger", par.vals = list(splitrule = "extratrees"))
		lrn.tuned = setHyperPars2(lrn, config.trafo)

		mod.tuned = train(lrn.tuned, task)

		opdf[id, ]$model[[1]] = mod.tuned
	}

	return(opt.path = opdf)

}

ALGORITHMS = list(
    randomsearch = list(fun = randomsearch, ades = data.table())
)

ades = lapply(ALGORITHMS, function(x) x$ades)

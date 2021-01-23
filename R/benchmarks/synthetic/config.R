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
		registry_name = "results/synthetic_tree_splitting_test" 
	},
	"REAL" = {
		# overwrite registry?
		OVERWRITE = FALSE
		# termination criterion for each run
		RUNTIME_MAX = 302400
    # registry name for storing files on drive     
		registry_name = "results/synthetic_tree_splitting"
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

TASK_LOCATION = "data/runs/synthetic/"

tasks = c("StyblinskiTang")
dimensions = c(2, 3, 5, 10)


pdes = data.table(tasks = tasks, dimensions = dimensions)


# --- 2. ALGORITHM DESIGN ---

perform_tree_splitting_synthetic = function(data, job, instance, grid.size, testdata.size, n.splits, lambda, objective) {

	# source("/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru59sol2/repos/paper_2020_xautoml/R/mlp_helper.r")
	# source("/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru59sol2/repos/paper_2020_xautoml/R/helper_evaluation.r")

	source("R/pdp_helpers.R")
	source("R/helper_evaluation.r")
	source("R/tree_splitting.R")

	obj = readRDS(file.path(instance, "objective.rds"))
    ps = getParamSet(obj)
    pl = getParamLengths(ps)

    max.evals = switch(which(pl == c(2, 3, 5, 10)), 50, 80, 150, 250)

    if (lambda == 1000) {
        init_design = max.evals
        max.evals = max.evals + 1
    } else {
    	init_design = 4 * pl
    }

    ctrl = makeMBOControl(store.model.at = 1:10000)
    ctrl = setMBOControlTermination(ctrl, max.evals = max.evals)
    ctrl = setMBOControlInfill(ctrl, makeMBOInfillCritCB(cb.lambda = lambda))



	set.seed(1234)        
    des = generateDesign(n = init_design, par.set = ps, fun = lhs::randomLHS)
    
    res = mbo(obj, design = des, control = ctrl, show.info = FALSE)
    opdf = as.data.frame(res$opt.path)

    models = res$models
    models = tail(models, 1)
    features = models[[1]]$features

	mbo_optima = opdf[which.min(opdf$y), ]

	set.seed(123456)
	testdata = generateRandomDesign(n = testdata.size, par.set = ps)

	start_t = Sys.time()

	reslist = compute_trees(
		n.split = n.splits, 
		models = models, 
		features = features, 
		testdata = testdata, 
		grid.size = grid.size, 
		objective = objective
	) 

    end_t = Sys.time()

	# Read in the ground-truth 
    gtdata = lapply(features, function(feature) {
	    list(marginal_effect(obj = obj, feature = feature, data = testdata, all.features = features, grid.size = grid.size, method = "pdp+ice") )
    })

    names(gtdata) = features

	eval = evaluate_results(reslist, mbo_optima, gtdata)

    return(list(
    	mbo_run = res,
    	reslist = reslist, 
    	eval = eval, 
    	runtime = as.integer(end_t) - as.integer(start_t)
    	)
    )
}

ades = data.table(grid.size = 20, testdata.size = 1000, n.splits = 7, lambda = c(0.1, 1, 2, 5, 10, 1000))
grid = expand.grid(seq(1, nrow(ades)), objective = c("SS_sd", "SS_area", "SS_L1", "SS_L2"))
ades = cbind(ades[grid$Var1, ], objective = grid$objective)

ALGORITHMS = list(
    perform_tree_splitting_synthetic = list(fun = perform_tree_splitting_synthetic, 
    	ades = ades
))

ades = lapply(ALGORITHMS, function(x) x$ades)

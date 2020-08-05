library(mlrMBO)
library(mlr)
library(data.table)

path = "data/runs/mlp/"
task = "blood-transfusion-service-center"

# Fit a ranger random forest 
lrn = makeLearner("regr.ranger")

data = read.csv2(file.path(path, task, "lcbench2000.csv"), sep = ",")

search_space = c("batch_size", "max_dropout", "max_units", "num_layers", "learning_rate", 
	"momentum", "weight_decay")
objective = "final_val_accuracy"

task_data = data[, c(search_space, objective)]
task_data$num_layers = ifelse(task_data$num_layers == "True", 5, task_data$num_layers)

task_data = as.data.table(task_data)
task_data = sapply(task_data, function(x) as.numeric(as.character(x)))
task_data = as.data.table(task_data)
task = makeRegrTask(data = task_data, target = objective)

par.set = makeParamSet(
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
    resample(lrn, task, cv3, show.info = FALSE)$aggr
  },
  par.set = par.set,
  noisy = TRUE,
  has.simple.signature = FALSE,
  minimize = TRUE
)

des = generateRandomDesign(n = 1000, par.set = par.set)

ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, max.evals = 999)

res = mbo(obj, design = des, control = ctrl, show.info = TRUE)



# play around with 
# num.trees
# mtry
# num.random.splits

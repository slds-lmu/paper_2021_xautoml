# Size of the dataset to fit a model to a randomLHS

library(mlrMBO)

instance = "data/runs/mlp/adult/"

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



set.seed(1234)
des = sampleValues(par = ps, n = 5000, trafo = TRUE)
y = lapply(des, obj)
des = lapply(des, unlist)
des = do.call(rbind, des)
des = as.data.frame(des)
des$y = unlist(y)

time_est = data.frame(size = c(100, 200, 500, 1000, 2000, 3000))
time_est$time = NA
time_est$mae = NA

test_ids = seq(2501, 5000)

lrn = makeLearner("regr.km", predict.type = "se", par.vals = list(covtype="matern3_2", nugget.estim = TRUE, optim.method="gen"))

for (i in seq_len(nrow(time_est))) {

	task = makeRegrTask(data = des[seq_len(time_est[i, ]$size), ], target = "y")
	bla = train(lrn, task)
	time_est[i, ]$time = bla$time

	pred = predict(bla, newdata = des[seq_len(time_est[i, ]$size), ])

	time_est[i, ]$mae = performance(pred, mae)

}



# Sanity check of data 

data = readRDS("data/runs/mlp/kc1/mlrmbo30_vs_randomLHS.rds")

data1 = data[1, ]$result[[1]]$opt.path[, 1:7]
data2 = data[2, ]$result[[1]]$opt.path[, 1:7]

data1 = data[1, ]$result[[1]]$models[[1]]$learner.model
data2 = data[2, ]$result[[1]]$models[[1]]$learner.model

merge(data1, data2)
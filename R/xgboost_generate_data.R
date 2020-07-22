# Setup script for initially setting up the benchmarks 

library(batchtools)

source("R/xgboost_config.R")

lapply(packages, library, character.only = TRUE)


# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "R/xgboost_config.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_len(length(tasks))) {
  addProblem(
    name = tasks[i], 
    data = paste(TASK_LOCATION, tasks[i], sep = "/"), 
    fun = readProblem,
    reg = reg
  )
}

for (i in 1:length(ALGORITHMS)) {
  addAlgorithm(name = names(ALGORITHMS)[i], reg = reg, fun = ALGORITHMS[[i]]$fun)  
}

addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = ades, 
  repls = 30L)


# --- 3. SUBMIT ON LRZ ---

resources.serial = list(
	walltime = 3600L * 96L, memory = 1024L * 4L,
	clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

reg = loadRegistry(registry_name, writeable = TRUE)
tab = summarizeExperiments(
	by = c("job.id", "algorithm", "lrn", "problem", "lambda"))


# filter for problems we are interested in NOW 
probs = c("blood-transfusion-service-center", "kc1")
tosubmit = tab[algorithm == "mlrmbo", ]
tosubmit = tosubmit[problem %in% probs, ]
tosubmit = ijoin(tosubmit, findNotDone())

tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 30L)

submitJobs(tosubmit, resources = resources.serial)

tosubmit = tab[algorithm == "randomsearch", ]
tosubmit = tosubmit[problem %in% probs, ]
tosubmit = tosubmit[ ,.SD[which.min(job.id)], by = problem]

submitJobs(tosubmit, resources = resources.serial)

# --- 4. REDUCE RESULTS ---

# scp -r ru59sol2@lxlogin1.lrz.de:/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru59sol2/repos/xautoml/xgboost_registry/ .

reg = loadRegistry(registry_name, writeable = FALSE)
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "lrn", "problem"))

res = reduceResultsDataTable()
res = ijoin(tab, res, by = "job.id")

# --- 5. Analysis ---

# - Q1: How predictive are our models? 

prob = "blood-transfusion-service-center"
perf = getPerformance(res, prob)

library(ggplot2)
library(iml)
# newdata = getGroundTruth(res, prob)
# preds = getPredictions(res, prob, newdata)
models = getModels(res, prob)
train_data = getTrainData(res, prob)

effectlist = lapply(seq_len(length(models)), function(i) {
  pred = Predictor$new(model = models[[i]], data = train_data[[i]])
  effects = FeatureEffect$new(predictor = pred, feature = "nrounds", method = "pdp") 
  effects$plot() 
})

pg <- ggplot_build(effectlist[[1]])

library(gridExtra)
do.call("grid.arrange", effectlist)

p1 = effectlist[[1]]
p1

p = ggplot() 
xlabel = effectlist[[1]]$labels$x
ylabel = effectlist[[1]]$labels$y

for (i in 1:length(effectlist)) {
  pg = ggplot_build(effectlist[[i]])
  p = p + geom_line(data = pg$data[[1]], aes(x = x, y = y), alpha = 0.5) 
  p = p + ylim(pg$layout$panel_scales_y[[1]]$limits)
  p = p + xlab(xlabel) + ylab(ylabel)    
}


# Create a gif for the course of the surrogate 
subres = res[1, ]$result[[1]]$res
init.size = sum(subres$opt.path$env$dob == 0)
n = 200
models = subres$models
train_data = subres$opt.path$env$path
td = lapply(seq(init.size, n), function(i) {
  train_data[1:i, ]
})

effectlist = lapply(seq_len(length(models)), function(i) {
  pred = Predictor$new(model = models[[i]], data = td[[i]])
  effects = FeatureEffect$new(predictor = pred, feature = "nrounds", method = "pdp") 
  p = effects$plot() 
  pg = ggplot_build(effectlist[[i]])$data[[1]]
  pg
})





saveGIF({
  for (i in 1:10) {
    print(effectlist[[i]] + ggtitle(paste("Iteration, ", i)))
  }}, movie.name = "test.gif", interval = 0.1)










p 

grid.arrange(p1, p2)


p = effects$plot()
p = p + geom_smooth(data = newdata, aes(x = nrounds, y = y))
p

prob = "blood-transfusion-service-center"
perf = getPerformance(res, prob)

library(ggplot2)
newdata = getGroundTruth(res, prob)
preds = getPredictions(res, prob, newdata)
models = getModels(res, prob)
train_data = getTrainData(res, prob)

i = 2
pred = Predictor$new(model = models[[i]], data = train_data[[i]])

effects = FeatureEffect$new(predictor = pred, feature = "nrounds", method = "pdp")
p = effects$plot()
p = p + geom_smooth(data = newdata, aes(x = nrounds, y = y))
p


p = ggplot() + geom_smooth(data = newdata, aes(x = nrounds, y = y))
p
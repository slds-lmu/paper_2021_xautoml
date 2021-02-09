source("R/mlp_helper.r")
library(iml)
library(data.table)
library(ranger)
library(mlr)

library(mlrMBO)
library(BBmisc)
library(customtrees)
library(Rfast)
library(ggplot2)
library(gridExtra)

source("R/synthetic_helper.r")
source("R/pdp_helpers2.R")
source("R/tree_splitting.R")
source("R/helper_evaulation.R")
source("R/benchmarks/synthetic/mbo_helpers.R")
source("R/mbo_helpers.R")

# get all data and models and objective
path = "data/StyblinskiTang/"
folder = list.files(path)
set.seed(123)
data = get_all(path, folder, 500)


id = "StyblinskiTang"

dimension = 2

obj = makeSingleObjectiveFunction(name = paste0(id, dimension, "D"), fn = function(x) {
        1 / 2 * sum(x^4 - 16 * x^2 + 5 * x)
    }, 
    par.set = makeParamSet(makeNumericVectorParam(id = "x", len = dimension, lower = - 5, upper = 5)), 
    global.opt.params = rep(-2.9035, dimension)
)
ps = getParamSet(obj)

lambdas = c(0.1, 1, 2, 5, 100)
runs = lapply(lambdas, function(lambda) {
    path = file.path("data/runs/synthetic", paste0(id, ""), paste0(dimension, "D"))
    readRDS(file.path(path, paste0("mlrmbo_run_lambda_", lambda, "matern32.rds")))
})

types = get_types_of_runs(runs)
names(runs) = types
models = extract_models(runs)
names(models) = types


model_for_interpretation = "MBO_2"
model = models[[model_for_interpretation]]
feat = "x1"

mymodel = makeS3Obj("mymodel", fun = function() return(model))
predict.mymodel = function(object, newdata) {
  pred = predict(object$fun(), newdata = newdata)
  pp = getPredictionSE(pred)
  return(pp)
}

df = generateRandomDesign(n = 1000, par.set = getParamSet(obj))

ps_ids = getParamIds(ps, with.nr = TRUE, repeated = TRUE)

predictor = Predictor$new(model = mymodel, data = df[, ps_ids], predict.function = predict.mymodel)
effect = FeatureEffect$new(predictor = predictor, feature = feat, method = "ice")


# Compute tree
tree = compute_tree(effect = effect, testdata = df, objective = "SS_L1", n.split = 1)

plot_tree_pdps(tree, df = testdata, model = model, pdp.feature = feat, depth = 2, grid.size = 20)



plot_pdp_for_node(node = tree[[1]][[1]], model = model, grid.size = 20, method = "pdp_var_gp", 
  testdata = testdata, pdp.feature = feat, objective.gt = obj, alpha = 0.05)






test = node$subset.idx

optimum = rep(-2.9035, 10)
names(optimum) = paste0("x",1:10)

node = find_optimal_node(tree, optimum)
node = tree[[1]][[1]]

eval = get_eval_measures(effect,node,model, "x1", objective)

eff = effect$results[effect$results$.id %in% test,]
ggplot(data = eff, aes(x = x1, y = .value, group = .id)) + geom_line()
plot(effect)


# Compute tree
tree = compute_tree(effect, SS_area, n.split = 3)
test = tree[[4]]$right.child$subset.idx

#feat = "x2"
lapply(seq_len(length(tree) ), function(depth) {
  browser()
  nodes = tree[[depth]]
  plist = lapply(nodes, function(node) {
    plot_pdp_for_node(node, data[["10D"]]$testset, model, feat, objective.gt = objective)
  })
  do.call(grid.arrange, c(plist, nrow = 1))
})








test = function(n.split, data){
  df = data.frame("id" = NA, "data" = NA, "model" = NA, "objective" = NA, "feature" = NA, "par.conf" = NA, "par.gt.abs" = NA, "par.gt.sd" = NA,
                  "opt.conf" = NA, "opt.gt.abs" = NA, "opt.gt.sd" = NA,
                  "par.conf.opt" = NA, "par.gt.abs.opt" = NA, "par.gt.sd.opt" = NA,
                  "opt.conf.opt" = NA, "opt.gt.abs.opt" = NA, "opt.gt.sd.opt" = NA)
  
  id = 1
  grid.size = 20
  objectives = c("SS_L1", "SS_area", "SS_sd", "SS_area_med", "SS_area_quant")
  
 #browser() 
for(i in 1:length(data)){
  testdata = data[[i]]$testset
  features = names(testdata)
  objective = data[[i]]$objective
  optimum = rep(-2.9035, length(features))# anpassen für mlp
  names(optimum) = features
  
  for(j in 1:length(data[[i]]$model)){
    model = data[[i]]$model[[j]]
    #browser()
    for(k in objectives){
      
      for(f in features){
        effect = get_ice_curves(model = model, data = testdata, feature = f, grid.size = grid.size, mean = FALSE) # variante mit mean = TRUE testen
        tree = compute_tree(model, testdata, f, objective = k, n.split = n.split, grid.size = grid.size, addMean = FALSE) # n.splits anpassen
        
        # parent node
        node = tree[[1]][[1]]
        eval.par = get_eval_measures(effect, node, model, f, optimum[f], grid.size, objective)
        
        # node with optimum
        node = find_optimal_node(tree, optimum)
        eval.opt = get_eval_measures(effect, node, model, f, optimum[f], grid.size, objective)
        
        df = rbind(df, data.frame("id" = id, "data" = names(data)[i], "model" = names(data[[i]]$model)[j], "objective" = k, "feature" = f, 
                                  "par.conf" = eval.par$conf.diff, "par.gt.abs" = eval.par$gt.diff.abs, "par.gt.sd" = eval.par$gt.diff.sd,
                                  "opt.conf" = eval.opt$conf.diff, "opt.gt.abs" = eval.opt$gt.diff.abs, "opt.gt.sd" = eval.opt$gt.diff.sd,
                                  "par.conf.opt" = eval.par$conf.diff.opt, "par.gt.abs.opt" = eval.par$gt.diff.abs.opt, "par.gt.sd.opt" = eval.par$gt.diff.sd.opt,
                                  "opt.conf.opt" = eval.opt$conf.diff.opt, "opt.gt.abs.opt" = eval.opt$gt.diff.abs.opt, "opt.gt.sd.opt" = eval.opt$gt.diff.sd.opt))
        
        id = id + 1
      }
      
      
      
    }
  }
}
  return(df)
}


res = test()




set.seed(123)
for(i in 1:10){
  data = get_all(path, folder, 500)
  for(n.split in c(1,2,3)){
    result = test(n.split, data)
    result$n.split = n.split
    result$run = i
    if(i == 1 & n.split == 1){
      result.all = result
    }
    else result.all = rbind(result.all, result)
  }
  
}

saveRDS(result.all, "resultEvals.10rep.rds")





############################
#improvements from parent node to final node containing optimum

# for entire x_s
res$conf.diff = res$par.conf-res$opt.conf
res$gt.abs.diff = res$par.gt.abs-res$opt.gt.abs
res$gt.sd.diff = res$par.gt.sd-res$opt.gt.sd

# for area around optimum in x_s
res$conf.diff.opt = res$par.conf.opt-res$opt.conf.opt
res$gt.abs.diff.opt = res$par.gt.abs.opt-res$opt.gt.abs.opt
res$gt.sd.diff.opt = res$par.gt.sd.opt-res$opt.gt.sd.opt

# precentage improvement
res$conf.rel = res$conf.diff/res$par.conf
res$gt.rel = res$gt.abs.diff/res$par.gt.abs
res$conf.rel.opt = res$conf.diff.opt/res$par.conf.opt
res$gt.rel.opt = res$gt.abs.diff.opt/res$par.gt.abs.opt


resultsEval = resultsEval[-1,]

library(plyr)
improv.conf = count(resultsEval[which(resultsEval$conf.diff > 0),], vars = c("data","model", "objective"))
improv.conf$rel.freq = round(improv.conf$freq/as.numeric(substr(improv.conf$data, 1, nchar(improv.conf$data)-1)),2)

improv.gt = count(resultsEval[which(resultsEval$gt.abs.diff > 0),], vars = c("data","model", "objective"))
improv.gt$rel.freq = round(improv.gt$freq/as.numeric(substr(improv.gt$data, 1, nchar(improv.gt$data)-1)),2)


improv.conf.opt = count(resultsEval[which(resultsEval$conf.diff.opt > 0),], vars = c("data","model", "objective"))
improv.conf.opt$rel.freq = round(improv.conf.opt$freq/as.numeric(substr(improv.conf.opt$data, 1, nchar(improv.conf.opt$data)-1)),2)

improv.gt.opt = count(resultsEval[which(resultsEval$gt.abs.diff.opt > 0),], vars = c("data","model", "objective"))
improv.gt.opt$rel.freq = round(improv.gt.opt$freq/as.numeric(substr(improv.gt.opt$data, 1, nchar(improv.gt.opt$data)-1)),2)


sum(improv.gt$freq[which(improv.gt$objective=="SS_area")])#38 57 #41 42
sum(improv.gt$freq[which(improv.gt$objective=="SS_L1")])  #34 57 #43 37
sum(improv.gt$freq[which(improv.gt$objective=="SS_L2")])  #33 54 #42 37
# 10D
# 0.1





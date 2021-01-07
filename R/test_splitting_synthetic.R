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
source("R/pdp_helpers.R")
source("R/tree_splitting.R")
source("R/helper_evaulation.R")
source("R/benchmarks/synthetic/mbo_helpers.R")


# get all data and models and objective
path = "data/StyblinskiTang/"
folder = list.files(path)
data = get_all(path, folder, 500)




# effects - example
model = data[["2D"]]$model[[0.10]]
effect = get_ice_curves(model = model, data = data[["2D"]]$testset, feature = names(data[["2D"]]$testset)[1], mean = FALSE)
objective = data[["2D"]]$objective

# Compute tree
tree = compute_tree(effect, SS_area, n.split = 3)
test = node$subset.idx

optimum = c(-2.9035, -2.9035)
names(optimum) = c("x1","x2")

node = find_optimal_node(tree, optimum)
node = tree[[1]][[1]]

eval = get_eval_measures(effect,node,model, "x1", objective)

eff = effect$results[effect$results$.id %in% test,]
ggplot(data = eff, aes(x = x1, y = .value, group = .id)) + geom_line()
plot(effect)


test = function(){
  df = data.frame("id" = NA, "data" = NA, "model" = NA, "objective" = NA, "feature" = NA, "par.conf" = NA, "par.gt.abs" = NA, "par.gt.sd" = NA,
                  "opt.conf" = NA, "opt.gt.abs" = NA, "opt.gt.sd" = NA)
  
  id = 1
  objectives = c("SS_L2", "SS_area")
 #browser() 
for(i in 1:length(data)){
  testdata = data[[i]]$testset
  features = names(testdata)
  objective = data[[i]]$objective
  optimum = rep(-2.9035, length(features))# anpassen für mlp
  names(optimum) = features
  
  for(j in 1:length(data[[i]]$model)){
    model = data[[i]]$model[[j]]
    
    for(k in objectives){
      
      for(f in features){
        effect = get_ice_curves(model = model, data = testdata, feature = f, mean = FALSE) # variante mit mean = TRUE testen
        tree = compute_tree(effect, get(k), n.split = 3) # n.splits anpassen
        
        # parent node
        node = tree[[1]][[1]]
        eval.par = get_eval_measures(effect, node, model, f, objective)
        
        # node with optimum
        node = find_optimal_node(tree, optimum)
        eval.opt = get_eval_measures(effect, node, model, f, objective)
        
        df = rbind(df, data.frame("id" = id, "data" = names(data)[i], "model" = names(data[[i]]$model)[j], "objective" = k, "feature" = f, 
                                  "par.conf" = eval.par$conf.diff, "par.gt.abs" = eval.par$gt.diff.abs, "par.gt.sd" = eval.par$gt.diff.sd,
                                  "opt.conf" = eval.opt$conf.diff, "opt.gt.abs" = eval.opt$gt.diff.abs, "opt.gt.sd" = eval.opt$gt.diff.sd))
        
        id = id + 1
      }
      
      
      
    }
  }
}
  return(df)
}


res = test()

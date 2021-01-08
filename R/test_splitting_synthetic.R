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
set.seed(123)
data = get_all(path, folder, 500)




# effects - example
model = data[["10D"]]$model[[0.10]]
effect = get_ice_curves(model = model, data = data[["10D"]]$testset, feature = names(data[["10D"]]$testset)[2], mean = FALSE)
objective = data[["10D"]]$objective

# Compute tree
tree = compute_tree(effect, SS_L2, n.split = 3)
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

feat = "x2"
lapply(seq_len(length(tree) ), function(depth) {
  nodes = tree[[depth]]
  plist = lapply(nodes, function(node) {
    plot_pdp_for_node(effect, node, model, feat, objective.groundtruth = objective)
  })
  do.call(grid.arrange, c(plist, nrow = 1))
})








test = function(){
  df = data.frame("id" = NA, "data" = NA, "model" = NA, "objective" = NA, "feature" = NA, "par.conf" = NA, "par.gt.abs" = NA, "par.gt.sd" = NA,
                  "opt.conf" = NA, "opt.gt.abs" = NA, "opt.gt.sd" = NA,
                  "par.conf.opt" = NA, "par.gt.abs.opt" = NA, "par.gt.sd.opt" = NA,
                  "opt.conf.opt" = NA, "opt.gt.abs.opt" = NA, "opt.gt.sd.opt" = NA)
  
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
        eval.par = get_eval_measures(effect, node, model, f, optimum[f], objective)
        
        # node with optimum
        node = find_optimal_node(tree, optimum)
        eval.opt = get_eval_measures(effect, node, model, f, optimum[f], objective)
        
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



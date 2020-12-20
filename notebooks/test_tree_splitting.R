library(iml)
library(mlr)
library(data.table)
library(mlrMBO)
library(BBmisc)
library(customtrees)
library(Rfast)
library(ggplot2)
library(gridExtra)

source("notebooks/mbo_helpers.R")
source("notebooks/pdp_helpers.R")
source("notebooks/tree_splitting.R")

# Objective function which was initially analyzed
obj = makeSingleObjectiveFunction(name = "StyblinkskiTang3D", fn = function(x) {
    1 / 2 * sum(x^4 - 16 * x^2 + 5 * x)
  }, 
  par.set = makeParamSet(makeNumericVectorParam(id = "x", len = 2, lower = - 5, upper = 5))
)
ps = getParamSet(obj)

# Read in models for interpretation
runs = list(MBO_0.1 = readRDS("synthetic/mlrmbo_run_StyblinkskiTang3D_01.rds"), 
            MBO_1 = readRDS("synthetic/mlrmbo_run_StyblinkskiTang3D_1.rds"),
            MBO_2 = readRDS("synthetic/mlrmbo_run_StyblinkskiTang3D_2.rds")
        )

# Choose one model to interpret
model_for_interpretation = "MBO_1"
model = models[[model_for_interpretation]]

# Testdata 
df = generateDesign(par.set = ps, n = 1000, fun = lhs::randomLHS)

# Compute ICE curves as 
mymodel = makeS3Obj("mymodel", fun = function() return(model))

predict.mymodel = function(object, newdata) {
  pred = predict(object$fun(), newdata = newdata)
  getPredictionSE(pred) 
}

predictor = Predictor$new(mymodel, data = df, predict.function = predict.mymodel)
effect = FeatureEffect$new(predictor, method = "ice", grid.size = 20, feature = "x1")


# define objective
SS_L1 = function(y, x, requires.x = FALSE, ...) {
  require(Rfast)
  ypred = Rfast::colMedians(as.matrix(y))
  sum(t(abs(t(y) - ypred)))
}


# Compute tree
tree = compute_tree(effect, SS_L1, n.split = 3)

# see how many points go into each child 
get_size_of_tree(tree)
get_objective_values(tree)

# Plot node 2 in depth 3
node = tree[[depth]][[2]]
plot_pdp_for_node(node, model, "x1", objective.groundtruth = obj1)

lapply(seq_len(length(tree) - 1), function(depth) {
  nodes = tree[[depth]]
  plist = lapply(nodes, function(node) {
    plot_pdp_for_node(node, model, "x1", objective.groundtruth = obj1)
  })
  do.call(grid.arrange, c(plist, nrow = 1))
})


# TODO: Some pruning

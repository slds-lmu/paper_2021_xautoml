library(iml)
library(mlr)
library(data.table)
library(mlrMBO)
library(BBmisc)
library(customtrees)
library(Rfast)
library(ggplot2)
library(gridExtra)

source("R/pdp_helpers.R")
source("R/tree_splitting.R")
source("R/benchmarks/synthetic/mbo_helpers.R")

# Objective function which was initially analyzed
obj = makeSingleObjectiveFunction(name = "StyblinkskiTang3D", fn = function(x) {
    1 / 2 * sum(x^4 - 16 * x^2 + 5 * x)
  }, 
  par.set = makeParamSet(makeNumericVectorParam(id = "x", len = 2, lower = - 5, upper = 5))
)
ps = getParamSet(obj)

# Read in models for interpretation
runs = list(MBO_0.1 = readRDS("data/runs/synthetic/mlrmbo_run_StyblinkskiTang3D_01.rds"), 
            MBO_1 = readRDS("data/runs/synthetic/mlrmbo_run_StyblinkskiTang3D_1.rds"),
            MBO_2 = readRDS("data/runs/synthetic/mlrmbo_run_StyblinkskiTang3D_2.rds")
        )

models = extract_models(runs)
names(models) = get_types_of_runs(runs)

# Choose one model to interpret
model_for_interpretation = "MBO_1"
model = models[[model_for_interpretation]]
feat = "x1"

# Testdata 
df = generateDesign(par.set = ps, n = 1000, fun = lhs::randomLHS)

mymodel = makeS3Obj("mymodel", fun = function() return(model))
predict.mymodel = function(object, newdata) {
  pred = predict(object$fun(), newdata = newdata)
  pp = getPredictionSE(pred)
  return(pp)
}
predictor = Predictor$new(model = mymodel, data = df[c("x1", "x2")], predict.function = predict.mymodel)
effect = FeatureEffect$new(predictor = predictor, feature = feat, method = "ice")


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

depth = 4
node = tree[[depth]][[3]]
plot_pdp_for_node(node, model, feat, objective.groundtruth = NULL)

lapply(seq_len(length(tree) - 1), function(depth) {
  nodes = tree[[depth]]
  plist = lapply(nodes, function(node) {
    plot_pdp_for_node(node, model, feat, objective.groundtruth = NULL)
  })
  do.call(grid.arrange, c(plist, nrow = 1))
})


# TODO: Some pruning

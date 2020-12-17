library(iml)
library(mlr)
library(data.table)
library("mlrMBO")
library("BBmisc")
source("notebooks/mbo_helpers.R")
library(customtrees)
library(Rfast)

# define objective
SS_L1 = function(y, x, requires.x = FALSE, ...) {
  require(Rfast)
  ypred = Rfast::colMedians(as.matrix(y))
  sum(t(abs(t(y) - ypred)))
}



# read in models and prepare 
runs = list(MBO_0.1 = readRDS("synthetic/mlrmbo_run_StyblinkskiTang3D_01.rds"), 
            MBO_1 = readRDS("synthetic/mlrmbo_run_StyblinkskiTang3D_1.rds"),
            # MBO_2 = readRDS("../synthetic/mlrmbo_run_StyblinkskiTang3D_2.rds"),
            MBO_10 = readRDS("synthetic/mlrmbo_run_StyblinkskiTang3D_10.rds")# , 
            # LHS = readRDS("../synthetic/mlrmbo_run_StyblinkskiTang3D_lhs.rds")
)


types = get_types_of_runs(runs)
models = extract_models(runs)
names(models) = types
opdf = concatenate_runs(runs)

opdf$type = factor(opdf$type, levels = c("LHS","MBO_10","MBO_2", "MBO_1", "MBO_0.1"), labels = c("LHS","MBO_10","MBO_2", "MBO_1", "MBO_0.1")) 
model_for_interpretation = "MBO_0.1"
model = models[[model_for_interpretation]]



# create test data frame to calculate effects on LHS
obj1 = makeSingleObjectiveFunction(name = "StyblinkskiTang3D", fn = function(x) {
  1 / 2 * sum(x^4 - 16 * x^2 + 5 * x)
}, 
par.set = makeParamSet(makeNumericVectorParam(id = "x", len = 2, lower = - 5, upper = 5))
)
ps = getParamSet(obj1)
df = generateDesign(par.set = ps, n = 100, fun = lhs::randomLHS)



# generate SE ice curves for x1
mymodel = makeS3Obj("mymodel", fun = function() return(model))

predict.mymodel = function(object, newdata) {
  pred = predict(object$fun(), newdata = newdata)
  getPredictionSE(pred) 
}

predictor = Predictor$new(mymodel, data = df, predict.function = predict.mymodel)
effect = FeatureEffect$new(predictor, method = "ice", grid.size = 20, feature = "x1")

# add id to df (to match later on x2)
df$.id = seq_len(nrow(df))


# compute tree
compute_tree(effect = effect, df = df, objective = SS_L1, n.splits = 2)

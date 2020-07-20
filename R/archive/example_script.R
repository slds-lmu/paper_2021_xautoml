# Example Script for Having a Look at the MBO results


mbo_res = readRDS("xgboost/blood-transfusion-service-center/mbo_lambda1.rds")

# The opt.path stores all the data that has been evaluated during the optimization
# it can be transformed into a dataframe

# nrounds, eta, gamma, max_depth, colsample_bytree, colsample_bylevel, lambda, alpha, and subsample are the hyperparameters that have been tuned over
# y is the 3-fold cross-validated performance of the model given the respective configuration, i.e. the value of the objective function
# dob is the iteration where the configuration entered (0 stands for initial design to fit the very first model)
# all ather columns are not that important for now, I think :-) 

opdf = data.frame(mbo_res$opt.path)
head(opdf)


# $models stores the final model that has been fitted on the design data 
# it can contain a list of multiple models, if the surrogate model is stored at each step
# in this case, only the last model is stored (sorry - I thought I stored all of them; but we decided to start with the last one anyways :-) ...) 
models = mbo_res$models



# the iMBO package takes objects of class MBOSingleObjResult
# the iMBO isn't a package yet... TO BE DONE BY FEDERICO

dir = "/home/julia/Documents/repos/iMBO/R"

source(file.path(dir, "FeatureEffectMBO.R"))
source(file.path(dir, "utils_fembo_inflinst.R"))

res = mbo_res$opt.result$mbo.result

# I am not sure how to use the below stuff... :-) 
feateffect = FeatureEffectMBO(res, feature = "nrounds")
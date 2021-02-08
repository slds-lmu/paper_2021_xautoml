library(iml)
library(mlrMBO)

# get data and model
source("R/data_prep.r")

# choose data set and lambda
data = data_btss
lambda = 1

object = get_objects(data_btss, 0.5)
model = object$model[[1]]
data_train = object$data_train
data_test = object$data_test

# correlation between features for training vs test set
cor(data_train)
cor(data_test)
# some features like eta and alpha have a recognizable correlation (around 0.3) in the training set
# in the test set all features are uncorrelated. 

# effect plots
pred = Predictor$new(model = model, data = data_train)

# ice + pdp
effects = FeatureEffect$new(predictor = pred, feature = "eta", method = "pdp+ice")
effects$plot()

# centered ice + pdp - shows interactions
effects.center = FeatureEffect$new(predictor = pred, feature = "eta", method = "pdp+ice", center.at = 0)
effects.center$plot()

# ale
ale = FeatureEffect$new(predictor = pred, feature = "eta", method = "ale")
ale$plot()

# 2 dim pdp
pdp2dim = FeatureEffect$new(predictor = pred, feature = c("nrounds", "eta"), method = "ale")
pdp2dim$plot()



# Permutation Feature importance
pred = Predictor$new(model = model, data = data_test, y = "y")
pfi = FeatureImp$new(predictor = pred, loss = "rmse")

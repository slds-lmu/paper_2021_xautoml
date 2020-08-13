# xAutoML - Explaining AutoML Systems 

## Use-Cases 

### `xgboost`-Tuning with `mlrMBO`

Tuning `xgboost` on the [AutoML Benchmark Data](https://openml.github.io/automlbenchmark/automl_overview.html) with `mlrMBO`.

#### Description of the "AutoML" System

We are tuning over the following search space of an `xgboost` classifier: 

```
ps = makeParamSet(
# do early stopping instead for the bigger datasets
    makeNumericParam("nrounds", lower = 0, upper = 12, trafo = function(x) round(2^x)), # 2^13 = 8192	
    makeNumericParam("eta", lower = -7, upper = 0, trafo = function(x) 2^x), # 2^(-7) = 0.007 < 0.01
    makeNumericParam("gamma", lower = -7, upper = 6, trafo = function(x) 2^x), 
    makeIntegerParam("max_depth", lower = 3, upper = 20),
    makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
    makeNumericParam("colsample_bylevel", lower = 0.5, upper = 1),
    makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("subsample", lower = 0.5, upper = 1)
)
```

#### Description of the datasets

#### Status 


### Surrogate Benchmark: Pseudo-tuning MLP's with the help of a surrogate model created  a by on the [AutoML Benchmark Data](https://openml.github.io/automlbenchmark/automl_overview.html) with `mlrMBO`

#### Description of the "AutoML" System

#### Description of the datasets

#### Status 

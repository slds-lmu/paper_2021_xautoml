# Explaining Hyperparameter Optimization via PDPs



This repository gives access to an implementation of the methods presented in the paper submission "Explaining Hyperparameter Optimization via PDPs", as well as all code that was used for the experimental analysis. 

This repository is structured as follows: 

```
    ├── renv/                   # renv configuration files to enable a reproducible setup 
    ├── R/                      # Implementation of methods 
    ├── experiments/            # Code for experimental analysis (section 6)
    │   ├── synthetic           # Synthetic benchmark (section 6.1)
    │   ├── mlp                 # DNN surrogate benchmark (section 6.2)
    ├── analysis/               # Scripts used to create figures and tables in the paper
    ├── LICENSE
    └── README.md               
```    

## Reproducible Setup 

To allow for a proper, reproducible setup of the environment we use the package `renv`. 

After installing the package `renv` via

```
install.packages("renv")
```

the project dependencies can be installed via 

```
renv::restore()
```

## Quick Start  

First, assume we have a surrogate model that we want to analyze. 

Here, for example, we tuned a support vector machine on the `iris` task, and extracted the surrogate model after the last iteration. 

```
library(mlr)
library(mlrMBO)
library(e1071)

par.set = makeParamSet(
  makeNumericParam("cost", -15, 15, trafo = function(x) 2^x),
  makeNumericParam("gamma", -15, 15, trafo = function(x) 2^x)
)

ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 5)
tune.ctrl = makeTuneControlMBO(mbo.control = ctrl)
res = tuneParams(makeLearner("classif.svm"), iris.task, cv3, par.set = par.set, control = tune.ctrl,
  show.info = FALSE)
  
surrogate =  res$mbo.result$models[[1]]
```

We are computing the PDP estimate with confidence for hyperparameter `cost`. We use the `marginal_effect_sd_over_mean` function, which is build on the basis of the `iml` package. 

```
library(iml)

me = marginal_effect_sd_over_mean(model = surrogate, feature = "cost", method = "pdp_var")
```

Visualization functionality is provided as well: 

```
library(ggplot2)

plot_pdp_with_se(me)
```

... perform tree splitting




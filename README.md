
# Explaining Hyperparameter Optimization via PDPs

![](docs/images/tree_example.png)<!-- -->

This repository gives access to an implementation of the methods
presented in the paper submission “Explaining Hyperparameter
Optimization via PDPs”, as well as all code that was used for the
experimental analysis.

This repository is structured as follows:

``` 
    ├── analysis/               # Scripts used to create figures and tables in the paper
    ├── data/                   # Location where all experimental data is stored
    │   ├── raw/                # Raw datasets for the DNN surrogate benchmark
    │   ├── runs/               # Individual runs 
    ├── experiments/            # Code for experimental analysis (section 6)
    │   ├── synthetic           # Synthetic benchmark (section 6.1)
    │   ├── mlp                 # DNN surrogate benchmark (section 6.2)
    ├── renv/                   # renv configuration files to enable a reproducible setup 
    ├── R/                      # Implementation of methods 
    ├── LICENSE
    └── README.md               
```

## Reproducible Setup

To allow for a proper, reproducible setup of the environment we use the
package `renv`.

The project dependencies can be installed via

``` r
library("renv")
```

    ## 
    ## Attaching package: 'renv'

    ## The following object is masked from 'package:stats':
    ## 
    ##     update

    ## The following objects are masked from 'package:utils':
    ## 
    ##     history, upgrade

    ## The following objects are masked from 'package:base':
    ## 
    ##     load, remove

``` r
renv::restore()
```

    ## * The library is already synchronized with the lockfile.

## Quick Start

``` r
# Loading all scripts we need
source("R/tree_splitting.R")
```

    ## 
    ## Attaching package: 'BBmisc'

    ## The following object is masked from 'package:base':
    ## 
    ##     isFALSE

``` r
source("R/helper.R")
source("R/marginal_effect.R")
source("R/plot_functions.R")
```

First, assume we have a surrogate model that we want to analyze.

Here, for example, we tuned a support vector machine on the `iris` task,
and extracted the surrogate model after the last iteration.

``` r
library(mlr)
```

    ## Loading required package: ParamHelpers

    ## 'mlr' is in maintenance mode since July 2019. Future development
    ## efforts will go into its successor 'mlr3' (<https://mlr3.mlr-org.com>).

``` r
library(mlrMBO)
```

    ## Loading required package: smoof

    ## Loading required package: checkmate

``` r
library(e1071)
```

    ## 
    ## Attaching package: 'e1071'

    ## The following object is masked from 'package:mlr':
    ## 
    ##     impute

``` r
par.set = makeParamSet(
  makeNumericParam("cost", -15, 15, trafo = function(x) 2^x),
  makeNumericParam("gamma", -15, 15, trafo = function(x) 2^x)
)

ctrl = makeMBOControl()
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritCB(cb.lambda = 1))
ctrl = setMBOControlTermination(ctrl, iters = 5)
tune.ctrl = makeTuneControlMBO(mbo.control = ctrl)
res = tuneParams(makeLearner("classif.svm"), iris.task, cv3, par.set = par.set, control = tune.ctrl,
  show.info = FALSE)
  
surrogate =  res$mbo.result$models[[1]]

print(surrogate)
```

    ## Model for learner.id=regr.km; learner.class=regr.km
    ## Trained on: task.id = data; obs = 13; features = 2
    ## Hyperparameters: jitter=TRUE,covtype=matern3_2,optim.method=gen,nugget.estim=TRUE

We are computing the PDP estimate with confidence for hyperparameter
`cost`. We use the `marginal_effect_sd_over_mean` function, which uses
the `iml` packages.

    ##         cost      mean         sd
    ## 1 -14.982900 0.4334600 0.10780414
    ## 2 -13.405465 0.4271929 0.10016707
    ## 3 -11.828030 0.4204815 0.09301408
    ## 4 -10.250595 0.4132970 0.08657514
    ## 5  -8.673160 0.4056233 0.08106157
    ## 6  -7.095725 0.3975385 0.07662969

We visualize the outcome:

``` r
library(ggplot2)

p = plot_pdp_with_uncertainty_1D(me)
print(p)
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

To improve the uncertainty estimates, we partition the input space. We
perform 3 splits and use the L2-objective.

``` r
predictor = Predictor$new(model = surrogate, data = data)
effects = FeatureEffect$new(predictor = predictor, feature = "cost", method = "pdp")

tree = compute_tree(effects, data, "SS_L2", 2)
```

    ## Loading required package: customtrees

    ## 
    ## Attaching package: 'customtrees'

    ## The following objects are masked _by_ '.GlobalEnv':
    ## 
    ##     adjust_nsplits, adjust_split_point, find_best_binary_split,
    ##     generate_node_index, generate_split_candidates, get_closest_point,
    ##     perform_split, split_parent_node

We now want to visualize the PDP in the node with the best objective
after 3 splits.

``` r
predictor = Predictor$new(model = surrogate, data = data)
effects = FeatureEffect$new(predictor = predictor, feature = "cost", method = "pdp")

tree = compute_tree(effects, data, "SS_L2", 2)
```

## Reproduce Experiments

The steps necessary to reproduce the experiments are described
[here](benchmarks/README.Rmd).

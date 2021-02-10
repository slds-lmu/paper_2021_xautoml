
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

    FALSE 
    FALSE Attaching package: 'renv'

    FALSE The following object is masked from 'package:stats':
    FALSE 
    FALSE     update

    FALSE The following objects are masked from 'package:utils':
    FALSE 
    FALSE     history, upgrade

    FALSE The following objects are masked from 'package:base':
    FALSE 
    FALSE     load, remove

``` r
renv::restore()
```

    FALSE * The library is already synchronized with the lockfile.

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
library(BBmisc)
library(data.table)

par.set = makeParamSet(
  makeNumericParam("cost", -10, 4, trafo = function(x) 2^x),
  makeNumericParam("gamma", -10, 4, trafo = function(x) 2^x)
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

    ##        cost      mean         sd
    ## 1 -9.998318 0.7561011 0.09025358
    ## 2 -9.262719 0.7588126 0.06684826
    ## 3 -8.527120 0.7601592 0.05138241
    ## 4 -7.791522 0.7588288 0.04146937
    ## 5 -7.055923 0.7522169 0.03390507
    ## 6 -6.320325 0.7356426 0.03462486

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
    ## Loading required package: customtrees
    ## Loading required package: customtrees

We now want to visualize the PDP in the node with the best objective
after 3 splits.

``` r
plot_pdp_for_node(node = tree[[2]][[2]], testdata = data, model = surrogate, pdp.feature = "cost", grid.size = 20)
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Reproduce Experiments

The steps necessary to reproduce the experiments are described
[here](benchmarks/README.md).

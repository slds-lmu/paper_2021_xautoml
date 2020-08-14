# xAutoML - Explaining AutoML Systems 

## Use-Cases 

### (1) Tuning `xgboost` with `mlrMBO`

We use [`mlrMBO`](https://github.com/mlr-org/mlrMBO) to tune an `xgboost` classifier. 

We want to **minimize** the function f: X --> Y with the following specifications: 

* Search space X: 

```
ps = makeParamSet(
    makeNumericParam("nrounds", lower = 0, upper = 12, trafo = function(x) round(2^x)), # 2^12 = 4096	
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
* Objective function f(x): 
    * A single function evaluation corresponds evaluating an `xgboost` model with a configuration `x` by a 3-fold cross-validation. 
    * Returned is the 3-fold cross-validated performance in terms of **mean misclassification error*. 


`mlrMBO` is configured as follows: 

|  |  |  |
| :---         |     :---:      |          ---: |
| Surrogate Model:         | Gaussian Process (default)        | Matérn3/2-kernel    |
| Infill Criterion         | Lower Confidence Bound (LCB) | lambda in {0.5, 1, 2}   |
| Infill Optimization      | Focus search (default)       |   /                     |
| Proposals per Iteration  |   1                          |   /                     |
| Termination Criterion    |   200 Evaluations of f(x)    |   /                     |

As "ground-truth", f was evaluated on a randomLHS with $n = 10000$ samples, to evaluate the bias that arises from analyzing "optimization data" as compared to randomly sampled data. 

Datasets are taken from [AutoML Benchmark Data](https://openml.github.io/automlbenchmark/automl_overview.html): 

| dataset | done | 
| :---         |     30 / 30 replication + 1 / 1 randomLHS   |     
| kc1         |     30 / 30 replication + 1 / 1 randomLHS   |     
| blood-transfusion-service center         |     30 / 30 replication + 1 / 1 randomLHS   |     
| numerai28.6      |     30 / 30 replication +  0 / 1 randomLHS   |     
| phoneme  |     30 / 30 replication + 1 / 1 randomLHS   |     
| sylvine    |     30 / 30 replication + 1 / 1 randomLHS   |     

All runs are stored here: https://drive.google.com/drive/folders/1EWzvn34tGG7PE8yR-Y_Qi2fzqLwn1bX7?usp=sharing under xgboost/<dataset>/mlrmbo_30_repls.rds.

---


### (2) Tuning a multi-layer perceptron with `mlrMBO` (surrogate meta-model optimization)

We use [`mlrMBO`](https://github.com/mlr-org/mlrMBO) to tune an `mlp` classifier. 

We want to **minimize** the function f: X --> Y with the following specifications: 

* Search space X: 

```
ps = makeParamSet(
        makeNumericParam("batch_size", lower = log(16, 2), upper = log(512, 2), trafo = function(x) round(2^x)), 
        makeNumericParam("max_dropout", lower = 0, upper = 1), 
        makeNumericParam("max_units", lower = log(64, 2), upper = log(1024, 2), trafo = function(x) round(2^x)), 
        makeIntegerParam("num_layers", lower = 1, upper = 5),
        makeNumericParam("learning_rate", lower = 0, upper = 0.01),
        makeNumericParam("momentum", lower = 0.1, upper = 1),
        makeNumericParam("weight_decay", lower = 0, upper = 0.1)
    )
```
* Objective function f(x) / surrogate model \hat f(x): 
    * To make model evaluations cheaper, we do not evaluate f(x) but an approximation \hat f(x) (pseudo-evaluations produced by a so-called "surrogate meta-model") that was trained on the [LCBench data](https://github.com/automl/LCBench)
    * A single function evaluation does **not** correspond to evaluation a multi-layer perceptron with a configuration `x` (expensive), but evaluating a surrogate meta-model \hat f(x) as approximiation for f(x) for a given configuration x. 
    * Returned is the balanced accuracy on the validation set (as estimated by the surrogate meta-model)
    * For details on the surrogata meta-model see below. 


`mlrMBO` is configured as follows: 

|  |  |  |
| :---         |     :---:      |          ---: |
| Surrogate Model:         | Gaussian Process (default)        | Matérn3/2-kernel    |
| Infill Criterion         | Lower Confidence Bound (LCB) | lambda in {0.5, 1, 2}   |
| Infill Optimization      | Focus search (default)       |   /                     |
| Proposals per Iteration  |   1                          |   /                     |
| Termination Criterion    |   200 Evaluations of f(x)    |   /                     |

Datasets are taken from [AutoML Benchmark Data](https://openml.github.io/automlbenchmark/automl_overview.html): 

| dataset                                    | p      | n      | done |
|--------------------------------------------|--------|------|---|
| christine (1)                              | 5418   | 1637 | 1 / 30  |
| dilbert (1)                                | 10000  | 2001 | 1 / 30   |
| guillermo (1)                              | 20000  | 4297 | 1 / 30   |
| riccardo (1)                               | 20000  | 4297 | 1 / 30   |
| dionis (1)                                 | 416188 | 61   | 1 / 30   |
| albert (1)                                 | 425240 | 79   | 1 / 30   |
| robert (1)                                 | 10000  | 7201 | 1 / 30   |
| MiniBooNE (1)                              | 130064 | 51   | 1 / 30   |
| fabert (1)                                 | 8237   | 801  | 1 / 30   |
| jasmine (1)                                | 2984   | 145  | 1 / 30   |
| sylvine (1)                                | 5124   | 21   | 1 / 30   |
| airlines (1)                               | 539383 | 8    | 1 / 30   |
| APSFailure (1)                             | 76000  | 171  | 1 / 30   |
| KDDCup09_appetency (1)                     | 50000  | 231  | 1 / 30   |
| cnae-9 (1)                                 | 1080   | 857  | 1 / 30   |
| covertype (4)                              | 581012 | 55   | 1 / 30   |
| nomao (1)                                  | 34465  | 119  | 1 / 30   |
| Amazon_employee_access (1)                 | 32769  | 10   | 1 / 30   |
| Fashion-MNIST (1)                          | 70000  | 785  | 1 / 30   |
| jungle_chess_2pcs_raw_endgame_complete (1) | 44819  | 7    | 1 / 30   |
| shuttle (1)                                | 58000  | 10   | 1 / 30   |
| car (3)                                    | 1728   | 7    | 1 / 30   |
| segment (3)                                | 2310   | 20   | 1 / 30   |
| higgs (2)                                  | 98050  | 29   | 1 / 30   |
| Australian (4)                             | 690    | 15   | 1 / 30   |
| volkert (1)                                | 58310  | 181  | 1 / 30   |
| helena (1)                                 | 65196  | 28   | 1 / 30   |
| jannis (1)                                 | 83733  | 55   | 1 / 30   |
| adult (2)                                  | 48842  | 15   | 1 / 30   |
| bank-marketing (1)                         | 45211  | 17   | 1 / 30   |
| phoneme (1)                                | 5404   | 6    | 1 / 30   |
| connect-4 (2)                              | 67557  | 43   | 1 / 30   |
| numerai28.6 (2)                            | 96320  | 22   | 1 / 30   |
| anneal (1)                                 | 898    | 39   | 1 / 30   |
| blood-transfusion-service-center (1)       | 748    | 5    | 1 / 30   |
| arrhythmia (1)                             | 452    | 280  | 1 / 30   |
| mfeat-factors (1)                          | 2000   | 217  | 1 / 30   |
| kr-vs-kp (1)                               | 3196   | 37   | 1 / 30   |
| credit-g (1)                               | 1000   | 21   | 1 / 30   |

All runs are stored here: https://drive.google.com/drive/folders/1EWzvn34tGG7PE8yR-Y_Qi2fzqLwn1bX7?usp=sharing under mlp/<dataset>/mlrmbo_30_repls.rds.
    
* Details on the surrogate meta-model:     
    * The surrogate model was itself optimized to the LCBench data: A simple random search with n = 500 and 3-fold cross-validation was performed over the following search space; the best model was chosen. 
 ```
ps = makeParamSet(
			makeNumericParam("num.trees", lower = log(10, 2), upper = log(500, 2), trafo = function(x) round(2^x)), # 2^13 = 8192	
			makeLogicalParam("do.mtry"), # doing mtry or using all features
			makeIntegerParam("min.node.size", lower = 1L, upper = 5L),
			makeIntegerParam("num.random.splits", lower = 1, upper = 100)
		)
```
This was performed both for the validation performance (what we focus on now), and for the test performance (maybe for future investigations).     

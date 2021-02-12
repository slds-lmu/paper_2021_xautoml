
``` r
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
```

# Experiments on a DNN Surrogate Benchmark (Section 6.2)

The setup of this experiment is slightly more complicated, since we
first have to train an empirical performance model, before we can
perform the MBO runs and perform the tree splitting.

### (1) Training the Empirical Performance Model

In order to train the empirical performance model, the LCBench data
needs first to be downloaded from
[here](https://ndownloader.figshare.com/files/21188598) and unzipped.

Then, the following `get_data_from_LCBench.py` allows to store the
respective datasets into a .csv files in a proper directory structure.
The python script was tested with Python 3.7.6.

As a result, you will find the LCBench data stored in
`data/runs/mlp/<dataset>/0_objective/lcbench2000.csv`.

Then, the following script allows will again setup the experimental
registry in which the surrogates are computed.

``` r
source("benchmarks/mlp/1_empirical_performance_model/generate_data.R")
```

    ## Reading registry in read-write mode

    ## No readable configuration file found

    ## Adding problem 'adult'

    ## Adding problem 'airlines'

    ## Adding problem 'albert'

    ## Adding problem 'Amazon_employee_access'

    ## Adding problem 'APSFailure'

    ## Adding problem 'Australian'

    ## Adding problem 'bank-marketing'

    ## Adding problem 'blood-transfusion-service-center'

    ## Adding problem 'car'

    ## Adding problem 'christine'

    ## Adding problem 'cnae-9'

    ## Adding problem 'connect-4'

    ## Adding problem 'covertype'

    ## Adding problem 'credit-g'

    ## Adding problem 'dionis'

    ## Adding problem 'fabert'

    ## Adding problem 'Fashion-MNIST'

    ## Adding problem 'helena'

    ## Adding problem 'higgs'

    ## Adding problem 'jannis'

    ## Adding problem 'jasmine'

    ## Adding problem 'jungle_chess_2pcs_raw_endgame_complete'

    ## Adding problem 'kc1'

    ## Adding problem 'KDDCup09_appetency'

    ## Adding problem 'kr-vs-kp'

    ## Adding problem 'mfeat-factors'

    ## Adding problem 'MiniBooNE'

    ## Adding problem 'nomao'

    ## Adding problem 'numerai28.6'

    ## Adding problem 'phoneme'

    ## Adding problem 'segment'

    ## Adding problem 'shuttle'

    ## Adding problem 'sylvine'

    ## Adding problem 'vehicle'

    ## Adding problem 'volkert'

    ## Adding algorithm 'randomsearch'

    ## Adding 1 experiments ('adult'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('airlines'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('albert'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('Amazon_employee_access'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('APSFailure'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('Australian'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('bank-marketing'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('blood-transfusion-service-center'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('car'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('christine'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('cnae-9'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('connect-4'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('covertype'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('credit-g'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('dionis'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('fabert'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('Fashion-MNIST'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('helena'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('higgs'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('jannis'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('jasmine'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('jungle_chess_2pcs_raw_endgame_complete'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('kc1'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('KDDCup09_appetency'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('kr-vs-kp'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('mfeat-factors'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('MiniBooNE'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('nomao'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('numerai28.6'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('phoneme'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('segment'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('shuttle'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('sylvine'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('vehicle'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

    ## Adding 1 experiments ('volkert'[1] x 'randomsearch'[1] x repls[1]) ...

    ## Skipping 1 duplicated experiments ...

The above script create an experimental registry in
`regs/LCBench_surogate_registry`.

The core part, which is the actual definition of the experiment is in
the function `randomsearch(...)` in the file
`benchmarks/synthetic/config.R`. In this function, a basic random search
is performed to find a proper configuration of a random forest, and fit
a random forest with this configuration.

Jobs are submitted as follows.

``` r
reg = loadRegistry("regs/LCBench_surogate_registry", writeable = TRUE)

# Overview table over experiments to be run
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem"))

# Submit all experiments to your local system  
submitJobs(tab)

# Alternatively, submit them to the linux cluster with respective resources
# source("benchmarks/resources.R")
# submitJobs(tab, resources = resources.serial.default)
```

The following commands will summarize the experimental results in a
compact way and store it in

``` r
reg = loadRegistry("regs/LCBench_surogate_registry")

# Check status of the experiments 
getStatus()

source("benchmarks/mlp/1_empirical_performance_model/reduce.R")
reduce_results_surrogate(reg, savedir = "data/runs/mlp_results")
```

### (2) Run MBO

For each problem, i.e. for each dataset, we run mlrMBO to perform
Bayesian optimization. We optimize the objective function as
approximated by the empirical performance model as computed before.

We perform 30 replications per experiment.

``` r
source("benchmarks/mlp/2_mbo_runs/generate_data.R")
```

    ## Reading registry in read-write mode

    ## No readable configuration file found

    ## Adding problem 'adult'

    ## Adding problem 'airlines'

    ## Adding problem 'albert'

    ## Adding problem 'Amazon_employee_access'

    ## Adding problem 'APSFailure'

    ## Adding problem 'Australian'

    ## Adding problem 'bank-marketing'

    ## Adding problem 'blood-transfusion-service-center'

    ## Adding problem 'car'

    ## Adding problem 'christine'

    ## Adding problem 'cnae-9'

    ## Adding problem 'connect-4'

    ## Adding problem 'covertype'

    ## Adding problem 'credit-g'

    ## Adding problem 'dionis'

    ## Adding problem 'fabert'

    ## Adding problem 'Fashion-MNIST'

    ## Adding problem 'helena'

    ## Adding problem 'higgs'

    ## Adding problem 'jannis'

    ## Adding problem 'jasmine'

    ## Adding problem 'jungle_chess_2pcs_raw_endgame_complete'

    ## Adding problem 'kc1'

    ## Adding problem 'KDDCup09_appetency'

    ## Adding problem 'kr-vs-kp'

    ## Adding problem 'mfeat-factors'

    ## Adding problem 'MiniBooNE'

    ## Adding problem 'nomao'

    ## Adding problem 'numerai28.6'

    ## Adding problem 'phoneme'

    ## Adding problem 'segment'

    ## Adding problem 'shuttle'

    ## Adding problem 'sylvine'

    ## Adding problem 'vehicle'

    ## Adding problem 'volkert'

    ## Adding algorithm 'mlrmbo'

    ## Adding 90 experiments ('adult'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('airlines'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('albert'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('Amazon_employee_access'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('APSFailure'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('Australian'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('bank-marketing'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('blood-transfusion-service-center'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('car'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('christine'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('cnae-9'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('connect-4'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('covertype'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('credit-g'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('dionis'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('fabert'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('Fashion-MNIST'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('helena'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('higgs'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('jannis'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('jasmine'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('jungle_chess_2pcs_raw_endgame_complete'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('kc1'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('KDDCup09_appetency'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('kr-vs-kp'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('mfeat-factors'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('MiniBooNE'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('nomao'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('numerai28.6'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('phoneme'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('segment'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('shuttle'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('sylvine'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('vehicle'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

    ## Adding 90 experiments ('volkert'[1] x 'mlrmbo'[3] x repls[30]) ...

    ## Skipping 90 duplicated experiments ...

The details of how the BO run is implemented can be seen in the function
`mlrmbo(...)`.

Again, jobs are submitted and reduced.

``` r
reg = loadRegistry("regs/mlp_bo_registry", writeable = TRUE)

# Overview table over experiments to be run
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem"))

# Submit all experiments to your local system  
submitJobs(tab)

# Alternatively, submit them to the linux cluster with respective resources
# source("benchmarks/resources.R")
# submitJobs(tab, resources = resources.serial.default)
```

We again store the compromised results.

``` r
reg = loadRegistry("regs/mlp_bo_registry")

# Check status of the experiments 
getStatus()

source("benchmarks/mlp/2_mbo_runs/reduce.R")
reduce_results_mlrmbo(reg, savedir = "data/runs/mlp_results")
```

### (3) Compute the Ground-Truth PDPs

We compute the ground-truth PDP on the data in order to be able to
compare our estimates against it.

We will use the same data for Monte Carlo Sampling as well as the same
set of grid points for both estimates.

``` r
source("benchmarks/mlp/3_gt_pdp/generate_data.R")
```

    ## No readable configuration file found

    ## Created registry in '/home/julia/Documents/repos/paper_2020_xautoml/regs/mlp_ground_truth_pdp' using cluster functions 'Interactive'

    ## Adding problem 'adult'

    ## Adding problem 'airlines'

    ## Adding problem 'albert'

    ## Adding problem 'Amazon_employee_access'

    ## Adding problem 'APSFailure'

    ## Adding problem 'Australian'

    ## Adding problem 'bank-marketing'

    ## Adding problem 'blood-transfusion-service-center'

    ## Adding problem 'car'

    ## Adding problem 'christine'

    ## Adding problem 'cnae-9'

    ## Adding problem 'connect-4'

    ## Adding problem 'covertype'

    ## Adding problem 'credit-g'

    ## Adding problem 'dionis'

    ## Adding problem 'fabert'

    ## Adding problem 'Fashion-MNIST'

    ## Adding problem 'helena'

    ## Adding problem 'higgs'

    ## Adding problem 'jannis'

    ## Adding problem 'jasmine'

    ## Adding problem 'jungle_chess_2pcs_raw_endgame_complete'

    ## Adding problem 'kc1'

    ## Adding problem 'KDDCup09_appetency'

    ## Adding problem 'kr-vs-kp'

    ## Adding problem 'mfeat-factors'

    ## Adding problem 'MiniBooNE'

    ## Adding problem 'nomao'

    ## Adding problem 'numerai28.6'

    ## Adding problem 'phoneme'

    ## Adding problem 'segment'

    ## Adding problem 'shuttle'

    ## Adding problem 'sylvine'

    ## Adding problem 'vehicle'

    ## Adding problem 'volkert'

    ## Adding algorithm 'compute_ground_truth_pdp'

    ## Adding 1 experiments ('adult'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('airlines'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('albert'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('Amazon_employee_access'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('APSFailure'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('Australian'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('bank-marketing'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('blood-transfusion-service-center'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('car'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('christine'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('cnae-9'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('connect-4'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('covertype'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('credit-g'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('dionis'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('fabert'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('Fashion-MNIST'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('helena'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('higgs'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('jannis'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('jasmine'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('jungle_chess_2pcs_raw_endgame_complete'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('kc1'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('KDDCup09_appetency'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('kr-vs-kp'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('mfeat-factors'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('MiniBooNE'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('nomao'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('numerai28.6'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('phoneme'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('segment'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('shuttle'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('sylvine'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('vehicle'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

    ## Adding 1 experiments ('volkert'[1] x 'compute_ground_truth_pdp'[1] x repls[1]) ...

``` r
reg = loadRegistry("regs/mlp_ground_truth_pdp", writeable = TRUE)

# Overview table over experiments to be run
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem"))

# Submit all experiments to your local system  
submitJobs(tab[1, ])

# Alternatively, submit them to the linux cluster with respective resources
# source("benchmarks/resources.R")
# submitJobs(tab, resources = resources.serial.default)
```

We again store the compromised results.

``` r
reg = loadRegistry("regs/mlp_ground_truth_pdp")

# Check status of the experiments 
getStatus()

source("benchmarks/mlp/3_gt_pdp/reduce.R")
reduce_results_gt_pdp(reg, savedir = "data/runs/mlp_results")
```

### (4) Compute Tree-Partitioning and perform Evaluation

Finally, we perform the tree-partitioning.

``` r
source("benchmarks/mlp/4_tree_splitting/generate_data.R")
```

    ## Reading registry in read-write mode

    ## No readable configuration file found

    ## Adding algorithm 'perform_tree_splitting'

    ## Adding 4 experiments ('adult'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('airlines'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('albert'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('Amazon_employee_access'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('APSFailure'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('Australian'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('bank-marketing'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('blood-transfusion-service-center'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('car'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('christine'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('cnae-9'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('connect-4'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('covertype'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('credit-g'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('dionis'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('fabert'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('Fashion-MNIST'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('helena'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('higgs'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('jannis'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('jasmine'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('jungle_chess_2pcs_raw_endgame_complete'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('kc1'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('KDDCup09_appetency'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('kr-vs-kp'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('mfeat-factors'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('MiniBooNE'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('nomao'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('numerai28.6'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('phoneme'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('segment'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('shuttle'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('sylvine'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('vehicle'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

    ## Adding 4 experiments ('volkert'[1] x 'perform_tree_splitting'[4] x repls[1]) ...

    ## Skipping 4 duplicated experiments ...

``` r
reg = loadRegistry("regs/tree_splitting", writeable = TRUE)

# Overview table over experiments to be run
tab = summarizeExperiments(
  by = c("job.id", "algorithm", "problem", "lambda", "objective"))

# Submit all experiments to your local system  
submitJobs(tab[1, ])

# Alternatively, submit them to the linux cluster with respective resources
# source("benchmarks/resources.R")
# submitJobs(tab, resources = resources.serial.default)
```

We again store the compromised results.

``` r
reg = loadRegistry("regs/tree_splitting")

# Check status of the experiments 
getStatus()

source("benchmarks/mlp/4_tree_splitting/reduce.R")
reduce_trees(reg, savedir = "data/runs/mlp_results")
```

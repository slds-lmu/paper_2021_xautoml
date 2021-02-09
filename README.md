# Explaining Hyperparameter Optimization via PDPs

This repository gives access to an implementation of the methods presented in the paper submission "Explaining Hyperparameter Optimization via PDPs", as well as all code that was used for the experimental analysis. 

This repository is structured as follows: 

```
    ├── renv/                   # renv configuration files to enable a reproducible setup 
    ├── R/                      # Implementation of all methods 
    ├── experiments/            # Source code for experimental analysis (section 6)
    │   ├── synthetic           # Synthetic function (section 6.1)
    │   ├── mlp                 # Synthetic function (section 6.1)
    ├── analysis/               # Analysis scripts used to create figures and tables in the paper
    ├── LICENSE
    └── README.md               # Introduction to this repository and usage of the methods in this paper
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

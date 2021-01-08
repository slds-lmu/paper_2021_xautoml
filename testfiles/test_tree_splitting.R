library(iml)
library(mlr)
library(data.table)
library(mlrMBO)
library(BBmisc)
library(customtrees)
library(Rfast)
library(ggplot2)
library(gridExtra)
library(partykit)
library(ggparty)

theme_set(theme_bw())

source("R/pdp_helpers.R") # helper functions to plot a partial dependence plot
source("R/tree_splitting.R") # helper functions to perform the tree splitting 
source("R/mlp_helper.r")


# -- SYNTHETIC FUNCTIONS 
id = "StyblinskiTang"
dimension = 5
lambda = 2
path = file.path("data/runs/synthetic", id, paste0(dimension, "D"))

# Get the objective
obj = readRDS(file.path(path, "objective.rds"))
ps = readRDS(file.path(path, "ps.rds"))
ps_ids = getParamIds(ps, repeated = TRUE, with.nr = TRUE)

# Get the run we are interested in 
run = readRDS(file.path(path, paste0("mlrmbo_run_lambda_", lambda, ".rds")))
model = run$models[[length(run$models)]]
best_candidate = as.data.frame(run$opt.path)[run$best.ind, ]

# Select the feature that we want to interpret
feature = "x3"
df = generateDesign(par.set = ps, n = 1000, fun = lhs::randomLHS)

tree = compute_tree(model = model,
            testdata = df, 
            feature = feature, 
            objective = "SS_L1",
            n.split = 3)


p = plot_tree_pdps(tree = tree, 
                df = df, 
                model = model, 
                pdp.feature = feature, 
                obj = obj, 
                method = "pdp_var_gp", 
                alpha = 0.01, 
                best_candidate = best_candidate
                )

# Check manually wether uncertainty reduced along the path
# Attention: this needs to be manually changed depending to in which node the optimum is  
sd_node_1_1 = cbind(node = "1_1", compute_pdp_for_node(node = tree[[1]][[1]], testdata = df, model = model, pdp.feature = feature, alpha = 0.01)$pdp_data)
sd_node_2_1 = cbind(node = "2_1", compute_pdp_for_node(node = tree[[2]][[1]], testdata = df, model = model, pdp.feature = feature, alpha = 0.01)$pdp_data)
sd_node_3_1 = cbind(node = "3_1", compute_pdp_for_node(node = tree[[3]][[1]], testdata = df, model = model, pdp.feature = feature, alpha = 0.01)$pdp_data)
sd_node_4_1 = cbind(node = "4_1", compute_pdp_for_node(node = tree[[4]][[1]], testdata = df, model = model, pdp.feature = feature, alpha = 0.01)$pdp_data)

sd_node = rbind(sd_node_1_1, sd_node_2_1, sd_node_3_1, sd_node_4_1)

p = ggplot(data = sd_node, aes(x = x3, y = sd, colour = node))
p + geom_line()

## -- MLP 

path = "data/runs/mlp/"

folder_mlp = "phoneme"
data = get_data(path, folder_mlp)
data = data[[paste0("data_", folder_mlp)]]

lambda = 1
iteration = 2

run = data[[paste0("mbo_lambda", lambda)]][[iteration]]
model = run$models[[length(run$models)]]
best_candidate = run$opt.path[which.min(run$opt.path$y), ]

# store this in a better format
ps =  readRDS("data/runs/mlp/ps.rds")
ps_ids = getParamIds(ps, repeated = TRUE, with.nr = TRUE)

feature = "learning_rate"
df = generateDesign(par.set = ps, n = 1000, fun = lhs::randomLHS)

tree = compute_tree(model = model,
            testdata = df, 
            feature = feature, 
            objective = "SS_L1",
            n.split = 2)

p1 = plot_pdp_for_node(node = tree[[1]][[1]],
                  testdata = df, 
                  model = model, 
                  pdp.feature = feature,
                  # objective.gt = obj,
                  method = "pdp_var_gp",
                  alpha = 0.01
                  ) 

p2 = plot_tree_pdps(tree = tree, 
                df = df, 
                model = model, 
                pdp.feature = feature, 
                # obj = obj, 
                method = "pdp_var_gp", 
                alpha = 0.05, 
                best_candidate = best_candidate
                )



## -- MLP 

path = "data/runs/xgboost/"

dataset = "phoneme"
data = readRDS(file.path(path, dataset, "mlrmbo_30_repls.rds"))

lambda = 1
iteration = 2

idx = which(data$lambda == lambda)[iteration]

run = data[idx, ]$result[[1]]
model = run$models[[length(run$models)]]
best_candidate = run$opt.path[which.min(run$opt.path$y), ]

# store this in a better format
ps =  readRDS(file.path(path, "ps.rds"))
ps_ids = getParamIds(ps, repeated = TRUE, with.nr = TRUE)

feature = "max_depth"
df = generateDesign(par.set = ps, n = 1000, fun = lhs::randomLHS)

tree = compute_tree(model = model,
            testdata = df, 
            feature = feature, 
            objective = "SS_L1",
            n.split = 3)

plot_tree_pdps(tree = tree, 
                df = df, 
                model = model, 
                pdp.feature = feature, 
                # obj = obj, 
                method = "pdp_var_gp", 
                alpha = 0.05, 
                best_candidate = best_candidate
                )







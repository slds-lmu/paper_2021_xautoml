source("R/pdp_helpers.R") # helper functions to plot a partial dependence plot
source("R/tree_splitting.R") # helper functions to perform the tree splitting 
library(ggplot2)
library(gridExtra)
library(partykit)
library(ggparty)
library(ParamHelpers)
library(smoof)
library(iml)
library(BBmisc)
library(data.table)
library(ggpubr)

theme_set(theme_pubr())

path = "data/runs/mlp/"

folder_mlp = "shuttle"

lambda = 1

# Read the MBO run
run = readRDS(file.path(path, folder_mlp, paste0("1_1_mlrmbo_runs/mlrmbo_run_lambda_", lambda, "_30repls.rds")))[1, ]$result[[1]]
opdf = run$opt.path

model = run$models[[2]]

best_candidate = opdf[which.min(opdf$y), ]

surr_val = readRDS(file.path(path, folder_mlp, "0_objective", "surrogate.rds"))$result[[1]]$model_val_balanced_acc[[1]]
obj = readRDS(file.path(path, folder_mlp, "0_objective", "obj.rds"))$obj
ps = getParamSet(obj)
ps_ids = getParamIds(ps)

df = readRDS(file.path(path, folder_mlp, "2_1_testdata", "testdata_1000.rds"))

# Get the tree
trees = readRDS(file.path(path, folder_mlp, "2_3_effects_and_trees", "eval_SS_L2_20_1000.rds"))

res = trees$result[[1]]$reslist[[1]]$weight_decay

tree = res$trees[[1]][1:2]

tree[[2]]$left.child$children = NULL
tree[[2]]$right.child$children = NULL

pdp.feature = "weight_decay"

res.ice = res$res.ice
pdp.gt.ice = readRDS(file.path(path, folder_mlp, "2_2_groundtruth_pdps", "gtpdp_20_1000.rds"))$pdp_ice_groundtruth$weight_decay[[1]]

# PDP for the source node

idx = tree[[1]][[1]]$subset.idx

pp = res.ice[which(res.ice$.id %in% idx),]
pp = setDT(pp)[, .(mean = mean(mean), sd = sqrt(mean(sd^2))), by = pdp.feature]  

pp.gt = pdp.gt.ice[which(pdp.gt.ice$.id %in% idx),]   
pp.gt = setDT(pp.gt)[, .(mean = mean(mean)), by = pdp.feature]     

pp$upper = pp$mean + 2 * pp$sd
pp$lower = pp$mean - 2 * pp$sd

p1 = ggplot()
p1 = p1 + geom_ribbon(data = pp, aes_string(x = pdp.feature, ymin = "lower", ymax = "upper"), alpha = 0.2)
p1 = p1 + geom_line(data = pp, aes_string(x = pdp.feature, y = "mean"), colour = "blue") 
p1 = p1 + geom_line(data = pp.gt, aes_string(x = pdp.feature, y = "mean")) 
p1 = p1 + ylim(c(0, 0.2)) + xlab(expression(lambda[s])) + ylab(expression(c[S]))
p1 = p1 + geom_vline(xintercept = best_candidate$weight_decay, colour = "orange")
p1

ggsave("shuttle_root_node.png", p1, width = 3.5, height = 2.3)


idx = tree[[2]][[1]]$subset.idx

pp = res.ice[which(res.ice$.id %in% idx),]
pp = setDT(pp)[, .(mean = mean(mean), sd = sqrt(mean(sd^2))), by = pdp.feature]     

pp.gt = pdp.gt.ice[which(pdp.gt.ice$.id %in% idx),]   
pp.gt = setDT(pp.gt)[, .(mean = mean(mean)), by = pdp.feature]     

pp$upper = pp$mean + 2 * pp$sd
pp$lower = pp$mean - 2 * pp$sd

p2 = ggplot()
p2 = p2 + geom_ribbon(data = pp, aes_string(x = pdp.feature, ymin = "lower", ymax = "upper"), alpha = 0.2)
p2 = p2 + geom_line(data = pp, aes_string(x = pdp.feature, y = "mean"), colour = "blue") 
p2 = p2 + ylim(c(0, 0.2)) + xlab(expression(lambda[s])) + ylab(expression(c[S]))
p2 = p2 + geom_line(data = pp.gt, aes_string(x = pdp.feature, y = "mean")) 
p2 = p2 + geom_vline(xintercept = best_candidate$weight_decay, colour = "orange")

p2

ggsave("shuttle_left_node.png", p2, width = 3.5, height = 2.3)


idx = tree[[2]][[2]]$subset.idx

pp = res.ice[which(res.ice$.id %in% idx),]
pp = setDT(pp)[, .(mean = mean(mean), sd = sqrt(mean(sd^2))), by = pdp.feature]     

pp.gt = pdp.gt.ice[which(pdp.gt.ice$.id %in% idx),]   
pp.gt = setDT(pp.gt)[, .(mean = mean(mean)), by = pdp.feature]     


pp$upper = pp$mean + 2 * pp$sd
pp$lower = pp$mean - 2 * pp$sd

p3 = ggplot()
p3 = p3 + geom_ribbon(data = pp, aes_string(x = pdp.feature, ymin = "lower", ymax = "upper"), alpha = 0.2)
p3 = p3 + geom_line(data = pp, aes_string(x = pdp.feature, y = "mean"), colour = "blue") 
p3 = p3 + ylim(c(0, 0.2)) + xlab(expression(lambda[s])) + ylab(expression(c[S]))
p3 = p3 + geom_line(data = pp.gt, aes_string(x = pdp.feature, y = "mean")) 

p3

ggsave("shuttle_right_node.png", p3, width = 3.5, height = 2.3)

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)



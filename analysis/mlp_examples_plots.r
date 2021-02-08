library(ggpubr)
source("R/helper_evaluation.r")
path = "data/trees/"
dataset = "shuttle"
data.list = readRDS(paste0(path, dataset, "/trees.rds"))
testdata = readRDS(paste0("data/testdata/", dataset, "/testdata_1000.rds"))

iter = 1
feat = "batch_size"

data = data.list$result[[1]]$reslist[[iter]][[feat]]
features = names (data.list$result[[1]]$reslist[[iter]])
optima = readRDS(paste0("data/optima/", dataset,"/mlrmbo_run_lambda_1_30repls.rds" ))
traindata = optima$result[[iter]]$opt.path
optimum = traindata[which.min(traindata$y), features]
gt = readRDS(paste0("data/groundtruth/", dataset, "/gtpdp_20_1000.rds"))
gt = gt[[1]][[feat]][[1]]
gt.pdp = gt[gt$.type=="pdp",]
gt.ice = gt[gt$.type == "ice",]

data.pdp = data$res.pdp
data.pdp$lower = data.pdp$mean - data.pdp$sd*1.96
data.pdp$upper = data.pdp$mean + data.pdp$sd*1.96
data.ice = data$res.ice
tree = data$trees
split.criteria = find_split_criteria(tree[[1]], optimum)
split.criteria$values = round(split.criteria$values, 4)

split.feat = tree[[1]][[1]][[1]]$split.feature
split.value = tree[[1]][[1]][[1]]$split.value
# first split indices
ind.left = tree[[1]][[2]]$left.child$subset.idx
ind.right = tree[[1]][[2]]$right.child$subset.idx

# calculate pdp for mean after splitting
pdp.left = data.ice[data.ice$.id %in% ind.left,] %>% group_by(get(feat)) %>% summarise(mean.left = mean(mean), sd.left = mean(sd))
pdp.left$lower = pdp.left$mean.left - pdp.left$sd.left*1.96
pdp.left$upper = pdp.left$mean.left + pdp.left$sd.left*1.96
colnames(pdp.left) = c(feat, "mean", "sd",   "lower" , "upper")
gt.left = gt.ice[gt.ice$.id %in% ind.left,] %>% group_by(get(feat)) %>% summarise(mean.left = mean(mean))
colnames(gt.left) = c(feat, "mean")

pdp.right = data.ice[data.ice$.id %in% ind.right,] %>% group_by(get(feat)) %>% summarise(mean.right = mean(mean), sd.right = mean(sd))
pdp.right$lower = pdp.right$mean.right - pdp.right$sd.right*1.96
pdp.right$upper = pdp.right$mean.right + pdp.right$sd.right*1.96
colnames(pdp.right) = c(feat, "mean", "sd",   "lower" , "upper")
gt.right = gt.ice[gt.ice$.id %in% ind.right,] %>% group_by(get(feat)) %>% summarise(mean.right = mean(mean))
colnames(gt.right) = c(feat, "mean")



# add cluster info to ice curves
data.ice$node = ifelse(data.ice$.id %in% ind.left, "left", "right")

theme_set(theme_pubr(legend = "none"))
y_lim = c(0.02, 0.19)

# PDP with confidence bands
p.pdp = ggplot(data = data.pdp, aes_string(x = feat, y = "mean")) + geom_line(colour = "blue", lwd = 1) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  geom_line(data = gt.pdp, lwd = 1) +
  geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
  ylim(y_lim) +
  xlab("Batch Size") + 
  ylab("c") + #ggtitle(paste0("root node: n = ", 1000))
  labs(title = paste0("Entire Hyperparameter Space, n = ", 1000),
       caption = paste(paste(rep("\n",4), collapse = " "))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic", size = 11),
        plot.title = element_text(size = 12),
        axis.title.y = element_text(size = 14))

# left pdp with confidence bands
p.pdp.left = ggplot(data = pdp.left, aes_string(x = feat, y = "mean")) + geom_line(colour = "blue", lwd = 1) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  geom_line(data = gt.left, lwd = 1) +
  geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
  ylim(y_lim) +
  xlab("Max. Units") + 
  ylab("c")  + #ggtitle(paste0("left child node: n = ", length(ind.left)))
  labs(title = paste0("Split 1: Optimal Sub-region, n = ", length(ind.right)),
       caption = paste("Sub-region Definition: \n", paste(split.criteria$features[c(1)],  " <= ",
                                                         split.criteria$values[c(1)], collapse = ", \n"),paste(rep("\n",3), collapse = " "))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic", size = 12),
        plot.title = element_text(size = 13),
        axis.title.y = element_text(size = 14))

# right pdp with confidence bands
p.pdp.right = ggplot(data = pdp.right, aes_string(x = feat, y = "mean")) + geom_line(colour = "blue", lwd = 1) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  geom_line(data = gt.right, lwd = 1) +
  geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
  ylim(y_lim) +
  ylab("c") + #ggtitle(paste0("right child node: n = ", length(ind.right))) +
  labs(title = paste0("split 1: Optimal Sub-region, n = ", length(ind.right)),
       caption = paste("Subregion Definition: \n", paste(split.criteria$features[c(1)], " > ",
                                                         split.criteria$values[c(1)], collapse = ", \n"),paste(rep("\n",3), collapse = " "))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic", size = 12),
        plot.title = element_text(size = 13),
        axis.title.y = element_text(size = 14))

p1 = gridExtra::grid.arrange(p.pdp, p.pdp.left, p.pdp.right, nrow = 1)



optimal.node = find_optimal_node(tree[[1]], optimum)

ind.opt = optimal.node$subset.idx
pdp.opt = data.ice[data.ice$.id %in% ind.opt,] %>% group_by(get(feat)) %>% summarise(mean.opt = mean(mean), sd.opt = mean(sd))
pdp.opt$lower = pdp.opt$mean.opt - pdp.opt$sd.opt*1.96
pdp.opt$upper = pdp.opt$mean.opt + pdp.opt$sd.opt*1.96
colnames(pdp.opt) = c(feat, "mean", "sd",   "lower" , "upper")
gt.opt = gt.ice[gt.ice$.id %in% ind.opt,] %>% group_by(get(feat)) %>% summarise(mean.opt = mean(mean))
colnames(gt.opt) = c(feat, "mean")


# right pdp with confidence bands

p.pdp.opt = ggplot(data = pdp.opt, aes_string(x = feat, y = "mean")) + geom_line(colour = "blue", lwd = 1) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  geom_line(data = gt.opt, lwd = 1) +
  geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
  ylim(y_lim) +
  ylab("c")  + #ggtitle(paste0("split 6: n = ", length(ind.opt), 
  xlab("Batch Size") +                                                                 #  split.criteria$features, ifelse(split.criteria$id == 1, " <= ", " > "),
                                                                    #       split.criteria$values))
  labs(title = paste0("Split 6: Optimal Sub-region, n = ", length(ind.opt)),
       caption = paste("Sub-region Definition:\n", paste(split.criteria$features[c(4:6)], ifelse(split.criteria$id[c(4:6)] == 1, " <= ", " > "),
                       split.criteria$values[c(4:6)], collapse = ", \n"), "\n")) +
  theme(plot.caption = element_text(hjust = 0, face= "italic", size = 11),
        plot.title = element_text(size = 12),
        axis.title.y = element_text(size = 14))
        

p1 = gridExtra::grid.arrange(p.pdp, p.pdp.opt, nrow = 1)

ggsave("figures/mlp_example1.png", p1, width = 10, height = 5)



p.pdp.opt = ggplot(data = pdp.opt, aes_string(x = feat, y = "mean")) + geom_line(colour = "blue", lwd = 1) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  geom_line(data = gt.opt, lwd = 1) +
  geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
  xlab("Max. Units") + 
  #geom_text(aes(x = as.numeric(optimum[feat]), y = 0, label = "optimum", colour = "orange"))
  ylim(y_lim) +
  ylab("c")  + #ggtitle(paste0("split 6: n = ", length(ind.opt), 
  #  split.criteria$features, ifelse(split.criteria$id == 1, " <= ", " > "),
  #       split.criteria$values))
  labs(title = paste0("Split 6: Optimal Sub-region, n = ", length(ind.opt)),
       caption = paste("Sub-region Definition: \n", paste(split.criteria$features[c(3:6)], ifelse(split.criteria$id[c(3:6)] == 1, " <= ", " > "),
                                                         split.criteria$values[c(3:6)], collapse = ", \n"))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic", size = 11),
        plot.title = element_text(size = 12),
        axis.title.y = element_text(size = 14))


p2 = gridExtra::grid.arrange(p.pdp, p.pdp.opt, nrow = 1)
p3 = grid.arrange(p2,p1, nrow = 2)
ggsave("figures/mlp_example.png", p3, width = 7.8, height = 5.7)



data.eval = readRDS(paste0("data/tree_splitting_reduced/",dataset, "/eval_SS_L2_20_1000.rds"))
data.eval = data.eval$result[[1]]$eval
feat = "batch_size"
data.eval.ex = data.eval[data.eval$model==iter & data.eval$feature==feat,]
data.eval.ex$neg_loglik.rel = (data.eval.ex$source.neg_loglik - data.eval.ex$neg_loglik)/abs(data.eval.ex$source.neg_loglik)

# relative improvement confidence at depth = 2
conf.rel.2 = data.eval.ex$conf.rel[data.eval.ex$depth==2]

# relative improvement confidence at depth = 7
conf.rel.7 = data.eval.ex$conf.rel[data.eval.ex$depth==7]

# relative improvement confidence (optimum) at depth = 2
conf.opt.rel.2 = data.eval.ex$conf.rel.opt.1[data.eval.ex$depth==2]

# relative improvement confidence (optimum) at depth = 7
conf.opt.rel.7 = data.eval.ex$conf.rel.opt.1[data.eval.ex$depth==7]

# relative improvement negative loglikelihood at depth = 2
nn.rel.2 = data.eval.ex$neg_loglik.rel[data.eval.ex$depth==2]

# relative improvement negative loglikelihood at depth = 7
nn.rel.7 = data.eval.ex$neg_loglik.rel[data.eval.ex$depth==7]

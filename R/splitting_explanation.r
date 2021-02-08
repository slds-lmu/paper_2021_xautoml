library(ggpubr)
path = "data/trees/"
dataset = "shuttle"
iter = 1
feat = "weight_decay"

data.list = readRDS(paste0(path, dataset, "/trees.rds"))
data = data.list$result[[1]]$reslist[[iter]][[feat]]
optima = readRDS(paste0("data/optima/", dataset,"/mlrmbo_run_lambda_1_30repls.rds" ))
traindata = optima$result[[iter]]$opt.path
optimum = traindata[which.min(traindata$y), features]

data.pdp = data$res.pdp
data.pdp$lower = data.pdp$mean - data.pdp$sd*1.96
data.pdp$upper = data.pdp$mean + data.pdp$sd*1.96
data.ice = data$res.ice
tree = data$trees
split.feat = tree[[1]][[1]][[1]]$split.feature
split.value = tree[[1]][[1]][[1]]$split.value
# first split indices
ind.left = tree[[1]][[2]]$left.child$subset.idx
ind.right = tree[[1]][[2]]$right.child$subset.idx

# calculate pdp for mean and sd after splitting
pdp.left = data.ice[data.ice$.id %in% ind.left,] %>% group_by(get(feat)) %>% summarise(mean.left = mean(mean), sd.left = mean(sd))
pdp.left$lower = pdp.left$mean.left - pdp.left$sd.left*1.96
pdp.left$upper = pdp.left$mean.left + pdp.left$sd.left*1.96
colnames(pdp.left) = c(feat, "mean", "sd",   "lower" , "upper")
pdp.right = data.ice[data.ice$.id %in% ind.right,] %>% group_by(get(feat)) %>% summarise(mean.right = mean(mean), sd.right = mean(sd))
pdp.right$lower = pdp.right$mean.right - pdp.right$sd.right*1.96
pdp.right$upper = pdp.right$mean.right + pdp.right$sd.right*1.96
colnames(pdp.right) = c(feat, "mean", "sd",   "lower" , "upper")

# add cluster info to ice curves
data.ice$node = ifelse(data.ice$.id %in% ind.left, "left", "right")

theme_set(theme_pubr(legend = "none"))

# PDP with confidence bands
p.pdp = ggplot(data = data.pdp, aes_string(x = feat, y = "mean")) + geom_line(colour = "blue", lwd = 1) + 
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + ylim(0.02,0.16) +
        geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
        xlab(~ paste(lambda["S"])) + ylab("performance") + ggtitle(paste0("root node: n = ", 1000))

# left pdp with confidence bands
p.pdp.left = ggplot(data = pdp.left, aes_string(x = feat, y = "mean")) + geom_line(colour = "blue", lwd = 1) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + ylim(0.02,0.16) +
  geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
  xlab(~ paste(lambda["S"])) + ylab("performance")  + ggtitle(paste0("left child node: n = ", length(ind.left)))

# right pdp with confidence bands
p.pdp.right = ggplot(data = pdp.right, aes_string(x = feat, y = "mean")) + geom_line(colour = "blue", lwd = 1) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + ylim(0.02,0.16) +
  geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
  xlab(~ paste(lambda["S"])) + ylab("performance")  + ggtitle(paste0("right child node: n = ", length(ind.right)))

p1 = gridExtra::grid.arrange(p.pdp, p.pdp.left, p.pdp.right, nrow = 1)
ggsave("figures/splitting_expl_perf.png", p1, width = 9.7, height = 2.9)

# sd ice curves that are clustered by tree

# before clustering
p.sd = ggplot(data.ice, aes_string(x = feat, y = "sd", group = ".id")) + geom_line(alpha = 0.5) +
  geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
  xlab(~ paste(lambda["S"])) + ylab(~ paste(hat("s"))) + ggtitle("Entire Hyperparameter Space") + ylim(0,0.033) +
  theme(plot.caption = element_text(hjust = 0, face= "italic", size = 12),
        plot.title = element_text(size = 13),
        axis.title.y = element_text(size = 14))

# p1 = ggplot(data.ice, aes_string(x = feat, y = "mean", color = "node")) + geom_line(alpha = 0.5,  aes(group = .id), show.legend = FALSE)  +
#   scale_color_manual(values = c( "#C3D7A4",  "#4E84C4")) + 
#   geom_line(data = pdp.left, aes_string(x = feat, y = "mean"), lwd = 1.3, col = "#52854C") +
#   geom_line(data = pdp.right, aes_string(x = feat, y = "mean"), lwd = 1.3, col = "#293352")
# p2 = ggplot(gt.ice, aes_string(x = feat, y = "mean", color = "node")) + geom_line(alpha = 0.5,  aes(group = .id), show.legend = FALSE)  +
#   scale_color_manual(values = c( "#C3D7A4",  "#4E84C4")) + 
#   geom_line(data = gt.left, aes_string(x = feat, y = "mean"), lwd = 1.3, col = "#52854C") +
#   geom_line(data = gt.right, aes_string(x = feat, y = "mean"), lwd = 1.3, col = "#293352")
# grid.arrange(p1, p2,
#              nrow = 1)

# ice curves and pdps of childnodes (best split)
p.sd.clust = ggplot(data.ice, aes_string(x = feat, y = "sd", color = "node")) + geom_line(alpha = 0.5,  aes(group = .id), show.legend = FALSE)  +
  scale_color_manual(values = c( "#C3D7A4",  "#4E84C4")) + 
  geom_line(data = pdp.left, aes_string(x = feat, y = "sd"), lwd = 1.3, col = "#52854C") +
  geom_line(data = pdp.right, aes_string(x = feat, y = "sd"), lwd = 1.3, col = "#293352") +
  geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
  xlab(~ paste(lambda["S"])) + ylab(~ paste(hat("s"))) + ggtitle(expression(paste("Entire Hyperparameter Space - Grouped"))) + ylim(0,0.033) +
  theme(plot.caption = element_text(hjust = 0, face= "italic", size = 12),
        plot.title = element_text(size = 13),
        axis.title.y = element_text(size = 14))




p.sd.left = ggplot(data.ice[data.ice$.id %in% ind.left,], aes_string(x = feat, y = "sd")) + geom_line(alpha = 0.5, aes(group = .id), col =  "#C3D7A4") +
  geom_line(data = pdp.left, aes_string(x = feat, y = "sd"), lwd = 1.3, col = "#52854C") + theme_pubr() +
  geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
  xlab(~ paste(lambda["S"])) + ylab(~ paste(hat("s"))) + ggtitle("Split 1: Left Sub-region") + ylim(0,0.033) +
  theme(plot.caption = element_text(hjust = 0, face= "italic", size = 12),
        plot.title = element_text(size = 13),
        axis.title.y = element_text(size = 14))

# sd curves of right child node
p.sd.right = ggplot(data.ice[data.ice$.id %in% ind.right,], aes_string(x = feat, y = "sd")) + geom_line(alpha = 0.5, aes(group = .id), col =  "#4E84C4") +
  geom_line(data = pdp.right, aes_string(x = feat, y = "sd"), lwd = 1.3, col = "#293352") + theme_pubr() +
  geom_vline(aes(xintercept = as.numeric(optimum[feat])), colour = "orange", lwd = 1) +
  xlab(~ paste(lambda["S"])) + ylab(~ paste(hat("s"))) + ggtitle("Split 1: Right Sub-region") + ylim(0,0.033) +
  theme(plot.caption = element_text(hjust = 0, face= "italic", size = 12),
        plot.title = element_text(size = 13),
        axis.title.y = element_text(size = 14))


p2 = gridExtra::grid.arrange(p.sd, p.sd.clust, p.sd.left, p.sd.right, nrow = 2)
ggsave("figures/splitting_expl_sd.png", p2, width = 8, height = 4.3)

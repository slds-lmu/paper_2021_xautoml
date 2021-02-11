library(gdata)
library(ggplot2)


# CREATE DATASET FOR VISUALIZATION

# path to folder, with folders for dataset results
path = "data/mlp_eval/"
datasets = list.files(path)

# create list with one list containing one dataframe per dataset
data.all = lapply(datasets, function(data){
  
  data_path = paste0(path, data, "/")
  objectives = list.files(data_path)
  trees = which(startsWith(objectives, "effects_trees_", trim = TRUE))
  if(length(trees) > 0) objectives = objectives[-trees]
  
  for(i in 1:length(objectives)){
 
    res = readRDS(paste0(data_path, objectives[i]))
    df_sub = res$result[[1]]$eval
    df_sub$objective = res$objective
    
    if(i == 1) df = df_sub
    else df = rbind(df, df_sub)
  }
  
  return(df)
})
names(data.all) = datasets

library(dplyr)
library(xtable)
# CREATE TABLE FOR PAPER
create_table_datasets = function(data.list, objective, target, depth){
  df = lapply(1:length(data.list), function(i){
    
    data = data.list[[i]]
    data = data[which(data$objective == objective & data$depth == depth),]
    if(target == "neg_loglik.rel"){
      data$neg_loglik.rel = (data$source.neg_loglik - data$neg_loglik)/abs(data$source.neg_loglik)
    }
    if(target == "gt.diff.sd"){
      data$gt.diff.sd = (data$source.gt.diff.sd - data$gt.diff.sd)/data$source.gt.diff.sd
    }
    data.features = data %>% group_by(feature) %>% summarise(mean = mean(get(target)))
    data.frame("dataset" = names(data.list)[i], "mean" = mean(data[,target]), "sd" = sd(data[,target]),
               "best.feat" = data.features[which.max(data.features$mean), "feature"], "best.mean" = data.features[which.max(data.features$mean), "mean"],
               "worst.feat" = data.features[which.min(data.features$mean), "feature"], "worst.mean" = data.features[which.min(data.features$mean), "mean"])
    
  })
  return(do.call("rbind", df))
}



df = lapply(1:length(data.all), function(i){
  
  data = data.all[[i]]
  data = data[which(data$objective == objective & data$depth == depth),]
  if(target == "neg_loglik.rel"){
    data$neg_loglik.rel = (data$source.neg_loglik - data$neg_loglik)/abs(data$source.neg_loglik)
  }
  data$dataset = names(data.all)[i]
  data.model = data %>% group_by(dataset, feature) %>% summarise(mean_conf = mean(conf.rel), mean_conf.opt = mean(conf.rel.opt.1), mean_neg.loglok = mean(neg_loglik.rel))
  #data.features
  
  as.data.frame(data.model)
  
})
df.new = do.call("rbind", df)
df.new = df.new[,c("dataset", "feature", )]

# analysis of confidence improvement
df_datasets = create_table_datasets(data.all, "SS_L2", "gt.diff.sd", 7)
df_feat_worst = as.data.frame(df_datasets %>% group_by(feature.1) %>% summarise(n()))
df_feat_best = as.data.frame(df_datasets %>% group_by(feature) %>% summarise(n()))
df_feat = left_join(df_feat_best, df_feat_worst, by = c("feature"= "feature.1"))

df_datasets[,c("mean","sd","mean.1", "mean.2")] = df_datasets[,c("mean","sd","mean.1", "mean.2")]*100
print(xtable(df_datasets[,-c(4,6)], digits = 0), include.rownames=FALSE)

# analysis of neg loglik improvement
df_datasets = create_table_datasets(data.all, "SS_L1", "neg_loglik.rel", 7)
df_feat_worst = as.data.frame(df_datasets %>% group_by(feature.1) %>% summarise(n()))
df_feat_best = as.data.frame(df_datasets %>% group_by(feature) %>% summarise(n()))
df_feat_loglik = left_join(df_feat_best, df_feat_worst, by = c("feature"= "feature.1"))
df_feat = left_join(df_feat, df_feat_loglik, by = "feature")

df_datasets[,c("mean","sd","mean.1", "mean.2")] = df_datasets[,c("mean","sd","mean.1", "mean.2")]*100
print(xtable(df_datasets[,-c(4,6)], digits = 0), include.rownames=FALSE)
print(xtable(df_feat), include.rownames=FALSE)

create_table_features = function(data.list, objective, target, depth){
  
  df = lapply(1:length(data.list), function(i){
    
    data = data.list[[i]]
    data = data[which(data$objective == objective & data$depth == depth),]
    if(target == "neg_loglik.rel"){
      data$neg_loglik.rel = (data$source.neg_loglik - data$neg_loglik)/abs(data$source.neg_loglik)
    }
    if(target == "neg_loglik"){
      data$neg_loglik = (data$source.neg_loglik - data$neg_loglik)
    }
    if(target == "gt.diff.sd"){
      data$gt.diff.sd = (data$source.gt.diff.sd - data$gt.diff.sd)/data$source.gt.diff.sd
    }
    data.features = data %>% group_by(feature) %>% summarise(mean = median(get(target)))
    data.features$dataset = names(data.list)[i]
    data.features
    
  })
  df = do.call("rbind", df)
  df %>% group_by(feature) %>% summarise(mean.feat = mean(mean), sd = sd(mean))
}

target = "gt.diff.sd"
df_features = create_table_features(data.all, "SS_L2", target, 7)
df_features_opt = create_table_features(data.all, "SS_L1", "conf.rel.opt.1", 7)
df_features = cbind(df_features, df_features_opt[,2:3])
df_features[,2:5] = df_features[,2:5]*100
print(xtable(df_features), include.rownames=FALSE)


# CREATE PLOT AND STORE PLOTS

# plotpath where plots are stores
plotpath = "figures/"
# evaluation metrics (one plot for each of them)
eval_metrics = c("conf.rel", "conf.rel.opt.1", "gt.abs", "neg_loglik")

lapply(1:length(data.all), function(i){
  data = data.all[[i]]
  for(eval in eval_metrics){
    p = ggplot(data, aes_string(x = "objective", y = eval)) + geom_boxplot(aes(fill = as.factor(depth)))  + facet_grid(. ~ feature)
    ggsave(file.path(plotpath, paste0(names(data.all)[i], eval, ".png")), p, width = 12, height = 3)
    
  }
})



data_combined = do.call("rbind", data.all)
ggplot(data_combined, aes_string(x = "objective", y = "gt.abs")) + geom_boxplot(aes(fill = as.factor(depth)))  + facet_grid(. ~ feature)

data_SS_L1 = data_combined[data_combined$objective=="SS_L1",]
data_SS_sd = data_combined[data_combined$objective=="SS_sd",]
data_SS_L1$conf.diff_sd = data_combined[data_combined$objective=="SS_sd",]$conf.diff
data_SS_L1$conf.diff_L2 = data_combined[data_combined$objective=="SS_L2",]$conf.diff
data_SS_L1$conf.diff_area = data_combined[data_combined$objective=="SS_area",]$conf.diff

data_SS_L1$conf.diff.opt_sd = data_combined[data_combined$objective=="SS_sd",]$gt.abs

for(i in 1:nrow(data_SS_L1)){
  ranks = rank(data_SS_L1[i, c("conf.diff","conf.diff_sd","conf.diff_L2","conf.diff_area")])
  data_SS_L1$L1_rank[i] = ranks[1]
  data_SS_L1$sd_rank[i] = ranks[2]
  data_SS_L1$L2_rank[i] = ranks[3]
  data_SS_L1$area_rank[i] = ranks[4]
}


for(i in 1:nrow(data_SS_L1)){
  ranks = rank(data_SS_L1[i, c("conf.diff","conf.diff_sd")])
  data_SS_L1$L1_rank[i] = ranks[1]
  data_SS_L1$sd_rank[i] = ranks[2]
  #data_SS_L1$L2_rank[i] = ranks[3]
  #data_SS_L1$area_rank[i] = ranks[4]
}

data_SS_L1_7 = data_SS_L1[data_SS_L1$depth==8,]
ggplot(data = data_SS_L1_7, aes(x = sd_rank))  + geom_bar(aes(y = (..count..)/sum(..count..))) 

summary(data_SS_L1$conf.diff_sd)

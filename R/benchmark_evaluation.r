library(gdata)
library(ggplot2)


# CREATE DATASET FOR VISUALIZATION

# path to folder, with folders for dataset results
path = "data/runs/mlp_new/"
datasets = list.files(path)

# create list with one list containing one dataframe per dataset
data.all = lapply(datasets, function(data){
  data_path = paste0(path, data, "/2_3_effects_and_trees/")
  objectives = list.files(data_path)
  trees = which(startsWith(objectives, "effects_trees_", trim = TRUE))
  if(length(trees) > 0) objectives = objectives[-trees]
  
  for(i in 1:length(objectives)){
 
    res = readRDS(paste0(data_path, objectives[i]))
    df_sub = res$result[[1]]
    df_sub$objective = res$objective
    
    if(i == 1) df = df_sub
    else df = rbind(df, df_sub)
  }
  
  return(df)
})
names(data.all) = datasets

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


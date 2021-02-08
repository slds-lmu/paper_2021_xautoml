library(devtools)
load_all("../paper_2019_dependent_features/R/")
feature = "batch_size"
mmd = c()
for(i in 1:10){
  optimum = optima[[i]]
  traindata = data$phoneme$result[[i]]$opt.path[,features]
  mmd.all = mmd2(as.data.frame(scale(traindata)), as.data.frame(scale(testdata)))
  
  tree = reslist[[i]][[feature]]$trees$SS_L2
  split.criteria = find_split_criteria(tree, optimum)
  traindata.opt = find_optimal_subset(traindata, split.criteria)[features]
  
  ps.opt = ps
  for(j in 1:length(split.criteria$features)){
    if(split.criteria$id[j] == 1) ps.opt$pars[[split.criteria$features[j]]]$upper = split.criteria$value[j]
    else ps.opt$pars[[split.criteria$features[j]]]$lower = split.criteria$value[j]
  }
  
  testdata.opt = generateRandomDesign(n = testdata.size, par.set = ps.opt)[features]
  
  mmd.opt = mmd2(as.data.frame(scale(traindata.opt)), as.data.frame(scale(testdata.opt)))
  
  mmd = c(mmd, (mmd.all - mmd.opt))
  
}





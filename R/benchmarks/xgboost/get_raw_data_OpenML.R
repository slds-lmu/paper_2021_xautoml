# This script creates some datasets to be analyzed with techniques of IML 

library(OpenML)
library(data.table)
library(mlr)

dataset_dir = "data/raw"

# List of datasets
# taken from the AutoML challenge 
study_id = 218
study_info = getOMLStudy(study_id)
data_ids = study_info$data$data.id

data_info = data.table(data_id = data_ids)

# download all the tasks 
res = lapply(data_info$data_id, function(did) {


	oml_data = getOMLDataSet(did)

	data_dir = file.path(dataset_dir, oml_data$desc$name)
	dir.create(file.path(data_dir))

	data = oml_data$data
	target = oml_data$target	
	task = makeClassifTask(id = oml_data$desc$name, data, target = target)

	saveRDS(data, file.path(data_dir, "data.rds"))
	saveRDS(task, file.path(data_dir, "task.rds"))	

	tdesc = task$task.desc
	nfeats = data.table(t(tdesc$n.feat))
	missings = tdesc$has.missings

	cbind(data.table(task_type = tdesc$type, classes = length(tdesc$class.levels), n = tdesc$size, p = sum(tdesc$n.feat)), nfeats)
})

files = list.files(dataset_dir, full.names = TRUE)

res = lapply(files, function(f) {

	task = readRDS(file.path(f, "task.rds"))	
	desc = task$task.desc

	target = desc$target
	type = desc$type
	nfeats = data.table(t(desc$n.feat))
	missings = desc$has.missings

	cbind(data.table(id = desc$id, task_type = type, target = target, missings = missings, classes = length(desc$class.levels), n = desc$size, p = sum(desc$n.feat)), nfeats)
})

res = do.call(rbind, res)

saveRDS(res, file.path(dataset_dir, "description.rds"))

res[classes == 2 & factors == 0, ]
import os
import pandas as pd 

# https://github.com/automl/LCBench
from api import Benchmark

# Download from https://ndownloader.figshare.com/files/21188598 and unzip
bench_dir = "data_2k_lw.json"
bench = Benchmark(bench_dir, cache=True)

path = "data/runs/mlp/"

dataset_names = bench.get_dataset_names()
openml_task_ids = bench.get_openml_task_ids()

os.mkdir(path)

for task in dataset_names:
	nrun = bench.get_number_of_configs(task)
	df = pd.DataFrame([bench.query(dataset_name=task, tag="config", config_id=0)])
	df['final_val_accuracy'] = bench.query(dataset_name=task, tag="final_val_accuracy", config_id=0)
	df['final_test_accuracy'] = bench.query(dataset_name=task, tag="final_val_accuracy", config_id=0)
	df['final_val_balanced_accuracy'] = bench.query(dataset_name=task, tag="final_val_accuracy", config_id=0)
	df['final_test_balanced_accuracy'] = bench.query(dataset_name=task, tag="final_val_accuracy", config_id=0)
	data = []
	for i in range(1, nrun):
		config = bench.query(dataset_name=task, tag="config", config_id=i)
		config['final_val_accuracy'] = bench.query(dataset_name=task, tag="final_val_accuracy", config_id=i)
		config['final_test_accuracy'] = bench.query(dataset_name=task, tag="final_val_accuracy", config_id=i)
		config['final_val_balanced_accuracy'] = bench.query(dataset_name=task, tag="final_val_accuracy", config_id=i)
		config['final_test_balanced_accuracy'] = bench.query(dataset_name=task, tag="final_val_accuracy", config_id=i)
		data.append(config)
	df = df.append(data, True)
	target_dir = path + task + "/"
	os.mkdir(target_dir)
	df.to_csv(target_dir + 'lcbench2000.csv')



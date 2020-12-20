import sys
sys.path.append('/home/julia/Documents/repos/LCBench')

from pprint import pprint
import matplotlib.pyplot as plt
import pandas as pd 

from api import Benchmark

bench_dir = "/home/julia/Documents/repos/LCBench/cached/data_2k_lw.json"
bench = Benchmark(bench_dir, cache=True)

queriable_tags = bench.get_queriable_tags()
pprint(queriable_tags)

dataset_names = bench.get_dataset_names()
openml_task_ids = bench.get_openml_task_ids()

print(dataset_names)
print(openml_task_ids)

task = 'blood-transfusion-service-center'

# Get an example for a loss log
example_loss = bench.query(dataset_name=task, tag="Train/loss", config_id=0)

# Get the log of the accuracy for the run with the best peak accuracy
example_best_acc = bench.query_best(task, "Train/val_accuracy", "Train/val_accuracy", 0)

print("Example loss log:\n", example_loss)
print("Best validation accuracy log:\n", example_best_acc)



# Query the final validation performance for all configs 

# number of configs
path = "/home/julia/Documents/repos/paper_2020_xautoml/data/runs/mlp/"
target_dir = path + task + "/"

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


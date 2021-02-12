# Reproduce Experiments

To perform the experiments on a cluster, we used the package
`bachtools`. The respective scripts are based on a submission to a Linux
cluster with [Slurm](https://slurm.schedmd.com/documentation.html) as
workload manager.

| Computing Infrastructure | Linux Cluster           |
| ------------------------ | ----------------------- |
| Architecture             | 28-way Haswell-EP nodes |
| Cores per Node           | 1                       |
| Memory limit (per core)  | 2.2 GB                  |

| Section | Type of Experiment | Link                                    |
| ------- | ------------------ | --------------------------------------- |
| 6.1     | Synthetic Function | [link](synthetic/README.md) |
| 6.2     | MLP                | [link](mlp/README.md)       |

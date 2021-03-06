#!/bin/bash
#SBATCH -J RF_test
#SBATCH -N 1
#SBATCH --partition=mem_0128
#SBATCH --qos=devel_0128
##SBATCH --account=lv71284
#SBATCH --mail-user=giulio.genova@eurac.edu
#SBATCH --mail-type=BEGIN,END
##SBATCH --time-min=3

module purge
module load intel/18 intel-mkl/2018 R/3.6.2 gdal/2.4.1 proj/4.9.3

Rscript $HOME/data/biomod4alps/launch_run_models_cmd.R RF 4
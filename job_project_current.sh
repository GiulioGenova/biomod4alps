#!/bin/bash
#SBATCH -J current
#SBATCH -N 1
#SBATCH --partition=mem_0064
#SBATCH --qos=normal_0064
##SBATCH --account=lv71284
##SBATCH --mail-user=giulio.genova@eurac.edu
#SBATCH --mail-user=francesco.rota@education.unibz.it
#SBATCH --mail-type=BEGIN,END
##SBATCH --time-min=3

module purge
module load intel/18 intel-mkl/2018 R/3.6.2 gdal/2.4.1 proj/4.9.3

## Rscript $HOME/data/biomod4alps/launch_project_current.R all 15

Rscript $HOME/data/biomod4alps/launch_project_current.R --cores 15  --workdir $HOME/data/ --scriptdir $HOME/data/biomod4alps -s  2 5 8 --models GAM


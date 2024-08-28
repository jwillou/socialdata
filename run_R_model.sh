#!/bin/bash

#SBATCH --job-name=henry_ABM
#SBATCH --partition=general  #general   #jrw0107_std #(up to 192 but spread over all 20) so 192/20
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -t 300:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=jrw0107@auburn.edu
#SBATCH --mem=32000M

module load R/4.2.1

cd /scratch/jrw0107_infoex/tourtolocal1

Rscript Cover.R  

### #SBATCH -mem=[0,35]

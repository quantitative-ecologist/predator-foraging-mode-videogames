#!/bin/bash
#SBATCH --account=def-monti
#SBATCH --nodes=1
#SBATCH --cpus-per-task=40
#SBATCH --mem=64000M
#SBATCH --time=0-06:00
#SBATCH --mail-user=fraser_franco.maxime@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020
module load gcc/9.3.0
module load r-bundle-bioconductor/3.14
module load r/4.1.2

#export R_LIBS=~/.local/R/$EBVERSIONR/
Rscript --no-save --no-restore 03B_hunting_success_base-model2.R
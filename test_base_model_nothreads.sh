#!/bin/bash
#SBATCH --account=def-monti
#SBATCH --nodes=1
#SBATCH --cpus-per-task=5
#SBATCH --mem=28000M
#SBATCH --time=0-01:30
#SBATCH --mail-user=fraser_franco.maxime@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 gcc/9.3.0
module load r/4.0.2
module load r-bundle-bioconductor/3.12

#export R_LIBS=~/.local/R/$EBVERSIONR/
Rscript --no-save --no-restore test_base_model_nothreads.R
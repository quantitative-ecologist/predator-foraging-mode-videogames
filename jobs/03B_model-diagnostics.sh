#!/bin/bash
#SBATCH --account=def-monti
#SBATCH --nodes=1
#SBATCH --cpus-per-task=20
#SBATCH --mem=50G
#SBATCH --time=0-18:00
#SBATCH --mail-user=fraser_franco.maxime@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020
module load gcc/9.3.0
module load r-bundle-bioconductor/3.12
module load r/4.0.2

#export R_LIBS=~/.local/R/$EBVERSIONR/
Rscript --no-save --no-restore 03B_model-diagnostics.R
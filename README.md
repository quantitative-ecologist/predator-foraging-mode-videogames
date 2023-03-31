# Predator foraging mode in videogames : Online repository for Fraser Franco et al. (2022)

![](https://img.shields.io/badge/license-CC%20BY--NC%204.0-green?style=for-the-badge)

## Description

Online repository to reproduce the results presented in :

Fraser Franco, M., Santostefano, F., Kelly, Clint D., Montiglio, P.-O. (2022) Studying predator foraging mode and hunting success at the individual level with an online videogame. *Behavioral Ecology*, *33*(5), 967-978. [https://doi.org/10.1093/beheco/arac063](https://doi.org/10.1093/beheco/arac063)

You can click [here](https://osf.io/wyvrt/) for a link to our associated OSF repository. Please use this DOI if you wish to cite our dataset : https://doi.org/10.17605/OSF.IO/WYVRT

## General coding workflow

All the R scripts are enumerated (see the [code](./code) folder). You will find the specific outputs generated from the R scripts in the [outputs](./outputs) folder. Each output file has a number that correponds to the R script used to generate it.

The analysis was made in 5 steps which is represented by the number of the .R file. Here is a description of the process :

1. Data exploration
2. Principal component analysis
3. Modelling / model validation / model processing
4. Produce figures
5. Produce tables

## Workflow to compute the models

I ran all the models on a remote computer cluster ([Cedar, Compute Canada](https://docs.computecanada.ca/wiki/Cedar)). You can open the .sh files in the [jobs](./jobs) folder to see the computer specifications used to run the models. The computer cluster runs on CentOS Linux 7.

Here is a table showing the workflow employed to generate the model outputs using R. This workflow is exclusively for .R files used to compute the Bayesian mixed models with the package "brms" (see the [code_models](./code/code_models) folder).

| Generator              | Operating system | Programming language | Code               | Results                  |
| ---------------------- | ---------------- | -------------------- | ------------------ | ------------------------ |
| Compute Canada / Cedar | CentOS Linux 7   | R                    | [code](./code) folder | [outputs](./outputs) folder |

**Note** : The model outputs were too large (~3.69Gb) for me to push them to this repository. Please see the section below for a description on how to access the files.

## Reproducibility

### Model outputs

You can reproduce every step of the analysis in your personal computer except for step 3 as it was run on a remote computer cluster (see section above). Steps 4 and 5 cannot be reproduced directly because the model outputs are not in this GitHub repository (as explained in the section above).

Here is what you can do to access the files :

1. Download the outputs as a folder in our [OSF repository](https://osf.io/wyvrt/) and name it "models"
2. Include the "models" folder as a subfolder in the [outputs](./outputs) folder when you download our GitHub repository in your computer

After that, you should be able to go on and reproduce steps 4 and 5.

### R packages versions

I used the [renv](https://rstudio.github.io/renv/index.html) package to contain each individual R package version within the project. If you wish to reproduce our results in your personal computer (excluding the model files that were run on Compute Canada), please refer to the official renv [collaborating vignette](https://rstudio.github.io/renv/articles/collaborating.html) to implemement the workflow in your personal computer.

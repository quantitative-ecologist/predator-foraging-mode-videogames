# Predator foraging mode in videogames : Online repository for Fraser Franco et al. (in press)

![](https://img.shields.io/badge/license-CC%20BY--NC%204.0-green?style=for-the-badge)

## Description

Online repository to reproduce the results presented in "Studying predator foraging mode and hunting success at the individual level with an online videogame" (in press).

(A url/doi will be added pointing to the original article)

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

**Note** : The model outputs were too large (~5Gb) for me to push them to this repository. I will gladly share the files to anyone who wishes to reproduce the results (processing, validation, plots, and tables).

## Reproducibility

You can reproduce every step of the analysis in your personal computer except for step 3. Steps 4 and 5 cannot technically be reproduced because the model outputs are not in the repository (as I explained above). However, once we find a way to share the large files, you will be able to reproduce steps 4 and 5.

I used the [renv](https://rstudio.github.io/renv/index.html) package to contain each individual R package version within the project. If you wish to reproduce our results in your personal computer (excluding the model files that were run on Compute Canada), please refer to the official renv [collaborating vignette](https://rstudio.github.io/renv/articles/collaborating.html) to implemement the workflow in your personal computer.

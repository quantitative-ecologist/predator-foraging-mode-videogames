# Predator foraging mode in videogames

Online repository to reproduce the results presented in "Studying predator foraging mode and hunting success at the individual level with an online videogame" (in press). (A url will be added pointing to the original article)

## Description

All models were run on a remote computer cluster ([Cedar, Compute Canada](https://docs.computecanada.ca/wiki/Cedar)). You can open the .sh files in the [jobs folder](./jobs) to see the computer specifications used to run the models. The computer cluster runs on CentOS Linux 7.

## Coding Workflow

| Generator | Operating system | Programming language | Code | Results |
| --------- | ---------------- | -------------------- | ---- | ------------ |
| Compute Canada / Cedar | CentOS Linux 7 | R | [code folder](./code) | [outputs folder](./outputs) |

All the R scripts are enumerated. You will find the specific outputs generated from the R scripts in the [outputs folder](./outputs). Each output file has a number that correponds to the R script used to generate it.

**Note** : The model outputs were too large (~5Gb) for me to push them to this repository. I will gladly share the files to anyone who wishes to reproduce the results (plots and tables).

## Reproducibility

We used the [renv](https://rstudio.github.io/renv/index.html) package to contain each individual R package version within the project. Please refer to the [collaborating vignette](https://rstudio.github.io/renv/articles/collaborating.html) too see how to implemement the workflow in your personal computer if you wish to reproduce our results.

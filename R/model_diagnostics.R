
library(data.table)
library(brms)
library(lme4)



logit_inverse <- function(x){exp(x)/(1+exp(x))}

load("C:/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/PhD_project/old_R_stuff/ancien_code_chap1/03B_base-model.rda")

load("base_model_threads.rda")



X <- model.matrix(model)

var_fixef <- X %*% fixef(model)

nrow(X)
head(X)

Z <- model.matrix(base_model)


# Diagnose functions
pp_check(base_model)

##########################################################################

#          Predator repeatability and behavioural correlations           #

##########################################################################

# Code for two multivariate mixed-models to :
# 1. partition the variance in predator behaviour
# 2. quantify behavioural correlations among predator behaviours
# 3. evaluate how predators respond to prey behaviour

# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec
# -----------------------------------------------------------------------





# =======================================================================
# 1. Load libraries, and export dataset
# =======================================================================

# Detect number of cores
options(mc.cores = parallel::detectCores())

# Import libraries
library(data.table)
library(MCMCglmm)
library(parallel)

# load dataset
data <- fread("/home/maxime11/projects/def-monti/maxime11/data/02_merged-data.csv",
              select = c("mirrors_id", "match_id", "character_name",
                         "map_name", "hunting_success", "sqrtspeed", 
                         "sqrtspace_covered_rate", "sqrtprox_mid_guard",
                         "sqrthook_start_time", "sqrtsurv_speed",
                         "sqrtsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare data for the model
# =======================================================================

# Normalize sqrt variables (Z-scores)
# -------------------------------------------------------
standardize <- function (x) {(x - mean(x)) / sd(x)}

data[, c("Zsqrtspeed", "Zsqrtspace_covered_rate", "Zsqrtprox_mid_guard",
         "Zsqrt_hook_start_time", "Zsqrtsurv_speed", 
         "Zsqrtsurv_space_covered_rate") :=
                lapply(.SD, standardize), 
                .SDcols = c(6, 7, 8, 9, 10, 11)]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Build the multivariate model 
# =======================================================================
# ( nitt - burnin ) / thin = 1000
# nitt: number of iterations, it has to be nitt = (thin*10000) + burnin

# Assign prior for variance-covariance matrix (inverse-Wishart) and random effects
#prior <- list(R = list(V = diag(3), nu = 2.002), 
#              G = list(G1 = list(V = diag(3), 
#                                 nu = 2.002, 
#                                 alpha.mu = rep(0, 3),
#                                 alpha.V = diag(3)*25^2),
#                       G2 = list(V = diag(3), 
#                                 nu = 2.002, 
#                                 alpha.mu = rep(0, 3),
#                                 alpha.V = diag(3)*25^2),
#                       G3 = list(V = diag(3), 
#                                 nu = 2.002, 
#                                 alpha.mu = rep(0, 3),
#                                 alpha.V = diag(3)*25^2)))

prior <- list(R = list(V = diag(3), nu = 2.002), 
              G = list(G1 = list(V = diag(3), 
                                 nu = 2.002),
                       G2 = list(V = diag(3), 
                                 nu = 2.002),
                       G3 = list(V = diag(3), 
                                 nu = 2.002),
                       G4 = list(V = diag(3), 
                                 nu = 2.002)))

# First model
# ------------------------------------------------
#ModMV1 <- MCMCglmm(
#                  cbind(Zsqrtspeed,
#                        Zsqrtspace_covered_rate,
#                        Zsqrtprox_mid_guard,
#                        Zsqrt_hook_start_time) ~ 
#                  (trait - 1), # set a general intercept for response,
#                  random = ~us(trait):mirrors_id + 
#                            us(trait):map_name + 
#                            us(trait):character_name, # random intercepts
#                  rcov = ~us(trait):units, # residual matrix
#                  prior = prior,
#                  nitt = 153000, 
#                  thin = 100, 
#                  burnin = 3000, 
#                  verbose = TRUE, 
#                  family = c("gaussian", 
#                             "gaussian",
#                             "gaussian",
#                             "gaussian"), # distribution of responses
#                  data = data, 
#                  pr = TRUE, 
#                  saveX = TRUE,
#                  saveZ = TRUE
#                  )
#
#save(ModMV1, file = "03A_multivariate-model.rda")
#load("03A_multivariate-model1.rda")

set.seed(20210419) # day the model was run
mv_model <- mclapply(1:20, function(i) 
             {
              MCMCglmm(
                  cbind(Zsqrtspeed,
                        Zsqrtspace_covered_rate,
                        Zsqrtprox_mid_guard,
                        Zsqrt_hook_start_time) ~ 
                  (trait - 1), # set a general intercept for response,
                  random = ~us(trait):mirrors_id + 
                            us(trait):map_name + 
                            us(trait):character_name, # random intercepts
                  rcov = ~us(trait):units, # residual matrix
                  prior = prior,
                  nitt = 153000, 
                  thin = 100, 
                  burnin = 3000, 
                  verbose = TRUE, 
                  family = c("gaussian", 
                             "gaussian",
                             "gaussian",
                             "gaussian"), # distribution of responses
                  data = data, 
                  pr = TRUE, 
                  saveX = TRUE,
                  saveZ = TRUE
                  )
               }, mc.cores = 20)

save(mv_model, file = "03A_multivariate-model.rda")

# Save session info for reproducibility
session <- sessionInfo()
capture.output(session, file = "03A_sessioninfo.txt")


# Second model with effect of prey behaviour
# ------------------------------------------------
#system.time(
#ModMV2 <- MCMCglmm(
#                  cbind(Zsqrtspeed,
#                        Zsqrtspace_covered_rate,
#                        Zsqrtprox_mid_guard) ~ 
#                  (trait - 1) + # set a general intercept for each predictor
#                  trait:Zsqrtsurv_speed +
#                  trait:Zsqrtsurv_space_covered_rate,
#                  random = ~us(trait):mirrors_id + 
#                            us(trait):map_name + 
#                            us(trait):character_name, # random intercepts
#                  rcov = ~us(trait):units, # residual matrix
#                  prior = prior,
#                  nitt = 203000,
#                  thin = 100,
#                  burnin = 3000, 
#                  verbose = TRUE, 
#                  family = c("gaussian",
#                             "gaussian",
#                             "gaussian"), # distribution of responses
#                  data = data
#                  )
#           )
#
#save(ModMV2, file = "03A_multivariate-model2.rda")
#load("03A_multivariate-model2.rda")
# ======================================================================
# ======================================================================

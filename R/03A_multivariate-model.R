#######################################################
#### A. Chapter 1. Predator behavioural expression ####
#######################################################
# Build two multivariate mixed-model to :
# 1. partition the variance of predator behaviour
# 2. evaluate how predators respond to prey behaviour


# Build a small model to evaluate prey repeatability of space covered rate (to do maybe)
# (to assess if prey are leading the predator-prey interaction)





# 1. Set working directory, load libraries, and export dataset ==========
# =======================================================================
# personal computer onedrive UQAM Montiglio lab
setwd("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/masters_project/chapter1/outputs")

# Import libraries
library(data.table)
library(ggplot2)
library(GGally)
library(MCMCglmm)

# load dataset
behaviour_data <- fread("02_merged-data.csv")



# Character variables to factor variables
char_as_factor <- names(behaviour_data)[sapply(behaviour_data, is.character)] # extract columns that are characters
behaviour_data[, (char_as_factor) := lapply(.SD, as.factor), .SDcols = char_as_factor] # columns as factors
# ======================================================================
# ======================================================================





# 2. Draw a random subsample of the table (for testing only)=============
# =======================================================================
# Filter out players who played less than 5 matches
# -------------------------------------------------
#behaviour_data[, n_matches := length(unique(match_id)), by = mirrors_id]

# How many players have less than 5 matches?
#length(unique(behaviour_data[n_matches < 5]$mirrors_id))

# Proportion of players having less than 5 matches on the total
#(length(unique(behaviour_data[n_matches < 5]$mirrors_id))) / (length(unique(behaviour_data$mirrors_id)))
# 41.7% of the entire player base



# Model should be runned on a small subsample for testing
# -------------------------------------------------------
#set.seed(20200519)
#sub_behaviour_data <- behaviour_data[mirrors_id %in% sample(unique(mirrors_id), 100)]



# Normalize sqrt variables (Z-scores)
# -------------------------------------------------------
standardize <- function (x) {(x - mean(x)) / sd(x)}

behaviour_data[, c("Zsqrtspeed", "Zsqrtspace_covered_rate", "Zsqrtprox_mid_guard",
                   "Zsqrtsurv_speed", "Zsqrtsurv_space_covered_rate") :=
                lapply(.SD, standardize), 
                .SDcols = c(48, 49, 47, 58, 59)]

# Remove matches where survivor speed seem like outliers (-9 stdev)
#behaviour_data_NOoutliers <- behaviour_data[Zsqrtsurv_speed > -8] 
# STANDARDIZE AGAIN AFTER
#behaviour_data_NOoutliers[, c("Zsqrtspeed", "Zsqrtspace_covered_rate", "Zsqrtprox_mid_guard",
#                              "Zsqrtsurv_speed", "Zsqrtsurv_space_covered_rate") :=
#                          lapply(.SD, standardize), 
#                          .SDcols = c(48, 49, 47, 58, 59)]


# Plot the data
# -------------------------------------------------------
# Standard plots
#with(sub_behaviour_data, plot(Zspeed))
with(behaviour_data, plot(Zspeed))
with(behaviour_data, plot(Zsurv_speed))

#with(sub_behaviour_data, plot(Zspace_covered_rate))
with(behaviour_data, plot(Zspace_covered_rate))
with(behaviour_data, plot(Zsurv_space_covered_rate))

#with(sub_behaviour_data, plot(Zprox_mid_guard))
with(behaviour_data, plot(Zprox_mid_guard))



# ZSqrt seems to be the best scaling for the model
ggpairs(behaviour_data[, c(77, 78, 79, # Zsqrt pred
                           80, 81 # Zsqrt prey
                           )
                      ]
       )

ggpairs(behaviour_data_NOoutliers[, c(78, 79, 80, # Zsqrt pred
                           81, 82 # Zsqrt prey
                           )
                      ]
       )
# ======================================================================
# ======================================================================





# 3. Specify the multivariate model ===================================== 
# =======================================================================
# ( nitt - burnin ) / thin = 1000
# nitt: number of iterations, it has to be nitt = (thin*10000) + burnin
# burnin: the number of iterations that are discarded at the beginning
# thin: every how many iterations we take one to store

# For testing, nitt = 53000, thin = 50, burnin = 3000
# For the final model, thinning = 100 with nitt = 203 000 with burning = 3000

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
                                 nu = 2.002)))
#

# First model
# ------------------------------------------------
system.time(
ModMV1 <- MCMCglmm(
                  cbind(Zsqrtspeed, Zsqrtspace_covered_rate, Zsqrtprox_mid_guard) ~ 
                  (trait - 1), # set a general intercept for response,
                  random = ~us(trait):mirrors_id + us(trait):map_name + us(trait):character_name, # random intercepts
                  rcov = ~us(trait):units, # residual matrix
                  prior = prior, nitt = 203000, thin = 100, burnin = 3000, 
                  verbose = TRUE, 
                  family = c("gaussian", "gaussian", "gaussian"), # distribution of responses
                  data = behaviour_data
                  )
           )

save(ModMV1, file = "05A_ModMV1.rda")
load("05A_ModMV1.rda")

#system.time(
#ModMV1NOoutliers <- MCMCglmm(
#                  cbind(Zsqrtspeed, Zsqrtspace_covered_rate, Zsqrtprox_mid_guard) ~ 
#                  (trait - 1),
#                  random = ~us(trait):mirrors_id + us(trait):map_name + us(trait):character_name,
#                  rcov = ~us(trait):units,
#                  prior = prior, nitt = 203000, thin = 100, burnin = 3000, 
#                  verbose = TRUE, 
#                  family = c("gaussian", "gaussian", "gaussian"),
#                  data = behaviour_data_NOoutliers
#                  )
#           )

#save(ModMV1NOoutliers, file = "05A_ModMV1NOoutliers.rda")



# Second model with prey effects
# ------------------------------------------------
system.time(
ModMV2 <- MCMCglmm(
                  cbind(Zsqrtspeed, Zsqrtspace_covered_rate, Zsqrtprox_mid_guard) ~ 
                  (trait - 1) + # set a general intercept for each predictor
                  trait:Zsqrtsurv_speed +
                  trait:Zsqrtsurv_space_covered_rate,
                  random = ~us(trait):mirrors_id + us(trait):map_name + us(trait):character_name, # random intercepts
                  rcov = ~us(trait):units, # residual matrix
                  prior = prior, nitt = 203000, thin = 100, burnin = 3000, 
                  verbose = TRUE, 
                  family = c("gaussian", "gaussian", "gaussian"), # distribution of responses
                  data = behaviour_data
                  )
           )

save(ModMV2, file = "05A_ModMV2.rda")
load("05A_ModMV2.rda")
# ======================================================================
# ======================================================================





# 4. Model diagnostics ================================================== 
# =======================================================================
# consider trying gelman.diag and gelman.plot for diagnostics too!


# First model
# ------------------------------------------------
# Visualize fixed effects and random effect variance components
plot(ModMV1$VCV)

# Check for autocorrelation
autocorr.diag(ModMV1$VCV)

# Check for effective sample size
# It should be equal to the number of times we sample (so 2000)
effectiveSize(ModMV1$VCV)

# Check linearity of residuals
corr <- as.data.table(posterior.cor(ModMV1$VCV[, 28:36]))
with(corr, plot(V2~V3))
with(corr, plot(V2~V6))
with(corr, plot(V3~V6))
# All good
# ----------------------

# Summary statistics
summary(ModMV1)

# Checking the variance covariance matrix.
# Values for the variances and covariances between and within IDs
posterior.mode(ModMV1$VCV)
HPDinterval(ModMV1$VCV)
# ------------------------------------------------



# Second model with prey effects
# ------------------------------------------------
# Visualize fixed effects and random effect variance components
plot(ModMV2$Sol)
plot(ModMV2$VCV)

# Check for autocorrelation
autocorr.diag(ModMV2$Sol)
autocorr.diag(ModMV2$VCV)

# Check for effective sample size
# It should be equal to the number of times we sample (so 2000)
effectiveSize(ModMV2$Sol)
effectiveSize(ModMV2$VCV) 
# ----------------------

# Summary statistics
summary(ModMV2$Sol)

# Extracting model parameters
beta <- posterior.mode(ModMV2$Sol) # Point estimate for fixed effects
CI <- HPDinterval(ModMV2$Sol) # 95% confidence intervals for fixed effects
# making a table to work with
beta_table <- as.data.table(cbind(beta, CI))
beta_table <- cbind(parameter_name = rownames(CI), beta_table)
beta_table


# Checking the variance covariance matrix.
# Values for the variances and covariances between and within IDs
posterior.mode(ModMV2$VCV)
HPDinterval(ModMV2$VCV)
# ======================================================================
# ======================================================================





# 5. Variance-covariance and correlation matrixes ======================= 
# =======================================================================
# Model 1
# ------------------------------------------------
# Correlations AMONG individuals (it's a 3x3 matrix so we take the first 9)
among_corr <- matrix(posterior.mode(posterior.cor(ModMV1$VCV[, 1:9])), ncol = 3, byrow = TRUE)
among_corrCI <- HPDinterval(posterior.cor(ModMV1$VCV[, 1:9]))

# Correlations AMONG environments
env_corr <- matrix(posterior.mode(posterior.cor(ModMV1$VCV[, 10:18])), ncol = 3, byrow = TRUE)
env_corrCI <- HPDinterval(posterior.cor(ModMV1$VCV[, 10:18]))
# No syndrome caused by different avatars
matrix(posterior.mode(posterior.cor(ModMV1$VCV[, 19:27])), ncol = 3, byrow = TRUE)

# Correlations WITHIN individuals (it's a 3x3 matrix so we take the last 9)
within_corr <- matrix(posterior.mode(posterior.cor(ModMV1$VCV[, 28:36])), ncol = 3, byrow = TRUE)
within_corrCI <- HPDinterval(posterior.cor(ModMV1$VCV[, 28:36]))


# make a list of the among and within correlations and save them
IDcorr_list <- list(among_corr = among_corr, among_corrCI = among_corrCI, 
                    within_corr = within_corr, within_corrCI = within_corrCI,
                    env_corr = env_corr, env_corrCI = env_corrCI)
save(IDcorr_list, file = "05A_IDcorr_list.rda")
load("05A_IDcorr_list.rda")
# ------------------------------------------------


# Model 2
# ------------------------------------------------
# Correlations AMONG individuals (it's 3 3x3 matrix so we take the first 9)
matrix(posterior.mode(posterior.cor(ModMV2$VCV[, 1:9])), ncol = 3, byrow = TRUE)
HPDinterval(posterior.cor(ModMV2$VCV[, 1:9]))

# Correlations WITHIN individuals (taking residuals so its the last 28:36)
matrix(posterior.mode(posterior.cor(ModMV2$VCV[, 28:36])), ncol = 3, byrow = TRUE)
HPDinterval(posterior.cor(ModMV2$VCV[, 28:36]))

# Results are mostly the same
# ======================================================================
# ======================================================================





# 6. Calculate repeatabilities (ICCs)  ================================== 
# =======================================================================
# First model
# ------------------------------------------------
# Speed player ID repeatability
rz1.1 <- ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] / 
         (ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
          ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
          ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] +
          ModMV1$VCV[,"traitZsqrtspeed:traitZsqrtspeed.units"])
CIrz1.1 <- HPDinterval(rz1.1)[, 1:2]
rz1.1 <- mean(rz1.1)

# map repeatability
map1.1 <- ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] / 
          (ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.units"])
CImap1.1 <- HPDinterval(map1.1)[, 1:2]
map1.1 <- mean(map1.1)

# character repeatability
char1.1 <- ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] / 
          (ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.units"])
CIchar1.1 <- HPDinterval(char1.1)[, 1:2]
char1.1 <- mean(char1.1)



# Space player ID repeatability
rz2.1 <- ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] / 
         (ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] + 
          ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
          ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] +
          ModMV1$VCV[,"traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CIrz2.1 <- HPDinterval(rz2.1)[, 1:2]
rz2.1 <- mean(rz2.1)

# map repeatability
map2.1 <- ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] / 
          (ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] +
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] +
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CImap2.1 <- HPDinterval(map2.1)[, 1:2]
map2.1 <- mean(map2.1)

# character repeatability
char2.1 <- ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] / 
          (ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] +
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] +
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CIchar2.1 <- HPDinterval(char2.1)[, 1:2]
char2.1 <- mean(char2.1)



# Guard player ID repeatability
rz3.1 <- ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] / 
         (ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] + 
          ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] + 
          ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] +
          ModMV1$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CIrz3.1 <- HPDinterval(rz3.1)[, 1:2]
rz3.1 <- mean(rz3.1)

# map repeatability
map3.1 <- ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] / 
          (ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] + 
           ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] + 
           ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] + 
           ModMV1$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CImap3.1 <- HPDinterval(map3.1)[, 1:2]
map3.1 <- mean(map3.1)

# character repeatability
char3.1 <- ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] / 
          (ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] + 
           ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] + 
           ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] + 
           ModMV1$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CIchar3.1 <- HPDinterval(char3.1)[, 1:2]
char3.1 <- mean(char3.1)
# ------------------------------------------------



# Second model with prey effects + environment ID and player avatar
# ------------------------------------------------
# Speed 
# repeatability
rz1.2 <- ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] / 
         (ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
          ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
          ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] + 
          ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.units"])
CIrz1.2 <- HPDinterval(rz1.2)[, 1:2]
rz1.2 <- mean(rz1.2)

# map repeatability
map1.2 <- ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] / 
          (ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.units"])
CImap1.2 <- HPDinterval(map1.2)[, 1:2]
map1.2 <- mean(map1.2)

# char repeatability
char1.2 <- ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] / 
          (ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.units"])
CIchar1.2 <- HPDinterval(char1.2)[, 1:2]
char1.2 <- mean(char1.2)



# Space 
# repeatability
rz2.2 <- ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] / 
         (ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] + 
          ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
          ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] + 
          ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CIrz2.2 <- HPDinterval(rz2.2)[, 1:2]
rz2.2 <- mean(rz2.2)

# map repeatability
map2.2 <- ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] / 
          (ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] +
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] +
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CImap2.2 <- HPDinterval(map2.2)[, 1:2]
map2.2 <- mean(map2.2)

# char repeatability
char2.2 <- ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] / 
          (ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] +
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] +
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CIchar2.2 <- HPDinterval(char2.2)[, 1:2]
char2.2 <- mean(char2.2)



# Guard 
# repeatability
rz3.2 <- ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] / 
         (ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] +
          ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] +
          ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] +
          ModMV2$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CIrz3.2 <- HPDinterval(rz3.2)[, 1:2]
rz3.2 <- mean(rz3.2)

# map repeatability
map3.2 <- ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] / 
          (ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] + 
           ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] + 
           ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] + 
           ModMV2$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CImap3.2 <- HPDinterval(map3.2)[, 1:2]
map3.2 <- mean(map3.2)

# character repeatability
char3.2 <- ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] / 
          (ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] + 
           ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] + 
           ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] + 
           ModMV2$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CIchar3.2 <- HPDinterval(char3.2)[, 1:2]
char3.2 <- mean(char3.2)



# Create a table and save it so I can check % variances
rpt <- as.data.table(rbind(rz1.1, rz2.1, rz3.1,
                           map1.1, map2.1, map3.1,
                           char1.1, char2.1, char3.1,
                           rz1.2, rz2.2, rz3.2,
                           map1.2, map2.2, map3.2,
                           char1.2, char2.2, char3.2)
                    )

CI <- as.data.table(rbind(CIrz1.1, CIrz2.1, CIrz3.1, 
                          CImap1.1, CImap2.1, CImap3.1,
                          CIchar1.1, CIchar2.1, CIchar3.1,
                          CIrz1.2, CIrz2.2, CIrz3.2,
                          CImap1.2, CImap2.2, CImap3.2,
                          CIchar1.2, CIchar2.2, CIchar3.2)
                   )

ranef_table <- cbind(parameter = c("CIrz1.1", "CIrz2.1", "CIrz3.1", 
                                   "CImap1.1", "CImap2.1", "CImap3.1",
                                   "CIchar1.1", "CIchar2.1", "CIchar3.1",
                                   "CIrz1.2", "CIrz2.2", "CIrz3.2",
                                   "CImap1.2", "CImap2.2", "CImap3.2",
                                   "CIchar1.2", "CIchar2.2", "CIchar3.2"), 
                     rpt = rpt, 
                     CI)

save(ranef_table, file = "05A_multivariate_ranef.rda")
load("05A_multivariate_ranef.rda")
# ======================================================================
# ======================================================================









# Compute 4 other models like the first one to check if convergence is OK
# ========================================================================
# ========================================================================
system.time(
ModMV1_2 <- MCMCglmm(
                  cbind(Zsqrtspeed, Zsqrtspace_covered_rate, Zsqrtprox_mid_guard) ~ 
                  (trait - 1), # set a general intercept for response,
                  random = ~us(trait):mirrors_id + us(trait):map_name + us(trait):character_name, # random intercepts
                  rcov = ~us(trait):units, # residual matrix
                  prior = prior, nitt = 203000, thin = 100, burnin = 3000, 
                  verbose = TRUE, 
                  family = c("gaussian", "gaussian", "gaussian"), # distribution of responses
                  data = behaviour_data
                  )
           )

save(ModMV1_2, file = "05A_ModMV1_2.rda")
load("05A_ModMV1_2.rda")

system.time(
ModMV1_3 <- MCMCglmm(
                  cbind(Zsqrtspeed, Zsqrtspace_covered_rate, Zsqrtprox_mid_guard) ~ 
                  (trait - 1), # set a general intercept for response,
                  random = ~us(trait):mirrors_id + us(trait):map_name + us(trait):character_name, # random intercepts
                  rcov = ~us(trait):units, # residual matrix
                  prior = prior, nitt = 203000, thin = 100, burnin = 3000, 
                  verbose = TRUE, 
                  family = c("gaussian", "gaussian", "gaussian"), # distribution of responses
                  data = behaviour_data
                  )
           )

save(ModMV1_3, file = "05A_ModMV1_3.rda")
load("05A_ModMV1_3.rda")


system.time(
ModMV1_4 <- MCMCglmm(
                  cbind(Zsqrtspeed, Zsqrtspace_covered_rate, Zsqrtprox_mid_guard) ~ 
                  (trait - 1), # set a general intercept for response,
                  random = ~us(trait):mirrors_id + us(trait):map_name + us(trait):character_name, # random intercepts
                  rcov = ~us(trait):units, # residual matrix
                  prior = prior, nitt = 203000, thin = 100, burnin = 3000, 
                  verbose = TRUE, 
                  family = c("gaussian", "gaussian", "gaussian"), # distribution of responses
                  data = behaviour_data, pr = TRUE
                  )
           )
save(ModMV1_4, file = "05A_ModMV1_4.rda")


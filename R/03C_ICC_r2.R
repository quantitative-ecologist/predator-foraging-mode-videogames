#########################################################################

#              Model fit and ICCs for the quadratic model               #

#########################################################################


# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec
# -----------------------------------------------------------------------





# =======================================================================
# 1. Set working directory, load libraries, datasets, and models
# =======================================================================

# Librairies
library(data.table)
library(brms)
library(broom.helpers)

# Load dataset
data <- fread("./data/02_merged-data.csv",
              select = c("mirrors_id", "match_id", 
                         "map_name", "hunting_success", "Zspeed", 
                         "Zprox_mid_guard", "Zspace_covered_rate",
                         "Zsurv_speed", "Zhook_start_time",
                         "Zsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Load model
load("03C_hunting_success_quadratic-model.rda")
print(object.size(quadratic_model), units = "MB")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Define Stan functions for diagnosis of the beta-binomial model
# =======================================================================

expose_functions(betabi_mod, vectorize = TRUE)

# define required log-lik.
log_lik_beta_binomial2 <- function(i, prep) {
  mu <- brms:::get_dpar(prep, "mu", i = i)
  phi <- brms:::get_dpar(prep, "phi", i = i)
  trials <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  beta_binomial2_lpmf(y, mu, phi, trials)
}

# Function for posterior predict
posterior_predict_beta_binomial2 <- function(i, prep, ...) {
  mu <- brms:::get_dpar(prep, "mu", i = i)
  phi <- brms:::get_dpar(prep, "phi", i = i)
  trials <- prep$data$vint1[i]
  beta_binomial2_rng(mu, phi, trials)
}

# 
posterior_epred_beta_binomial2 <- function(prep) {
  mu <- brms:::get_dpar(prep, "mu", i = i)
  trials <- prep$data$vint1
  trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  mu * trials
}

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Assess model fit (R-squared)
# =======================================================================
# Calculation by hand 
# Based on the supplementary material 2 from Nakagawa & al. 2017

# Compute the model matrixes
mm2 <- model_get_model_matrix(quadratic_model)

# Variance components :
# 1. Fixed effects variance
# --------------------------
# Compute variance in fitted values (Fixed effects variance)
VarF2 <- var(as.vector(mm2%*%fixef(quadratic_model)))

# 2. Distribution-specific variance
# --------------------------
# For binomial model with OLRE
#VarDS <- pi^2/3

# For beta binomial model
# (phi from the distribution)
VarDS2 <- summary(quadratic_model)$spec_pars[1]

# 3. Random effects variance
# --------------------------
VarR2 <- VarCorr(quadratic_model)$mirrors_id$sd[1]^2 + 
         VarCorr(quadratic_model)$map_name$sd[1]^2

#VarSE2 <- VarCorr(quadratic_model)$obs$sd[1]

# 4. Total variance
# --------------------------
# binomial model with OLRE
#VarT2 <- VarF2 + VarR2 + VarSE2 + VarDS

# beta-binomial model
VarT2 <- VarF2 + VarR2 + VarDS2

# 5. Compute R-squared values
# --------------------------
# Marginal R2
R2_M2 <- VarF2 /
         VarT2

# Conditional R2 (OLRE is excluded in the numerator to only account for random effects)
R2_C2 <- (VarF2 + VarR2) / # Fixed effect variance + random effect variance
          VarT2                   # Total variance


# Save r-squared values into a table
r_squared <- as.data.table(cbind(R2_M2, R2_C2))
capture.output(r_squared, file = "03C_r2-table.txt")
# =======================================================================
# =======================================================================





# =======================================================================
# 4. Compute ICCs and their 95% credibility intervals
# =======================================================================



# =======================================================================
# =======================================================================
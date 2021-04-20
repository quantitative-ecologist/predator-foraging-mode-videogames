############################################################################

#                       Base hunting success analysis                      #

############################################################################

# Code to run the linear hunting success analysis
# This script was run on Calcul Canada's supercomputer Beluga

# This model quantifies the relationship between hunting behaviour and hunting success. Hunting success = number of prey captured.

# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec

# Detect number of cores
options(mc.cores = parallel::detectCores())
# -----------------------------------------------------------------------





# =======================================================================
# 1. Set working directory, load libraries, and import dataset 
# =======================================================================

# Packages
library(data.table)
library(brms)
library(cmdstanr)

# Set the working directory on the servers
#setwd("/home/maxime11/projects/def-monti/maxime11/scripts")

# Load dataset
data <- fread("/home/maxime11/projects/def-monti/maxime11/data/02_merged-data.csv",
              select = c("mirrors_id", "match_id", 
                         "map_name", "hunting_success", "Zspeed", 
                         "Zprox_mid_guard", "Zspace_covered_rate",
                         "Zhook_start_time", "Zsurv_speed", 
                         "Zsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Add observation-level random effect
data$obs <- 1:nrow(data)
# =======================================================================
# =======================================================================





# =======================================================================
# 2. Parametrize the model
# =======================================================================

# Compute the custom family
beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int", vars = "vint1[n]"
)

# Function
stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"

# Variables
stanvars <- stanvar(scode = stan_funs, block = "functions")

# Set priors
priors <- set_prior("normal(0, 5)", class = "b")
# on the random intercepts?

# quadratic model formula
model_formula <- brmsformula(hunting_success | vint(4) ~
                                        # Quadratic terms
                                        I(Zspeed^2) +
                                        I(Zspace_covered_rate^2) +
                                        I(Zprox_mid_guard^2) +
                                        I(Zhook_start_time^2) +
                                        I(Zsurv_speed^2) +
                                        I(Zsurv_space_covered_rate^2) +
                                        # Linear terms
                                        Zspeed +
                                        Zspace_covered_rate +
                                        Zprox_mid_guard +
                                        Zhook_start_time +
                                        Zsurv_speed +
                                        Zsurv_space_covered_rate +
                                        # Predator trait covariances
                                        Zspeed : Zspace_covered_rate +
                                        Zspeed : Zprox_mid_guard +
                                        Zspeed : Zhook_start_time +
                                        Zspace_covered_rate : Zprox_mid_guard +
                                        Zspace_covered_rate : Zhook_start_time +
                                        Zprox_mid_guard : Zhook_start_time +
                                        # Predator-prey trait covariances
                                        Zspeed : Zsurv_speed +
                                        Zspeed : Zsurv_space_covered_rate +
                                        Zspace_covered_rate : Zsurv_speed +
                                        Zspace_covered_rate : Zsurv_space_covered_rate +
                                        Zprox_mid_guard : Zsurv_speed +
                                        Zprox_mid_guard : Zsurv_space_covered_rate +
                                        Zhook_start_time : Zsurv_speed +
                                        Zhook_start_time : Zsurv_space_covered_rate +
                                        (1 | map_name) +
                                        (1 | mirrors_id))

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Run model
# =======================================================================

system.time(quad_beta <- brm(formula = model_formula,
                              family = beta_binomial2,
                              warmup = 3000, 
                              iter = 153000,
                              thin = 100,
                              chains = 4, 
                              inits = "0", 
                              threads = threading(10),
                              backend = "cmdstanr",
                              seed = 20210419,
                              stanvars = stanvars,
                              prior = priors,
                              control = list(adapt_delta = 0.95),
                              data = data))

save(quad_beta, file = "quad_beta-model.rda")


# Save session info for reproducibility
session <- sessionInfo()
capture.output(session, file = "03B_sessioninfo-quadbeta.txt")

# =======================================================================
# =======================================================================
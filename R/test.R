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

# linear model formula
model_formula <- brmsformula(hunting_success | vint(4) ~
                                        Zspeed +
                                        Zspace_covered_rate +
                                        Zprox_mid_guard +
                                        Zhook_start_time +
                                        Zsurv_speed +
                                        Zsurv_space_covered_rate +
                                        (1 | map_name) +
                                        (1 | mirrors_id))

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Run model
# =======================================================================

system.time(beta_binom_mod <- brm(formula = model_formula,
                              family = beta_binomial2,
                              warmup = 3000, 
                              iter = 153000,
                              thin = 100,
                              chains = 4, 
                              inits = "0", 
                              threads = threading(10),
                              backend = "cmdstanr",
                              seed = 20210414,
                              stanvars = stanvars,
                              prior = priors,
                              control = list(adapt_delta = 0.95),
                              data = data))

save(beta_binom_mod, file = "beta_binom_mod.rda")


# Save session info for reproducibility
#session <- sessionInfo()
#capture.output(session, file = "03B_sessioninfo.txt")

# =======================================================================
# =======================================================================
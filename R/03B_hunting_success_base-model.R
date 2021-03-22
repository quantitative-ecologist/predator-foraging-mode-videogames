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
                         "Zsurv_speed", "Zsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Add observation-level random effect
data$obs <- 1:nrow(data)

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Parametrize the model
# =======================================================================

# Set priors
priors <- set_prior("normal(0, 5)", class = "b")
# on the random intercepts?

# linear model formula
model_formula <- brmsformula(hunting_success | trials(4) ~
                                        Zspeed +
                                        Zspace_covered_rate +
                                        Zprox_mid_guard +
                                        Zsurv_speed +
                                        Zsurv_space_covered_rate +
                                        (1 | map_name) +
                                        (1 | mirrors_id) +
                                        (1 | obs))

# Create the stan code (RUN ONCE):
#make_stancode(formula = model_formula, 
#              family = binomial(link = "logit"),
#              data = data,
#              prior = priors,
#              )

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Run model
# =======================================================================

# Base model brms
system.time(base_model <- brm(formula = model_formula,
                              family = binomial(link = "logit"),
                              warmup = 3000, 
                              iter = 203000,
                              thin = 100,
                              chains = 4, 
                              inits = "0", 
                              threads = threading(20),
                              backend = "cmdstanr",
                              seed = 20210318,
                              prior = priors,
                              control = list(adapt_delta = 0.95),
                              data = data))

save(base_model, file = "base_model.rda")

# =======================================================================
# =======================================================================

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

# Small subset of the data for testing the models
data_sub <- data[mirrors_id %in% c("JATHS5909D", "OZDOD9085O", "ZETSA0228O", "YZGIN4008I", "IMVTR2511Q"),]
data_sub$obs <- 1:nrow(data_sub)

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
# =======================================================================
# =======================================================================





# =======================================================================
# 3. Run model
# =======================================================================

# Base model brms
system.time(base_model <- brm(formula = model_formula,
                              family = binomial(link = "logit"),
                              warmup = 3000, 
                              iter = 53000,
                              thin = 50,
                              chains = 4, 
                              inits = "0", 
                              cores = 20,
                              seed = 20210310,
                              prior = priors,
                              control = list(adapt_delta = 0.95),
                              data = data_sub))

save(base_model, file = "base_model_nothreads.rda")
# =======================================================================
# =======================================================================
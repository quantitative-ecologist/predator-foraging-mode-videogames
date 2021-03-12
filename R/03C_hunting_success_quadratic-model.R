############################################################################

#                  Quadratic hunting success analysis                      #

############################################################################

# Code to run the quadratic hunting success analysis
# This script was run on Calcul Canada's supercomputer Beluga

# This model quantifies the quadratic relationship between hunting behaviour and hunting success. It also evaluates the correlated effect of predator and prey traits on hunting success. Hunting success = number of prey captured.

# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec

# Detect number of cores?
# options(mc.cores = parallel::detectCores())
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

# Add total number of prey
data[, total_prey := 4]

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

# quadratic model formula
model_formula <- brmsformula(hunting_success | trials(total_prey) ~
                                        # Quadratic terms
                                        I(Zspeed^2) +
                                        I(Zspace_covered_rate^2) +
                                        I(Zprox_mid_guard^2) +
                                        I(Zsurv_speed^2) +
                                        I(Zsurv_space_covered_rate^2) +
                                        # Linear terms
                                        Zspeed +
                                        Zspace_covered_rate +
                                        Zprox_mid_guard +
                                        Zsurv_speed +
                                        Zsurv_space_covered_rate +
                                        # Predator trait covariances
                                        Zspeed : Zspace_covered_rate +
                                        Zspeed : Zprox_mid_guard +
                                        Zspace_covered_rate : Zprox_mid_guard +
                                        # Predator-prey trait covariances
                                        Zspeed : Zsurv_speed +
                                        Zspeed : Zsurv_space_covered_rate +
                                        Zspace_covered_rate : Zsurv_speed +
                                        Zspace_covered_rate : Zsurv_space_covered_rate +
                                        Zprox_mid_guard : Zsurv_speed +
                                        Zprox_mid_guard : Zsurv_space_covered_rate +
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
# -----------------------------------------------------------------------
system.time(quadratic_model <- brm(formula = model_formula,
                                   family = binomial(link = "logit"),
                                   warmup = 3000, 
                                   iter = 203000,
                                   thin = 100,
                                   chains = 4, 
                                   inits = "0", 
                                   cores = 30,
                                   seed = 20210312,
                                   prior = priors,
                                   control = list(adapt_delta = 0.95),
                                   data = data_sub))

save(quadratic_model, file = "quadratic_model.rda")
# -----------------------------------------------------------------------


# Base model with STAN
# -----------------------------------------------------------------------
#quadratic_model_stan <- stan(file = "03C_hunting_success_quadratic-model.stan", 
#                             data = data, 
#                             iter = 203000,
#                             warmup = 3000, 
#                             thin = 100,
#                             chains = 4,
#                             cores = 30,
#                             init = 0, # or "random"?
#                             seed = 20210312, # date the model was ran 
#                             algorithm = "NUTS",
#                             verbose = TRUE,
#                             control = list(adapt_delta = 0.95) # smaller steps
#                             )
#
#save(quadratic_model_stan, file = "quadratic_model_stan.rda")
# -----------------------------------------------------------------------
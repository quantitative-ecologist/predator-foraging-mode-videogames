############################################################################

#                         Hunting success analyses                         #

############################################################################

# Run both hunting success models (linear + quadratic)


# Detect number of cores?
# options(mc.cores = parallel::detectCores())
# -----------------------------------------------------------------------




# =======================================================================
# 1. Set working directory, load libraries, and import dataset 
# =======================================================================

# Packages
library(data.table)
library(rstan)
library(brms)

# Load dataset
data <- fread("C:/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/PhD_project/project_data/02_merged-data.csv",
              select = c("mirrors_id", "match_id", 
                         "map_name", "hunting_success", "Zspeed", 
                         "Zprox_mid_guard", "Zspace_covered_rate",
                         "Zsurv_speed", "Zsurv_space_covered_rate"))

# Add total number of prey
data[, total_prey := 4]

# Add observation-level random effect
data$obs <- 1:nrow(data)

# Small subset of the data for testing the models
data_sub <- data[mirrors_id %in% c("JATHS5909D", "OZDOD9085O", "ZETSA0228O"),]
data_sub$obs <- 1:nrow(data_sub)

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Parametrize models 
# =======================================================================

# Set priors
priors <- set_prior("normal(0, 5)", class = "b")
# on the random intercepts?

# linear model formula
model_formula1 <- brmsformula(hunting_success | trials(total_prey) ~
                                        Zspeed +
                                        Zspace_covered_rate +
                                        Zprox_mid_guard +
                                        Zsurv_speed +
                                        Zsurv_space_covered_rate +
                                        (1 | map_name) +
                                        (1 | mirrors_id) +
                                        (1 | obs))

# quadratic model formula
model_formula2 <- brmsformula(hunting_success | trials(total_prey) ~
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

# Create the stan code :
# a. linear model
make_stancode(formula = model_formula1, 
              family = binomial(link = "logit"),
              data = data,
              prior = priors,
              )

# a. quadratic model
make_stancode(formula = model_formula2, 
              family = binomial(link = "logit"),
              data = data,
              prior = priors,
              )

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Run models 
# =======================================================================

# Quadratic model brms
# -----------------------------------------------------------------------
system.time(quadratic_model <- brm(formula = model_formula2,
                                    family = binomial(link = "logit"),
                                    warmup = 3000, 
                                    iter = 53000,
                                    thin = 50,
                                    chains = 2, 
                                    inits = "0", 
                                    cores = 2,
                                    seed = 123,
                                    prior = priors,
                                    control = list(adapt_delta = 0.95),
                                    data = data_sub))
save(quadratic_model, "quadratic_model_test.rda")

# -----------------------------------------------------------------------


# Base model with STAN
# -----------------------------------------------------------------------
base_model_stan <- stan(file = "base_model.stan", 
                             data = data, 
                             iter = 53000,
                             warmup = 3000, 
                             thin = 50,
                             chains = 4,
                             cores = 30),
                             init = 0, # or "random"?
                             seed = 20210310, # date the model was ran 
                             algorithm = "NUTS",
                             verbose = TRUE,
                             control = list(adapt_delta = 0.95) # smaller steps
                             )

save(base_model_stan, "base_model_stanCC.rda")
# -----------------------------------------------------------------------


# Quadratic model with STAN
# -----------------------------------------------------------------------
base_model_stan <- stan(file = "base_model.stan", 
                             data = data, 
                             iter = 53000,
                             warmup = 3000, 
                             thin = 50,
                             chains = 4,
                             cores = 30),
                             init = 0, # or "random"?
                             seed = 20210310, # date the model was ran 
                             algorithm = "NUTS",
                             verbose = TRUE,
                             control = list(adapt_delta = 0.95) # smaller steps
                             )
save(quadratic_model_stan, "quadratic_model_stanCC.rda")
# -----------------------------------------------------------------------

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

# to use an interactive node to test my model
salloc --time=00:30:00 --mem=5000M --account=def-monti --ntasks=4


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

# Load dataset
data <- fread("./data/02_merged-data.csv",
              select = c("mirrors_id", "match_id", "character_name",
                         "map_name", "hunting_success", "Zspeed", 
                         "Zprox_mid_guard", "Zspace_covered_rate",
                         "Zhook_start_time", "Zsurv_speed", 
                         "Zsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Add observation-level random effect
data$obs <- 1:nrow(data)


# Small subset of the data for testing the models
data_sub <- data[mirrors_id %in% c("GGMRS1168C", "GDJRB7034K", "GAGKF3834V", 
                                   "ZLGGM6266Z", "ZPAOC9787K"),]
data_sub$obs <- 1:nrow(data_sub)



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
                              warmup = 1000, 
                              iter = 3000,
                              thin = 10,
                              chains = 2, 
                              inits = "0", 
                              threads = threading(2),
                              backend = "cmdstanr",
                              seed = 20210414,
                              stanvars = stanvars,
                              prior = priors,
                              control = list(adapt_delta = 0.95),
                              data = data_sub))

save(beta_binom_mod, file = "beta_binom_mod.rda")


# Save session info for reproducibility
#session <- sessionInfo()
#capture.output(session, file = "03B_sessioninfo.txt")

# =======================================================================
# =======================================================================





# =======================================================================
# =======================================================================

# -----------------------------------------------------------
# linear model formula
speed_formula <- bf(Zspeed ~
                      Zsurv_speed +
                      Zsurv_space_covered_rate +
                      (1 |a| map_name) +
                      (1 |b| character_name) +
                      (1 |c| mirrors_id))

space_formula <- bf(Zspace_covered_rate ~
                      Zsurv_speed +
                      Zsurv_space_covered_rate +
                      (1 |a| map_name) +
                      (1 |b| character_name) +
                      (1 |c| mirrors_id))

guard_formula <- bf(Zprox_mid_guard ~
                      Zsurv_speed +
                      Zsurv_space_covered_rate +
                      (1 |a| map_name) +
                      (1 |b| character_name) +
                      (1 |c| mirrors_id))

hook_formula <- bf(Zhook_start_time ~
                      Zsurv_speed +
                      Zsurv_space_covered_rate +
                      (1 |a| map_name) +
                      (1 |b| character_name) +
                      (1 |c| mirrors_id))

# priors
priors <- c(
  set_prior("normal(0, 5)", 
            class = "b", 
            coef = "Zsurv_speed",
            resp = c("Zspeed", "Zspace_covered_rate", 
                     "Zprox_mid_guard", "Zhook_start_time")),
  set_prior("normal(0, 5)", 
            class = "b", 
            coef = "Zsurv_space_covered_rate",
            resp = c("Zspeed", "Zspace_covered_rate", 
                     "Zprox_mid_guard", "Zhook_start_time")),
  set_prior("lkj(2)", class = "cor"))

# Base model brms
system.time(mv_model <- brm(speed_formula + 
                              space_formula + 
                              guard_formula + 
                              hook_formula +
                              set_rescor(TRUE),
                          #  family = gaussian(), #this model was run without a specified family
                            warmup = 1000, 
                            iter = 4000,
                            thin = 5,
                            chains = 4, 
                            inits = "0",
                            #  threads = threading(10),
                            #  backend = "cmdstanr",
                            seed = 20210414,
                            #prior = priors, #this model was run without priors
                            control = list(adapt_delta = 0.95),
                            data = data_sub))
#save(mv_model, file = "mv_model_test.rda")
load("mv_model_test.rda")
mv_model <- add_criterion(mv_model, "loo")
# -----------------------------------------------------------



# -----------------------------------------------------------
# linear model formula
speed_form3 <- bf(Zspeed ~
                      Zsurv_speed +
                      Zsurv_space_covered_rate +
                      (1 |a| map_name) +
                      (1 |b| character_name) +
                      (1 |c| mirrors_id)) +
                      gaussian()

space_form3 <- bf(Zspace_covered_rate ~
                      Zsurv_speed +
                      Zsurv_space_covered_rate +
                      (1 |a| map_name) +
                      (1 |b| character_name) +
                      (1 |c| mirrors_id)) +
                      gaussian()

guard_form3 <- bf(Zprox_mid_guard ~
                      Zsurv_speed +
                      Zsurv_space_covered_rate +
                      (1 |a| map_name) +
                      (1 |b| character_name) +
                      (1 |c| mirrors_id)) +
                      gaussian()

hook_form3 <- bf(Zhook_start_time ~
                      Zsurv_speed +
                      Zsurv_space_covered_rate +
                      (1 |a| map_name) +
                      (1 |b| character_name) +
                      (1 |c| mirrors_id)) +
                      gaussian()
# -----------------------------------------------------------




# WORKS!!!!!
# -----------------------------------------------------------
# Formula
mv_form3 <- bf(mvbind(Zspeed,
                     Zspace_covered_rate,
                     Zprox_mid_guard,
                     Zhook_start_time) ~
                Zsurv_speed +
                Zsurv_space_covered_rate +
                (1 |a| map_name) +
                (1 |b| character_name) +
                (1 |c| mirrors_id)) + gaussian()

# priors3
priors3.1 <- c(
  set_prior("normal(0, 5)", 
            class = "b",
            coef = "Zsurv_speed",
            resp = c("Zspeed", "Zspacecoveredrate", 
                     "Zproxmidguard", "Zhookstarttime")),
  set_prior("normal(0, 5)", 
            class = "b",
            coef = "Zsurv_space_covered_rate",
            resp = c("Zspeed", "Zspacecoveredrate", 
                     "Zproxmidguard", "Zhookstarttime")),
  set_prior("lkj(2)", 
            class = "cor",
            group = "character_name"),
  set_prior("lkj(2)", 
            class = "cor",
            group = "map_name"),
  set_prior("lkj(2)", 
            class = "cor",
            group = "mirrors_id"))

# Base model brms
system.time(mv_model3 <- brm(mv_form3 +
                             set_rescor(TRUE),
                             warmup = 1000, 
                             iter = 3000,
                             thin = 5,
                             chains = 4, 
                             inits = "0",
                             #  threads = threading(10),
                             #  backend = "cmdstanr",
                             seed = 20210414,
                             prior = priors3.1,
                             control = list(adapt_delta = 0.95),
                             data = data_sub))

# Base model brms
system.time(mv_model3.1 <- brm(speed_form3 +
                               space_form3 +
                               guard_form3 +   
                               hook_form3  +
                               set_rescor(TRUE),
                               warmup = 1000, 
                               iter = 3000,
                               thin = 5,
                               chains = 4, 
                               inits = "0",
                               #  threads = threading(10),
                               #  backend = "cmdstanr",
                               seed = 20210414,
                               prior = priors3.1,
                               control = list(adapt_delta = 0.95),
                               data = data_sub))


trace <- plot(mv_model3.1)

pp_check(mv_model3.1, resp = "Zspeed")
pp_check(mv_model3.1, resp = "Zspacecoveredrate")
pp_check(mv_model3.1, resp = "Zproxmidguard")
pp_check(mv_model3.1, resp = "Zhookstarttime")

pp_check(mv_model3.1, 
         resp = "Zspeed",
         nsamples = 1e3, 
         type = "stat_2d") + 
  theme_bw(base_size = 20)
# -----------------------------------------------------------

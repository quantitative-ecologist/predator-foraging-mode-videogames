##########################################################################

#               Multivariate model of predator behaviour                 #

##########################################################################

# Code for a multivariate mixed-models to :
# 1. partition the variance in predator behaviour
# 2. quantify behavioural correlations among predator behaviours
# -----------------------------------------------------------------------





# =======================================================================
# 1. Load libraries, and import dataset
# =======================================================================

# Detect number of cores
options(mc.cores = parallel::detectCores())

# Load libraries
library(data.table)
library(brms)
library(parallel)

# import dataset
data <- fread("/home/maxime11/projects/def-monti/maxime11/data/merged-data.csv",
              select = c("mirrors_id", "match_id", "character_name",
                         "map_name", "hunting_success", "sqrtspeed", 
                         "sqrtspace_covered_rate", "sqrtprox_mid_guard",
                         "sqrthook_start_time"),
              stringsAsFactors = TRUE)

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare variables for the model
# =======================================================================

# Normalize sqrt variables (Z-scores)
standardize <- function (x) {(x - mean(x)) / sd(x)}

data[, c("Zsqrtspeed", "Zsqrtspace_covered_rate", "Zsqrtprox_mid_guard",
         "Zsqrthook_start_time") :=
       lapply(.SD, standardize), 
     .SDcols = c(6:9)]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Build the multivariate model 
# =======================================================================

# Formula for each response variable
# Each model will fit a seperate var-cov matrix for each random effect
speed_form <- bf(Zsqrtspeed ~ 1 +
                   (1 |a| map_name) +
                   (1 |b| character_name) +
                   (1 |c| mirrors_id)) +
  gaussian()

space_form <- bf(Zsqrtspace_covered_rate ~ 1 +
                   (1 |a| map_name) +
                   (1 |b| character_name) +
                   (1 |c| mirrors_id)) +
  gaussian()

guard_form <- bf(Zsqrtprox_mid_guard ~ 1 +
                   (1 |a| map_name) +
                   (1 |b| character_name) +
                   (1 |c| mirrors_id)) +
  gaussian()

hook_form <- bf(Zsqrthook_start_time ~ 1 +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| mirrors_id)) +
  gaussian()

# priors
priors <- c(
  set_prior("lkj(2)", 
            class = "cor",
            group = "character_name"),
  set_prior("lkj(2)", 
            class = "cor",
            group = "map_name"),
  set_prior("lkj(2)", 
            class = "cor",
            group = "mirrors_id"))

# ======================================================================
# ======================================================================





# =======================================================================
# 4. Run the multivariate model
# =======================================================================
#( nitt - burnin ) / thin = 1000
mv_model <- brm(speed_form +
                  space_form +
                  guard_form +   
                  hook_form +
                  set_rescor(TRUE),
                warmup = 3000, 
                iter = 43000,
                thin = 40,
                chains = 4, 
                inits = "0",
                threads = threading(10),
                backend = "cmdstanr",
                seed = 20210608,
                prior = priors,
                control = list(adapt_delta = 0.95),
                data = data)

save(mv_model, file = "03A_multivariate-model1.rda")
# ======================================================================
# ======================================================================
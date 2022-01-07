# ========================================================================

#               Multivariate model of predator behaviour                 #

# ========================================================================

# Code for a multivariate mixed-models to :
# 1. partition the variance in predator behaviour
# 2. quantify behavioural correlations among predator behaviours
# 3. Test if experience changes the results
# -----------------------------------------------------------------------





# =======================================================================
# 1. Load libraries, and import dataset
# =======================================================================


# Detect number of cores ------------------------------------------------

options(mc.cores = parallel::detectCores())



# Load libraries --------------------------------------------------------

library(data.table)
library(brms)
library(parallel)



# import dataset --------------------------------------------------------

# Folder path Compute Canada
folder <- file.path("/home", "maxime11", "projects", "def-monti", 
                    "maxime11", "phd_project", "data")

data <- fread(file.path(folder, "merged-data2021.csv"),
              select = c("player_id", "cumul_xp_total",
                         "total_xp",
                         "match_id", "character_name",
                         "map_name", "game_duration", 
                         "speed", "space_covered_rate",
                         "prox_mid_PreyGuarding",
                         "hook_start_time"),
                         stringsAsFactors = TRUE)

# When working locally
#data <- fread("./data/merged-data2021.csv",
#              select = c("player_id", "cumul_xp_total",
#                         "total_xp",
#                         "match_id", "character_name",
#                         "map_name", "game_duration",
#                         "speed", "space_covered_rate",
#                         "prox_mid_PreyGuarding",
#                         "hook_start_time"),
#                         stringsAsFactors = TRUE)

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Seperate the data by experience
# =======================================================================


# Check the quantiles of experience -------------------------------------

players <- unique(data[,.(player_id, total_xp)])

quantile(players[, total_xp])
#  0%  25%  50%  75% 100% 
#   1    2    8   31 1059

length(players[total_xp <= 31, player_id])
# 1788

length(players[total_xp > 31, player_id])
# 590

# Proportion on player base
nrow(players[total_xp <= 31]) / nrow(players) # 75%
nrow(players[total_xp > 31]) / nrow(players) # 25%



# Check proportion on total dataset -------------------------------------

nrow(data[cumul_xp_total <= 31]) / nrow(data)
# 0.3842849

nrow(data[cumul_xp_total > 31]) / nrow(data)
# 0.6157151



# Seperate the data -----------------------------------------------------

data_experienced <- data[cumul_xp_total > 31]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Prepare variables for the model
# =======================================================================


# Transform -------------------------------------------------------------

data_experienced[, ":=" (prox_mid_PreyGuarding = log(prox_mid_PreyGuarding + 1),
                         hook_start_time = log(hook_start_time + 1),
                         game_duration = sqrt(game_duration))]



# Standardise the variables (Z-scores) ----------------------------------

standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / 
                              sd(x, na.rm = TRUE)}

data[, c("Zgame_duration", "Zspeed",
         "Zspace_covered_rate", "Zprox_mid_PreyGuarding",
         "Zhook_start_time", "Zprey_avg_speed", 
         "Zprey_avg_space_covered_rate") :=
                lapply(.SD, standardize), 
                .SDcols = c(7:13)]

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Build the multivariate model 
# =======================================================================


# Formula for each response variable ------------------------------------

# Each model will fit a seperate var-cov matrix for each random effect
speed_form <- bf(Zspeed ~
                  Zprey_avg_speed +
                  Zprey_avg_space_covered_rate +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| player_id)) +
                  gaussian()

space_form <- bf(Zspace_covered_rate ~
                  Zprey_avg_speed +
                  Zprey_avg_space_covered_rate +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| player_id)) +
                  gaussian()

guard_form <- bf(Zprox_mid_PreyGuarding ~
                  Zprey_avg_speed +
                  Zprey_avg_space_covered_rate +
                  Zgame_duration +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| player_id)) +
                  gaussian()

hook_form <- bf(Zhook_start_time ~
                  Zprey_avg_speed +
                  Zprey_avg_space_covered_rate +
                  Zgame_duration +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| player_id)) +
                  gaussian()



# priors ----------------------------------------------------------------

priors <- c(
  # priors on fixed effects
  set_prior("normal(0, 2)",
            class = "b",
            coef = c("Zprey_avg_speed", 
                     "prey_avg_space_covered_rate"),
            resp = c("Zspeed", "Zspacecoveredrate",
                     "ZproxmidPreyGuarding", "Zhookstarttime")),
  set_prior("normal(0, 2)",
            class = "b",
            coef = "Zgame_duration",
            resp = c("ZproxmidPreyGuarding", "Zhookstarttime")),
  # priors on var. parameters (brms automatically detects half-normal)
  set_prior("normal(0, 1)",
            class = "sd", # applies to all variance parameters
            resp = c("Zspeed", "Zspacecoveredrate", 
                     "ZproxmidPreyGuarding", "Zhookstarttime")),
  # priors on the variance-covariance matrices
  set_prior("lkj(2)",
            class = "cor",
            group = "character_name"),
  set_prior("lkj(2)", 
            class = "cor",
            group = "map_name"),
  set_prior("lkj(2)", 
            class = "cor",
            group = "player_id"))

# ======================================================================
# ======================================================================





# =======================================================================
# 5. Run the multivariate model
# =======================================================================

#( nitt - burnin ) / thin = 1000

# Run the model on experienced players ----------------------------------

mv_model_experienced <- brm(speed_form +
                            space_form +
                            guard_form +   
                            hook_form +
                            set_rescor(TRUE),
                            warmup = 500, 
                            iter = 2500,
                            thin = 8,
                            chains = 4, 
                            inits = "0",
                            threads = threading(10),
                            backend = "cmdstanr",
                            seed = 123,
                            prior = priors,
                            control = list(adapt_delta = 0.95),
                          #  save_pars = save_pars(all = TRUE),
                            sample_prior = TRUE,
                            data = data_experienced)



# Save the model objects -----------------------------------------------

saveRDS(mv_model_experienced,
        file = "03A_multivariate-model-experienced.rds")

# ======================================================================
# ======================================================================
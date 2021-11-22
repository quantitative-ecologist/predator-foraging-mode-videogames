# ========================================================================

#               Multivariate model of predator behaviour                 #

# ========================================================================

# Code for a multivariate mixed-models to :
# 1. partition the variance in predator behaviour
# 2. quantify behavioural correlations among predator behaviours
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



# Import dataset --------------------------------------------------------

# Folder path Compute Canada
folder <- file.path("home", "maxime11", "projects", "def-monti", 
                    "maxime11", "phd_project", "data", "/")

# Import the data
data <- fread(file.path(folder, "merged-data2021.csv"),
              select = c("player_id", "match_id",
                         "character_name", "map_name",
                         "game_duration", "speed",
                         "space_covered_rate",
                         "prox_mid_PreyGuarding",
                         "hook_start_time"),
                         stringsAsFactors = TRUE)

# When working locally
#data <- fread("./data/merged-data2021.csv",
#              select = c("player_id", "match_id",
#                         "character_name", "map_name",
#                         "game_duration", "speed",
#                         "space_covered_rate",
#                         "prox_mid_PreyGuarding",
#                         "hook_start_time"),
#                         stringsAsFactors = TRUE)

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare variables for the model
# =======================================================================


# Transform --------------------------------------------------------------

# Transform the data even though it is not perfect
data[, ":=" (prox_mid_PreyGuarding = log(prox_mid_PreyGuarding + 1),
             hook_start_time = log(hook_start_time + 1),
             game_duration = sqrt(game_duration))]



# Standardise the variables (Z-scores) ----------------------------------

standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / 
                              sd(x, na.rm = TRUE)}

data[, c("Zgame_duration", "Zspeed",
         "Zspace_covered_rate", "Zprox_mid_PreyGuarding",
         "Zhook_start_time") :=
                lapply(.SD, standardize), 
                .SDcols = c(5:9)]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Build the multivariate model 
# =======================================================================


# Formula for each response variable ------------------------------------

# Each model will fit a seperate var-cov matrix for each random effect
speed_form <- bf(Zspeed ~
                  1 +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| player_id)) +
                  gaussian()

space_form <- bf(Zspace_covered_rate ~
                  1 +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| player_id)) +
                  gaussian()

guard_form <- bf(Zprox_mid_PreyGuarding ~
                  Zgame_duration +
                  (1 |a| map_name) +
                  (1 |b| character_name) +
                  (1 |c| player_id)) +
                  gaussian()

hook_form <- bf(Zhook_start_time ~
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
            group = "player_id")
            )

# ======================================================================
# ======================================================================





# =======================================================================
# 4. Run the multivariate model
# =======================================================================


# Model specifications --------------------------------------------------

#( nitt - burnin ) / thin = 1000
mv_model <- brm(speed_form +
                space_form +
                guard_form +   
                hook_form +
                set_rescor(TRUE),
                warmup = 3000, 
                iter = 13000,
                thin = 10,
                chains = 4, 
                inits = "0",
                threads = threading(10),
                backend = "cmdstanr",
                seed = 123,
                prior = priors,
                control = list(adapt_delta = 0.95),
                save_pars = save_pars(all = TRUE),
                sample_prior = TRUE,
                data = data)



# Save the model object ------------------------------------------------

saveRDS(mv_model, file = "03A_multivariate-model1.rds")



# Capture the session --------------------------------------------------

session <- sessionInfo()
capture.output(session, file = "session-mvmodels.txt")

# ======================================================================
# ======================================================================
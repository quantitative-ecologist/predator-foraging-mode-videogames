# ==========================================================================

#                       Base hunting success analysis                      #

# ==========================================================================

# Code to run the linear hunting success analysis
# This script was run on Calcul Canada's Cedar supercomputer

# This model quantifies the relationship between hunting behaviour and hunting success. 
# Hunting success = number of prey captured.

# Detect number of cores
options(mc.cores = parallel::detectCores())
# -----------------------------------------------------------------------





# ==========================================================================
# 1. Load libraries and import the data 
# ==========================================================================


# Packages -----------------------------------------------------------------

library(data.table)
library(brms)
#library(cmdstanr)



# Import the data ----------------------------------------------------------

# Folder path Compute Canada
folder <- file.path("/home", "maxime11", "projects", "def-monti", 
                    "maxime11", "phd_project", "data")

# Import the data
data <- fread(file.path(folder, "FraserFrancoetal2022-data.csv"),
              select = c("player_id", "match_id",
                         "hunting_success", "environment_id", 
                         "game_duration", "speed",
                         "space_covered_rate",
                         "prox_mid_PreyGuarding",
                         "hook_start_time"),
                         stringsAsFactors = TRUE)

# Add observation-level random effect
data$obs <- 1:nrow(data)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Prepare variables for the model
# ==========================================================================


# Transform ----------------------------------------------------------------

data[, ":=" (prox_mid_PreyGuarding = log(prox_mid_PreyGuarding + 1),
             hook_start_time = log(hook_start_time + 1),
             game_duration = sqrt(game_duration))]



# Standardise the variables (Z-scores) -------------------------------------

standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / 
                              sd(x, na.rm = TRUE)}

data[, c("Zgame_duration", "Zspeed",
         "Zspace_covered_rate", "Zprox_mid_PreyGuarding",
         "Zhook_start_time") :=
                lapply(.SD, standardize), 
                .SDcols = c(5:9)]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. Build the model
# ==========================================================================


# Set priors ---------------------------------------------------------------

priors <- c(
  # priors on fixed effects
  set_prior("normal(0, 2)",
            class = "b"),
  # priors on var. parameters (brms automatically detects half-normal)
  set_prior("normal(0, 1)",
            class = "sd") # applies to all variance parameters
            )



# linear model formula -----------------------------------------------------

model_formula <- brmsformula(hunting_success | trials(4) ~
                                        Zspeed +
                                        Zspace_covered_rate +
                                        Zprox_mid_PreyGuarding +
                                        Zhook_start_time +
                                        Zgame_duration +
                                        (1 | map_name) +
                                        (1 | player_id) +
                                        (1 | obs))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Run the model
# ==========================================================================


# Model specifications -----------------------------------------------------

base_model <- brm(formula = model_formula,
                  family = binomial(link = "logit"),
                  warmup = 500, 
                  iter = 2500,
                  thin = 8,
                  chains = 4, 
                  inits = "0", 
                  #threads = threading(10),
                  #backend = "cmdstanr",
                  seed = 123,
                  prior = priors,
                  control = list(adapt_delta = 0.95),
                  #save_pars = save_pars(all = TRUE),
                  sample_prior = TRUE,
                  data = data)



# Save the model object ----------------------------------------------------

saveRDS(base_model, file = "03B_hunting_success_base-model1.rds")



# Capture the session ------------------------------------------------------

session <- sessionInfo()
capture.output(session, file = "session-hunting_success-models.txt")

# ==========================================================================
# ==========================================================================
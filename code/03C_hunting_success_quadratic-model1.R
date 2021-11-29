# ==========================================================================

#               Predator quadratic hunting success analysis                #

# ==========================================================================

# Code to run the quadratic hunting success analysis
# This script was run on Calcul Canada's Cedar supercomputer

# This model quantifies the quadratic relationship between hunting behaviour 
# and hunting success. It also evaluates the correlated effect of predator 
# traits on hunting success. 

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
library(cmdstanr)



# Import the data ----------------------------------------------------------

# Folder path Compute Canada
folder <- file.path("/home", "maxime11", "projects", "def-monti", 
                    "maxime11", "phd_project", "data")

# Import the data
data <- fread(file.path(folder, "merged-data2021.csv"),
              select = c("player_id", "match_id",
                         "hunting_success", "map_name", 
                         "game_duration", "speed",
                         "space_covered_rate",
                         "prox_mid_PreyGuarding",
                         "hook_start_time",
                         "prey_avg_speed",
                         "prey_avg_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Add observation-level random effect
data$obs <- 1:nrow(data)

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Prepare the variables for the model
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
         "Zhook_start_time", "Zprey_avg_speed",
         "Zprey_avg_space_covered_rate") :=
                lapply(.SD, standardize), 
                .SDcols = c(5:11)]

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



# Quadratic model formula --------------------------------------------------

model_formula <- brmsformula(hunting_success | trials(4) ~
                               # Quadratic terms
                               I(Zspeed^2) +
                               I(Zspace_covered_rate^2) +
                               I(Zprox_mid_PreyGuarding^2) +
                               I(Zhook_start_time^2) +
                               # Linear terms
                               Zspeed +
                               Zspace_covered_rate +
                               Zprox_mid_PreyGuarding +
                               Zhook_start_time +
                               Zgame_duration +
                               # Predator trait covariances
                               Zspeed : Zspace_covered_rate +
                               Zspeed : Zprox_mid_PreyGuarding +
                               Zspeed : Zhook_start_time +
                               Zspace_covered_rate : Zprox_mid_PreyGuarding +
                               Zspace_covered_rate : Zhook_start_time +
                               Zprox_mid_PreyGuarding : Zhook_start_time +
                               (1 | map_name) +
                               (1 | mirrors_id) +
                               (1 | obs))

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 4. Run model
# ==========================================================================


# Model specifications -----------------------------------------------------

quadratic_model <- brm(formula = model_formula,
                       family = binomial(link = "logit"),
                       warmup = 3000, 
                       iter = 11000,
                       thin = 32,
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



# Save the model object ----------------------------------------------------

saveRDS(quadratic_model, file = "03C_hunting_success_quadratic-model1.rds")

# ==========================================================================
# ==========================================================================
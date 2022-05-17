# ==========================================================================

#              PSIS-LOO for the base hunting success model 1               #

# ==========================================================================


# Detect number of cores
options(mc.cores = parallel::detectCores())
# --------------------------------------------------------------------------





# ==========================================================================
# 1. Load libraries and import the data 
# ==========================================================================


# Packages -----------------------------------------------------------------

library(data.table)
library(brms)



# Import the data ----------------------------------------------------------

# Folder path Compute Canada
folder <- file.path("/home", "maxime11", "projects", "def-monti", 
                    "maxime11", "phd_project", "data")

# Import the data
data <- fread(file.path(folder, "FraserFrancoetal2022-data"),
              select = c("player_id", "match_id",
                         "hunting_success", "environment_id", 
                         "game_duration", "speed",
                         "space_covered_rate",
                         "prox_mid_PreyGuarding",
                         "hook_start_time",
                         "prey_avg_speed",
                         "prey_avg_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Add observation-level random effect
#data$obs <- 1:nrow(data)

data <- na.omit(data)

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
                                        (1 | player_id))

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
                  seed = 123,
                  prior = priors,
                  save_pars = save_pars(all = TRUE), 
                  control = list(adapt_delta = 0.95),
                  data = data)

# ==========================================================================
# ==========================================================================





# =======================================================================
# 5. Perform PSIS-leave-one-out cross-validation
# =======================================================================

loo <- loo(base_model,
           moment_match = TRUE)

saveRDS(loo, file = "03B_loo_base-model1.rds")

# =======================================================================
# =======================================================================
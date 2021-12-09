# =======================================================================

#                      LOO cross validation on the                      #
#                     hunting success base-model 1                      #

# =======================================================================





# =======================================================================
# 1. Load the libraries and the model
# =======================================================================


# Import libraries ------------------------------------------------------ 

library(brms)
library(parallel)
options(mc.cores = parallel::detectCores())


# Load the model --------------------------------------------------------

base_model <- readRDS("03B_hunting_success_base-model1.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# Perform the leave-one-out cross-validation
# =======================================================================

base_model <- add_criterion(base_model,
                            criterion = "loo",
                            moment_match = TRUE,
                            moment_match_args = list(cores = 40))

# If it fails, try with loo instead

# Save the output
saveRDS(base_model, file = "base_model1.rds")

# =======================================================================
# =======================================================================
# =======================================================================

#                    K-fold cross validation on the                     #
#                     hunting success base-model 1                      #

# =======================================================================





# =======================================================================
# 1. Load the libraries and the model
# =======================================================================


# Import libraries ------------------------------------------------------ 

options(mc.cores = parallel::detectCores())

library(brms)



# Load the model --------------------------------------------------------

base_model <- readRDS("03B_hunting_success_base-model1.rds")

#recompile_model(base_model)

# =======================================================================
# =======================================================================





# =======================================================================
# Perform the k-fold cross-validation
# =======================================================================

# Load the future package to parallelize the folds
#library(future)
#plan(multicore)
#
#options(future.globals.maxSize = 4000000000)

# Function for the stratified K-fold cross-validation
# We use player_id as the grouping factor that will be stratified.
# This will ensure that the relative frequencies of each player
# will stay approximately equivalent

#cv_object <- kfold(base_model,
#                   K = 10,
#                   Ksub = NULL,
#                   folds = "stratified",
#                   group = "player_id",
#                   chains = 1)


cv_object <-loo(base_model,
                chains = 1,
                cores = 40,
                moment_match = TRUE)

saveRDS(cv_object, file = "loo-base_model1.rds")

# =======================================================================
# =======================================================================
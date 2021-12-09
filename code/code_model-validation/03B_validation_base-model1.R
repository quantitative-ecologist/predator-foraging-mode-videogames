# =======================================================================

#                    K-fold cross validation on the                     #
#                     hunting success base-model 1                      #

# =======================================================================





# =======================================================================
# 1. Load the libraries and the model
# =======================================================================


# Import libraries ------------------------------------------------------ 

library(brms)



# Load the model --------------------------------------------------------

base_model <- readRDS("03B_hunting_success_base-model1.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# Perform the k-fold cross-validation
# =======================================================================

# Load the future package to parallelize the folds
library(future)
plan(multicore)

options(future.globals.maxSize = 4000000)

# Function for the stratified K-fold cross-validation
# We use player_id as the grouping factor that will be stratified.
# This will ensure that the relative frequencies of each player
# will stay approximately equivalent

cv_object <- kfold(base_model,
                   K = 10,
                   Ksub = NULL,
                   folds = "stratified",
                   group = "player_id",
                   chains = 1)

saveRDS(cv_object, file = "kfoldcv-base_model1.rds")

# =======================================================================
# =======================================================================
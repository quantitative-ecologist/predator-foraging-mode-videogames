# =======================================================================

#               LOO-CV table for hunting success models                 #

# =======================================================================





# =======================================================================
# 1. Load libraries and loo objects
# =======================================================================


# Librairies ------------------------------------------------------------

library(data.table)
library(brms)



# Load loo objects ------------------------------------------------------

loo1 <- readRDS("./outputs/R_objects/03B_loo_base-model1.rds")
loo2 <- readRDS("./outputs/R_objects/03B_loo_base-model2.rds")
loo3 <- readRDS("./outputs/R_objects/03c_loo_quadratic-model1.rds")
loo4 <- readRDS("./outputs/R_objects/03c_loo_quadratic-model2.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Compare the models with the loo table
# =======================================================================

loo_compare(loo1, loo2)
loo_compare(loo2, loo3)

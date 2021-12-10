# =======================================================================

#                  Extract fitted values of mv-model 2                  #

# =======================================================================

# Here, I extract the fitted values of the second multivariate-model
# to calculate the ICC conditioned on the variance of the prey
# behaviours, i.e. speed and space covered.



# =======================================================================
# 1. Load libraries and models
# =======================================================================


# Librairies ------------------------------------------------------------

library(data.table)
library(tidybayes)



# Load models -----------------------------------------------------------

model2 <- readRDS("./outputs/models/03A_multivariate-model2.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 1. Calculate the fitted values
# =======================================================================

fitted_values <- posterior_linpred(model2,
                                   ndraws = NULL, # all draws
                                   re_formula = NA)

saveRDS(fitted_values, file = "mv-model2_fitted_tab.rds")

#fitted_values2 <- add_linpred_draws(base_model,
#                                    newdata = NULL,
#                                    ndraws = NULL,
#                                    re_formula = NA,
#                                    resp = "Zspacecoveredrate"
#                                    seed = 123)
#
#fitted_values3 <- posterior_linpred(base_model,
#                                    newdata = NULL,
#                                    ndraws = NULL,
#                                    re_formula = NA,
#                                    resp = "ZproxmidPreyGuarding"
#                                    seed = 123)
#
#fitted_values4 <- posterior_linpred(base_model,
#                                    newdata = NULL,
#                                    ndraws = NULL, 
#                                    re_formula = NA,
#                                    resp = "Zhookstarttime"
#                                    seed = 123)

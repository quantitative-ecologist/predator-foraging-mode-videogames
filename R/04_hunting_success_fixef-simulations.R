############################################################################

#                      Simulations for fixed effects                       #

############################################################################


# The simulations were performed using Calcul Québec's supercomputer
# Beluga



# =======================================================================
# 1. Set working directory, load libraries, and import dataset 
# =======================================================================
setwd("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/PhD_project/chapter1/outputs") # personal computer onedrive UQAM Montiglio lab

# R packages
library(data.table)
library(lme4)


# Load models
load("05B_base-model.rda")
load("05B_quadratic-model.rda")
# =======================================================================
# =======================================================================





# =======================================================================
# 2. Run simulations
# =======================================================================

# Bootstrap fixed effects to have confidence intervals
boot_fixef <- function(.) {
                           fixed_effects = fixef(.)
                           }


# Run simulations (parametric bootstraps) that will refit the model each time
# a. Base model
system.time(
   fixef_base_boot <- bootMer(base_model, 
                       boot_fixef, 
                       seed = 20200701,
                       type = "parametric",
                       parallel = "multicore",
                       ncpus = 30,
                       nsim = 1000)
            )
save(fixef_base_boot, file = "05D_base_fixef_boot.rda") # save output

# b. Quadratic model
system.time(
   fixef_quad_boot <- bootMer(quadratic_model, 
                       boot_fixef, 
                       seed = 20200703, # started at 23h50
                       type = "parametric",
                       parallel = "multicore",
                       ncpus = 30,
                       nsim = 1000)
            )
save(fixef_quad_boot, file = "05D_quad_fixef_boot.rda") # save output
# =======================================================================
# =======================================================================



############################################################################

#                          Simulations for ICC's                           #

############################################################################

# Compute ICC's using simulations (bootrapping)
# The simulations were performed using Calcul Québec's supercomputer Beluga




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

# Distribution specific variance for binomial models (See Nakagawa et al., 2017)
VarDS <- pi^2/3

# Create a function that will compute ICC's for player ID and map name
# I calculate the proportion of explained variance for each random effect by dividing it by the total variance (including the obs in the denominator)
boot_ranef <- function(.) {
                           ICC = (unlist(VarCorr(.)[c(2, 3)])) / (sum(as.numeric(VarCorr(.))) + VarDS)
                           }

# Run 1000 simulations (parametric bootstraps) that will refit the model each time
system.time(
   icc_base_boot <- bootMer(base_model, 
                       boot_ranef, 
                       seed = 20200905,
                       type = "parametric",
                       parallel = "multicore",
                       ncpus = 30,
                       nsim = 1000)
            )
# save output
save(icc_base_boot, file = "05D_base_icc_boot.rda")

system.time(
      icc_quad_boot <- bootMer(quadratic_model, 
                       boot_ranef, 
                       seed = 20200901,
                       type = "parametric",
                       parallel = "multicore",
                       ncpus = 30,
                       nsim = 1000)
            )
# save output
save(icc_quad_boot, file = "05D_quad_icc_boot.rda")
# =======================================================================
# =======================================================================





# Combine the outputs
ICC_tab <- rbind(ICC_boot$t, ICC_boot$t)

# Compute confidence intervals from the bootMer object
CI_lower <- apply(ICC_tab, 2, function(x) as.numeric(quantile(x, probs = .025, na.rm = TRUE)))
CI_upper <- apply(ICC_tab, 2, function(x) as.numeric(quantile(x, probs = .975, na.rm = TRUE)))

# Create a table to save ICC's and their 95% confidence intervals
ICC_boot_tab <- as.data.table(ICC_boot$t0)
setnames(ICC_boot_tab, "V1", "ICC")
ICC_boot_tab[, ":="(random_effect = c("mirrors_id", "map_name"),
                    upper_CI = CI_upper,
                    lower_CI = CI_lower)]
save(ICC_boot_tab, file = "05B_ICC-table.rda")




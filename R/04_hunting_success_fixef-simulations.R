############################################################################

#                      Simulations for fixed effects                       #

############################################################################


# The simulations were performed using Calcul Québec's supercomputer Beluga
# We simulate slopes and % of variances for fixed effects :
# a. the base model
# b the quadratic model



# Contact: Maxime Fraser Franco
# Email: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec


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
# 2. Simulations for fixed effects slopes
# =======================================================================

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





# =======================================================================
# 3. Simulations for fixed effects % of variance
# =======================================================================

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













CI_lower_prey <- apply(boot_prey$t, 2, function(x) as.numeric(quantile(x, probs = .025, na.rm = TRUE)))
CI_upper_prey <- apply(boot_prey$t, 2, function(x) as.numeric(quantile(x, probs = .975, na.rm = TRUE)))

# Create a table to save ICC's and their 95% confidence intervals
boot_prey_tab <- as.data.table(boot_prey$t0)
setnames(boot_prey_tab, "V1", "Value")
boot_prey_tab[, ":="(fixed_effect = c("Intercept", 
                                      "I(Zspeed^2)",
                                        "I(Zspace_covered_rate^2)",
                                        "I(Zprox_mid_guard^2)",
                                        "I(Zsurv_speed^2)",
                                        "I(Zsurv_space_covered_rate^2)",
                                        "Zspeed",
                                        "Zspace_covered_rate",
                                        "Zprox_mid_guard",
                                        "Zsurv_speed",
                                        "Zsurv_space_covered_rate",
                                        "Zspeed : Zspace_covered_rate",
                                        "Zspeed : Zprox_mid_guard",
                                        "Zspace_covered_rate : Zprox_mid_guard",
                                        "Zspeed : Zsurv_speed",
                                        "Zspeed : Zsurv_space_covered_rate",
                                        "Zspace_covered_rate : Zsurv_speed",
                                        "Zspace_covered_rate : Zsurv_space_covered_rate",
                                        "Zprox_mid_guard : Zsurv_speed",
                                        "Zprox_mid_guard : Zsurv_space_covered_rate"
),
lower_ci = CI_lower_prey,
upper_ci = CI_upper_prey)]







boot_prey_tab[, c(1,3,4) := round(.SD, 3), .SDcols = c(1,3,4)]
save(boot_prey_tab, file = "05C_fixef_prey-table.rda")



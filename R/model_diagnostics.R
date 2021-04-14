#########################################################################

#                 Base and quadratic model diagnostics                  #

#########################################################################


# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec
# -----------------------------------------------------------------------





# =======================================================================
# 1. Set working directory, load libraries, datasets, and models
# =======================================================================

# Working directory
setwd("C:/Users/maxim/OneDrive/Documents/GitHub/Chapter2/outputs") # personal computer

# Librairies
library(data.table)
library(brms)
library(bayesplot)

# Load dataset
data <- fread("C:/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/PhD_project/project_data/02_merged-data.csv",
              select = c("mirrors_id", "match_id", 
                         "map_name", "hunting_success", "Zspeed", 
                         "Zprox_mid_guard", "Zspace_covered_rate",
                         "Zsurv_speed", "Zsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Load both models
load("03B_hunting_success_base-model.rda")
load("03C_hunting_success_quadratic-model.rda")

print(object.size(base_model), units = "MB")
print(object.size(base_model2), units = "MB")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Basic model diagnostics (take a very long time to compute)
# =======================================================================

# Visualize on web browser :
# Shinystan app
# -------------------
launch_shinystan(base_model)
launch_shinystan(quadratic_model)
# -------------------


# Diagnose functions
# -------------------
# Posterior predictive checks
brms::pp_check(base_model)
pp_check(quadratic_model)

# Trace plots
plot(base_model)
plot(quadratic_model)
mcmc_plot(base_model, type = "trace")
mcmc_plot(quadratic_model, type = "trace")
#mcmc_trace(base_model)

# Rhat
rhat_vals <- rhat(base_model)
rhat_table <- as.data.table(mcmc_rhat_data(rhat_vals))

rhat_vals_quad <- rhat(quadratic_model)
rhat_table_quad <- as.data.table(mcmc_rhat_data(rhat_vals_quad))

# Effective sample sizes
neff_vals <- neff_ratio(base_model)
neff_table <- as.data.table(mcmc_neff_data(neff_vals))

neff_vals_quad <- neff_ratio(quadratic_model)
neff_table_quad <- as.data.table(mcmc_neff_data(neff_vals_quad))
# -------------------
# =======================================================================
# =======================================================================





# =======================================================================
# 3. Assess model fit (R-squared)
# =======================================================================
# Gelman's R2
# For an introduction to the approach, see Gelman et al. (2018) and
# <URL: https://github.com/jgabry/bayes_R2/>.
base_bayesr2 <- bayes_R2(base_model)
quad_bayesr2 <- bayes_R2(quadratic_model)



capture.output(blabla, file = "blabla.txt")

# Calculation by hand (supplementary material 2 from Nakagawa & al. 2017)
# Create new data
dat <- data.table(speed      = seq(min(data$Zspeed), 
                                   max(data$Zspeed),
                                   length.out = 100), 
                  space      = seq(min(data$Zspace_covered_rate), 
                                   max(data$Zspace_covered_rate),
                                   length.out = 100),             
                  guard      = seq(min(data$Zprox_mid_guard), 
                                   max(data$Zprox_mid_guard),
                                   length.out = 100),            
                  surv_speed = seq(min(data$Zsurv_speed), 
                                   max(data$Zsurv_speed),
                                   length.out = 100),            
                  surv_space = seq(min(data$Zsurv_space_covered_rate), 
                                   max(data$Zsurv_space_covered_rate),
                                   length.out = 100))

library(broom.helpers) # good way to do
mm1 <- model_get_model_matrix(base_model)
# Model matrices
mm1 <- model.matrix(~ speed + 
                      space + 
                      guard + 
                      surv_speed + 
                      surv_space, dat)

mm2 <- model.matrix(~ # Quadratic terms
                     I(speed^2) +
                     I(space^2) +
                     I(guard^2) +
                     I(surv_speed^2) +
                     I(surv_space^2) +
                     # Linear terms
                     speed +
                     space +
                     guard +
                     surv_speed +
                     surv_space +
                     # Predator trait covariances
                     speed : space +
                     speed : guard +
                     space : guard +
                     # Predator-prey trait covariances
                     speed : surv_speed +
                     speed : surv_space +
                     space : surv_speed +
                     space : surv_space +
                     guard : surv_speed +
                     guard : surv_space, dat)


# Variance components :
# 1. Fixed effects variance
# --------------------------
# Compute variance in fitted values (Fixed effects variance)
# [1:100] because only 100 rows
VarF1 <- var(as.vector(mm1%*%fixef(base_model))[1:100])
VarF2 <- var(as.vector(mm2%*%fixef(quadratic_model))[1:100])

# 2. Theoretical distribution-specific variance
# --------------------------
VarDS <- pi^2/3

# 3. Random effects variance
# --------------------------
VarR1 <- VarCorr(base_model)$obs$sd[1]^2 + 
         VarCorr(base_model)$mirrors_id$sd[1]^2 + 
         VarCorr(base_model)$map_name$sd[1]^2

VarR_no_obs1 <- VarCorr(base_model)$mirrors_id$sd[1]^2 + 
                VarCorr(base_model)$map_name$sd[1]^2

VarR2 <- VarCorr(quadratic_model)$obs$sd[1]^2 + 
         VarCorr(quadratic_model)$mirrors_id$sd[1]^2 + 
         VarCorr(quadratic_model)$map_name$sd[1]^2

VarR_no_obs2 <- VarCorr(quadratic_model)$mirrors_id$sd[1]^2 + 
                VarCorr(quadratic_model)$map_name$sd[1]^2

# 4. Total variance
# --------------------------
VarT1 <- VarF1 + VarR1 + VarDS
VarT2 <- VarF2 + VarR2 + VarDS

# 5. Compute R-squared values
# --------------------------
# Marginal R2
R2_M1 <- VarF1 / # Fixed effect variance
         VarT1   # Total variance

R2_M2 <- VarF2 /
         VarT2

# Conditional R2 (OLRE is excluded in the numerator to only account for random effects)
R2_C1 <- (VarF1 + VarR_no_obs1) / # Fixed effect variance + random effect variance
          VarT1                   # Total variance

R2_C2 <- (VarF2 + VarR_no_obs2) /
          VarT2


# Save r-squared values into a table
r_squared <- as.data.table(cbind(R2_M1, R2_C1, R2_M2, R2_C2))

capture.output(r_squared, file = "r2-table.txt")
# =======================================================================
# =======================================================================
# =======================================================================

#                       Base models diagnostics                         #

# =======================================================================





# =======================================================================
# 1. Load libraries and models
# =======================================================================


# Librairies ------------------------------------------------------------

library(data.table)
library(brms)
library(bayesplot)
library(ggpubr)



# Load models -----------------------------------------------------------

#model <- readRDS("./outputs/models/03A_multivariate-model1.rds")
model <- readRDS("./outputs/models/03B_hunting_success_base-model1.rds")

#print(object.size(model), units = "MB")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Model diagnostics
# =======================================================================


# Basic MCMC plots ------------------------------------------------------

# Trace plots and parameter posterior distributions
plot(model)

# Effective sample sizes
neff_vals <- neff_ratio(model) # need to specify the parameters **** (no obs)
neff_table <- as.data.table(mcmc_neff_data(neff_vals))
levels(as.factor(neff_table$description))
# All effective samples sizes are above 0.5 = Good!

# Rhat
rhat_vals <- rhat(model) # need to specify the parameters **** (no obs)
rhat_table <- as.data.table(mcmc_rhat_data(rhat_vals))
levels(as.factor(rhat_table$description))
# All rhat values are below or equal to 1.05 = Good!



# Predictions diagnostics -----------------------------------------------

# Observed y outcomes vs posterior predicted outcomes
dens_overlay <- brms::pp_check(model,
                                type = "dens_overlay",
                                ndraws = 100)

# Export the figure
ggexport(dens_overlay,
         filename = "./outputs/model_diagnostics/03B_diagnostic1.png",
         width = 3000, height = 2500, res = 300)


# Error scatter for predicted values
error <- brms::pp_check(model,
                         type = 'error_scatter_avg',
                         ndraws = 100)

# Export the figure
ggexport(error,
         filename = "./outputs/model_diagnostics/03B_diagnostic2.png",
         width = 3000, height = 2500, res = 300) # more 


# Parameter value around posterior distribution
stat <- brms::pp_check(model,
                       type = 'stat',
                       stat = 'mean',
                       ndraws = 100)

# Export the figure
ggexport(stat,
         filename = "./outputs/model_diagnostics/03B_diagnostic3.png",
         width = 3000, height = 2500, res = 300) # more 



# Residual vs covariate plots -------------------------------------------

# Speed
speed <- brms::pp_check(model,
                         x = "Zspeed",
                         type = "error_scatter_avg_vs_x",
                         ndraws = 100)

space <- brms::pp_check(model,
                         x = "Zspace_covered_rate",
                         type = "error_scatter_avg_vs_x",
                         ndraws = 100)

guard <- brms::pp_check(model,
                         x = "Zprox_mid_PreyGuarding",
                         type = "error_scatter_avg_vs_x",
                         ndraws = 100)

hook <- brms::pp_check(model,
                         x = "Zhook_start_time",
                         type = "error_scatter_avg_vs_x",
                         ndraws = 100)

# Arrange a figure
resid_1 <- ggarrange(speed,
                     space,
                     guard,
                     hook,
                     ncol = 2, nrow = 2)

# Export the figure
ggexport(resid_1,
         filename = "./outputs/model_diagnostics/03B_diagnostic4.png",
         width = 3000, height = 2500, res = 300) # more 

# =======================================================================
# =======================================================================
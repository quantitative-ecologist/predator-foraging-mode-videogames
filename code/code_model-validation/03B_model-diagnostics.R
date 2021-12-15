# =======================================================================

#                       Base models diagnostics                         #

# =======================================================================





# =======================================================================
# 1. Load libraries and models
# =======================================================================


# Librairies ------------------------------------------------------------

options(mc.cores = parallel::detectCores())

library(data.table)
library(brms)
library(bayesplot)



# Load models -----------------------------------------------------------

model1 <- readRDS("03B_hunting_success_base-model1.rds")
model2 <- readRDS("03B_hunting_success_base-model2.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Model diagnostics
# =======================================================================


# Basic MCMC plots ------------------------------------------------------

# Trace plots and parameter posterior distributions
#plot(model)

# Effective sample sizes
neff_vals1 <- neff_ratio(model1, pars = c("b_Intercept", "b_Zspeed",
                                          "b_Zspace_covered_rate", 
                                          "b_Zprox_mid_PreyGuarding",
                                          "b_Zhook_start_time",
                                          "b_Zgame_duration",
                                          "sd_map_name__Intercept",
                                          "sd_obs__Intercept",
                                          "sd_player_id__Intercept"))

neff_vals2 <- neff_ratio(model2, pars = c("b_Intercept", "b_Zspeed",
                                          "b_Zspace_covered_rate",
                                          "b_Zprox_mid_PreyGuarding",
                                          "b_Zhook_start_time",
                                          "b_Zprey_avg_speed",
                                          "b_Zprey_avg_space_covered_rate",
                                          "b_Zgame_duration",
                                          "sd_map_name__Intercept",
                                          "sd_obs__Intercept",
                                          "sd_player_id__Intercept"))

neff_table1 <- as.data.table(mcmc_neff_data(neff_vals1))
neff_table2 <- as.data.table(mcmc_neff_data(neff_vals2))

# Rhat
rhat_vals1 <- rhat(model1, pars = c("b_Intercept", "b_Zspeed",
                                    "b_Zspace_covered_rate", 
                                    "b_Zprox_mid_PreyGuarding",
                                    "b_Zhook_start_time",
                                    "b_Zgame_duration",
                                    "sd_map_name__Intercept",
                                    "sd_obs__Intercept",
                                    "sd_player_id__Intercept"))

rhat_vals2 <- rhat(model2, pars = c("b_Intercept", "b_Zspeed",
                                    "b_Zspace_covered_rate",
                                    "b_Zprox_mid_PreyGuarding",
                                    "b_Zhook_start_time",
                                    "b_Zprey_avg_speed",
                                    "b_Zprey_avg_space_covered_rate",
                                    "b_Zgame_duration",
                                    "sd_map_name__Intercept",
                                    "sd_obs__Intercept",
                                    "sd_player_id__Intercept"))

rhat_table1 <- as.data.table(mcmc_rhat_data(rhat_vals1))
rhat_table2 <- as.data.table(mcmc_rhat_data(rhat_vals2))

capture.output(neff_table1, rhat_table1,
               neff_table2, rhat_table2,
               file = "03B_neff-rhat.txt")



# Predictions diagnostics -----------------------------------------------

# Observed y outcomes vs posterior predicted outcomes
png("03B_diagnostic1.png",
     res = 300,
     width = 3000,
     height = 2500)

par(mar = c(4, 4, 4, 4),
    oma = c(1, 1, 1, 1),
    mfrow = c(1, 2))

brms::pp_check(model1,
               type = "dens_overlay",
               ndraws = 100)

brms::pp_check(model2,
               type = "dens_overlay",
               ndraws = 100)

dev.off()


# Error scatter for predicted values
png("03B_diagnostic2.png",
     res = 300,
     width = 3000,
     height = 2500)

par(mar = c(4, 4, 4, 4),
    oma = c(1, 1, 1, 1),
    mfrow = c(1, 2))

brms::pp_check(model1,
               type = 'error_scatter_avg',
               ndraws = 100)

brms::pp_check(model2,
               type = 'error_scatter_avg',
               ndraws = 100)

dev.off()


# Parameter value around posterior distribution
png("03B_diagnostic3.png",
     res = 300,
     width = 3000,
     height = 2500)

par(mar = c(4, 4, 4, 4),
    oma = c(1, 1, 1, 1),
    mfrow = c(1, 2))

brms::pp_check(model1,
               type = 'stat',
               stat = 'mean',
               ndraws = 100)

brms::pp_check(model2,
               type = 'stat',
               stat = 'mean',
               ndraws = 100)

dev.off()


# Residual vs covariate plots -------------------------------------------

# Model 1
png("03B_diagnostic4.png",
     res = 300,
     width = 3000,
     height = 2500)

par(mar = c(4, 4, 4, 4),
    oma = c(1, 1, 1, 1),
    mfrow = c(2, 2))

brms::pp_check(model1,
               x = "Zspeed",
               type = "error_scatter_avg_vs_x",
               ndraws = 100)

brms::pp_check(model1,
               x = "Zspace_covered_rate",
               type = "error_scatter_avg_vs_x",
               ndraws = 100)

brms::pp_check(model1,
               x = "Zprox_mid_PreyGuarding",
               type = "error_scatter_avg_vs_x",
               ndraws = 100)

brms::pp_check(model1,
                x = "Zhook_start_time",
                type = "error_scatter_avg_vs_x",
                ndraws = 100)

dev.off()


# Model 2
png("03B_diagnostic5.png",
     res = 300,
     width = 4000,
     height = 2500)

par(mar = c(4, 4, 4, 4),
    oma = c(1, 1, 1, 1),
    mfrow = c(2, 3))

brms::pp_check(model2,
               x = "Zspeed",
               type = "error_scatter_avg_vs_x",
               ndraws = 100)

brms::pp_check(model2,
               x = "Zspace_covered_rate",
               type = "error_scatter_avg_vs_x",
               ndraws = 100)

brms::pp_check(model2,
               x = "Zprox_mid_PreyGuarding",
               type = "error_scatter_avg_vs_x",
               ndraws = 100)

brms::pp_check(model2,
                x = "Zhook_start_time",
                type = "error_scatter_avg_vs_x",
                ndraws = 100)

brms::pp_check(model2,
              x = "Zprey_avg_speed",
              type = "error_scatter_avg_vs_x",
              ndraws = 100)

brms::pp_check(model2,
              x = "Zprey_avg_space_covered_rate",
              type = "error_scatter_avg_vs_x",
              ndraws = 100)

dev.off()

# =======================================================================
# =======================================================================
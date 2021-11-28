#########################################################################

#                       Base models diagnostics                         #

#########################################################################





# =======================================================================
# 1. Set working directory, load libraries, datasets, and models
# =======================================================================

# Librairies
library(data.table)
library(brms)
library(rstan)
library(bayesplot)

# Load dataset
data <- fread("./data/merged-data.csv",
              select = c("mirrors_id", "match_id", 
                         "map_name", "hunting_success", "Zspeed", 
                         "Zprox_mid_guard", "Zspace_covered_rate",
                         "Zsurv_speed", "Zhook_start_time",
                         "Zsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Load model
base_model1 <- readRDS("./outputs/03B_hunting_success_base-model1.rds")
load("./outputs/03B_hunting_success_base-model2.rda")
base_model2 <- base_model
rm(base_model)
print(object.size(base_model), units = "MB")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Basic model diagnostics
# =======================================================================


# --------------------------
# Diagnosis
# --------------------------
# Observed y outcomes vs posterior predicted outcomes
dens_overlay1 <- brms::pp_check(base_model1, type = "dens_overlay", nsamples = 100)
dens_overlay2 <- brms::pp_check(base_model2, type = "dens_overlay", nsamples = 100)
#brms::pp_check(base_model, type = 'ecdf_overlay')


# Error scatter for predicted values
error1 <- brms::pp_check(base_model1, type = 'error_scatter_avg', nsamples = 100)
error2 <- brms::pp_check(base_model2, type = 'error_scatter_avg', nsamples = 100)


# Parameter value around posterior distribution
stat1 <- brms::pp_check(base_model1, 
                        type = 'stat', stat = 'mean', nsamples = 100)
stat2 <- brms::pp_check(base_model2, 
                         type = 'stat', stat = 'mean', nsamples = 100)

# Residual vs covariate plots
speed <- brms::pp_check(base_model1, x = 'Zspeed', 
                         type = 'error_scatter_avg_vs_x', nsamples = 100)
space <- brms::pp_check(base_model1, x = 'Zspace_covered_rate', 
                         type = 'error_scatter_avg_vs_x', nsamples = 100)
guard <- brms::pp_check(base_model1, x = 'Zprox_mid_guard', 
                         type = 'error_scatter_avg_vs_x', nsamples = 100)
hook <-  brms::pp_check(base_model1, x = 'Zhook_start_time',
                         type = 'error_scatter_avg_vs_x', nsamples = 100)
survspeed <- brms::pp_check(base_model2, x = 'Zsurv_speed',
                         type = 'error_scatter_avg_vs_x', nsamples = 100)
survspace <- brms::pp_check(base_model2, x = 'Zsurv_space_covered_rate',
               type = 'error_scatter_avg_vs_x', nsamples = 100)


# Trace plots and parameter distributions
#plot(base_model)
trace1 <- mcmc_plot(base_model1, type = "trace")
dens1 <- mcmc_plot(base_model1, type = "dens")

trace2 <- mcmc_plot(base_model2, type = "trace")
dens2 <- mcmc_plot(base_model2, type = "dens")

# Investigate overdispersion
#loo_plot <- plot(loo(base_model))


# Rhat
rhat_vals1 <- rhat(base_model1)
rhat_table1 <- as.data.table(mcmc_rhat_data(rhat_vals1))

rhat_vals2 <- rhat(base_model2)
rhat_table2 <- as.data.table(mcmc_rhat_data(rhat_vals2))

# Display tables
rhat_table1
rhat_table2

# Effective sample sizes
neff_vals1 <- neff_ratio(base_model1)
neff_table1 <- as.data.table(mcmc_neff_data(neff_vals1))

neff_vals2 <- neff_ratio(base_model2)
neff_table2 <- as.data.table(mcmc_neff_data(neff_vals2))


# Display tables
neff_table
# --------------------------
# --------------------------



# --------------------------
# Export plots and tables
# --------------------------
#pp_figure1 <- ggarrange(speed,
#                        space,
#                        guard,
#                        hook,
#                        survspeed,
#                        survspace,
#                        ncol = 3, nrow = 2)
#
#ggexport(pp_figure1, filename = "03B_pp_diagnose1.tiff",
#         width = 4500, height = 2500, res = 500) # more res = bigger plot zoom
#
#
#pp_figure2 <- ggarrange(speed2,
#                        space2,
#                        guard2,
#                        hook2,
#                        survspeed2,
#                        survspace2,
#                        ncol = 3, nrow = 2)
#
#ggexport(pp_figure2, filename = "03B_pp_diagnose2.tiff",
#         width = 5500, height = 3500, res = 500) # more res = bigger plot zoom
#
#
#ggexport(trace1, filename = "03B_trace1.tiff", 
#          width = 6500, height = 3500, res = 800)
#ggexport(dens1, filename = "03B_dens1.tiff", 
#          width = 6500, height = 3500, res = 800)
#ggexport(trace2, filename = "03B_trace2.tiff", 
#          width = 6500, height = 3500, res = 800)
#ggexport(dens2, filename = "03B_dens2.tiff", 
#          width = 6500, height = 3500, res = 800)
# --------------------------

# =======================================================================
# =======================================================================
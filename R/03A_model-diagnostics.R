#########################################################################

#                     Multivariate model diagnostics                    #

#########################################################################





# =======================================================================
# 1. Load libraries, datasets, and models
# =======================================================================

# Librairies
library(data.table)
library(brms)
library(bayesplot)
library(broom.helpers)

# Load dataset
data <- fread("./data/merged-data.csv",
              select = c("mirrors_id", "match_id", "character_name",
                         "map_name", "hunting_success", "sqrtspeed", 
                         "sqrtprox_mid_guard", "sqrtspace_covered_rate",
                         "sqrtsurv_speed", "sqrthook_start_time",
                         "sqrtsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Normalize sqrt variables (Z-scores)
standardize <- function (x) {(x - mean(x)) / sd(x)}

data[, c("Zsqrtspeed", "Zsqrtspace_covered_rate", "Zsqrtprox_mid_guard",
         "Zsqrt_hook_start_time", "Zsqrtsurv_speed", 
         "Zsqrtsurv_space_covered_rate") :=
                lapply(.SD, standardize), 
                .SDcols = c(6:11)]


# Load model
load("./outputs/03A_multivariate-model.rda")
print(object.size(mv_model), units = "MB")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Basic model diagnostics (takes a very long time to compute)
# =======================================================================

# Diagnosis
# -------------------
# Observed y outcomes vs posterior predicted outcomes
dens_overlay1 <- brms::pp_check(mv_model, resp = "Zspeed",
                                type = "dens_overlay", nsamples = 100)
dens_overlay2 <- brms::pp_check(mv_model, resp = "Zspacecoveredrate",
                                type = "dens_overlay", nsamples = 100)
dens_overlay3 <- brms::pp_check(mv_model, resp = "Zproxmidguard",
                                type = "dens_overlay", nsamples = 100)
dens_overlay4 <- brms::pp_check(mv_model, resp = "Zhookstarttime",
                                type = "dens_overlay", nsamples = 100)
#brms::pp_check(mv_model, type = 'ecdf_overlay')


# Error scatter for predicted values
error1 <- brms::pp_check(mv_model, resp = "Zspeed", type = 'error_scatter_avg', nsamples = 100)
error2 <- brms::pp_check(mv_model, resp = "Zspacecoveredrate", type = 'error_scatter_avg', nsamples = 100)
error3 <- brms::pp_check(mv_model, resp = "Zproxmidguard", type = 'error_scatter_avg', nsamples = 100)
error4 <- brms::pp_check(mv_model, resp = "Zhookstarttime", type = 'error_scatter_avg', nsamples = 100)


# Parameter value around posterior distribution
stat1 <- brms::pp_check(mv_model,  
                         resp = "Zsqrtspeed",
                         type = 'stat', stat = 'mean', nsamples = 100)
stat2 <- brms::pp_check(mv_model, 
                         resp = "Zsqrtspacecoveredrate",
                         type = 'stat', stat = 'mean', nsamples = 100)
stat3 <- brms::pp_check(mv_model, 
                         resp = "Zsqrtproxmidguard",
                         type = 'stat', stat = 'mean',  nsamples = 100)
stat4 <- brms::pp_check(mv_model,
                         resp = "Zsqrthookstarttime",
                         type = 'stat', stat = 'mean',  nsamples = 100)


# Residual vs covariate plots
speed1 <- brms::pp_check(mv_model, x = 'Zsqrtsurv_speed', 
                           resp = "Zsqrtspeed",
                         type = 'error_scatter_avg_vs_x', nsamples = 100)
speed2 <- brms::pp_check(mv_model, x = 'Zsqrtsurv_speed',
                           resp = "Zsqrtspacecoveredrate", 
                         type = 'error_scatter_avg_vs_x', nsamples = 100)
speed3 <- brms::pp_check(mv_model, x = 'Zsqrtsurv_speed', 
                           resp = "Zsqrtproxmidguard",
                         type = 'error_scatter_avg_vs_x', nsamples = 100)
speed4 <-  brms::pp_check(mv_model, x = 'Zsqrtsurv_speed',
                            resp = "Zsqrthookstarttime",
                         type = 'error_scatter_avg_vs_x', nsamples = 100)

space1 <- brms::pp_check(mv_model, x = 'Zsurv_speed',
                           resp = "Zsqrtspeed",
                           type = 'error_scatter_avg_vs_x', nsamples = 100)
space2 <- brms::pp_check(mv_model, x = 'Zsurv_space_covered_rate',
                           resp = "Zsqrtspacecoveredrate", 
                           type = 'error_scatter_avg_vs_x', nsamples = 100)
space3 <- brms::pp_check(mv_model, x = 'Zsurv_speed',
                           resp = "Zsqrtproxmidguard",
                           type = 'error_scatter_avg_vs_x', nsamples = 100)
space4 <- brms::pp_check(mv_model, x = 'Zsurv_space_covered_rate',
                           resp = "Zsqrthookstarttime",
                           type = 'error_scatter_avg_vs_x', nsamples = 100)


# Trace plots and parameter distributions
#plot(mv_model)
trace1 <- mcmc_plot(mv_model, type = "trace")
dens1 <- mcmc_plot(mv_model, type = "dens")


# Rhat
rhat_vals <- rhat(mv_model)
rhat_table <- as.data.table(mcmc_rhat_data(rhat_vals))
# Display tables
rhat_table


# Effective sample sizes
neff_vals <- neff_ratio(mv_model)
neff_table <- as.data.table(mcmc_neff_data(neff_vals))
# Display tables
neff_table
# -------------------



# Export plots and tables
# -------------------
#pp_figure1 <- ggarrange(speed1,
#                        space1,
#                        guard1,
#                        hook1,
#                        survspeed1,
#                        survspace1,
#                        ncol = 3, nrow = 2)
#
#ggexport(pp_figure1, filename = "03A_pp_diagnose1.tiff",
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
#ggexport(pp_figure2, filename = "03A_pp_diagnose2.tiff",
#         width = 5500, height = 3500, res = 500) # more res = bigger plot zoom


#ggexport(trace1, filename = "03A_trace1.tiff", 
#          width = 6500, height = 3500, res = 800)
#ggexport(dens1, filename = "03A_dens1.tiff", 
#          width = 6500, height = 3500, res = 800)
#ggexport(trace2, filename = "03A_trace2.tiff", 
#          width = 6500, height = 3500, res = 800)
#ggexport(dens2, filename = "03A_dens2.tiff", 
#          width = 6500, height = 3500, res = 800)
# -------------------

# =======================================================================
# =======================================================================
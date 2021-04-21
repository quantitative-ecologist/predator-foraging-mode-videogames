#########################################################################

#                     Quadratic model diagnostics                       #

#########################################################################


# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec
# -----------------------------------------------------------------------





# =======================================================================
# 1. Set working directory, load libraries, datasets, and models
# =======================================================================

# Librairies
library(data.table)
library(brms)
library(bayesplot)
library(broom.helpers)

# Load dataset
data <- fread("./data/02_merged-data.csv",
              select = c("mirrors_id", "match_id", 
                         "map_name", "hunting_success", "Zspeed", 
                         "Zprox_mid_guard", "Zspace_covered_rate",
                         "Zsurv_speed", "Zhook_start_time",
                         "Zsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Load model
load("03C_hunting_success_quadratic-model.rda")

print(object.size(quadratic_model), units = "MB")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Basic model diagnostics (takes a very long time to compute)
# =======================================================================

# Define Stan functions for diagnosis of the beta-binomial model
# -------------------
expose_functions(betabi_mod, vectorize = TRUE)

# define required log-lik.
log_lik_beta_binomial2 <- function(i, prep) {
  mu <- brms:::get_dpar(prep, "mu", i = i)
  phi <- brms:::get_dpar(prep, "phi", i = i)
  trials <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  beta_binomial2_lpmf(y, mu, phi, trials)
}

# Function for posterior predict
posterior_predict_beta_binomial2 <- function(i, prep, ...) {
  mu <- brms:::get_dpar(prep, "mu", i = i)
  phi <- brms:::get_dpar(prep, "phi", i = i)
  trials <- prep$data$vint1[i]
  beta_binomial2_rng(mu, phi, trials)
}

# 
posterior_epred_beta_binomial2 <- function(prep) {
  mu <- brms:::get_dpar(prep, "mu", i = i)
  trials <- prep$data$vint1
  trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  mu * trials
}
# -------------------


# Diagnosis
# -------------------
# Observed y outcomes vs posterior predicted outcomes
dens_overlay <- brms::pp_check(quad_model, type = "dens_overlay", nsamples = 100)
#brms::pp_check(base_model, type = 'ecdf_overlay')


# Error scatter for y
error <- brms::pp_check(quad_model, type = 'error_scatter_avg', nsamples = 100)


# Parameter value around posterior distribution (arrange for quad parameters)
speed1 <- brms::pp_check(quad_model, x = 'Zspeed', 
                        type = 'stat', stat = 'mean', nsamples = 1000)
space1 <- brms::pp_check(quad_model, x = 'Zspace_covered_rate', 
                         type = 'stat', stat = 'mean', nsamples = 1000)
guard1 <- brms::pp_check(quad_model, x = 'Zprox_mid_guard', 
                         type = 'stat', stat = 'mean',  nsamples = 1000)
hook1 <- brms::pp_check(quad_model, x = 'Zhook_start_time',
                        type = 'stat', stat = 'mean',  nsamples = 1000)
survspeed1 <- brms::pp_check(quad_model, x = 'Zsurv_speed',
                             type = 'stat', stat = 'mean',  nsamples = 1000)
survspace1 <- brms::pp_check(quad_model, x = 'Zsurv_space_covered_rate',
                             type = 'stat', stat = 'mean',  nsamples = 1000)


# residual vs covariate plots
speed2 <- brms::pp_check(quad_model, x = 'Zspeed', 
                         type = 'error_scatter_avg_vs_x', nsamples = 1000)
space2 <- brms::pp_check(quad_model, x = 'Zspace_covered_rate', 
                         type = 'error_scatter_avg_vs_x', nsamples = 1000)
guard2 <- brms::pp_check(quad_model, x = 'Zprox_mid_guard', 
                         type = 'error_scatter_avg_vs_x', nsamples = 1000)
hook2 <-  brms::pp_check(quad_model, x = 'Zhook_start_time',
                         type = 'error_scatter_avg_vs_x', nsamples = 1000)
survspeed2 <- brms::pp_check(quad_model, x = 'Zsurv_speed',
                         type = 'error_scatter_avg_vs_x', nsamples = 1000)
survspace2 <- brms::pp_check(quad_model, x = 'Zsurv_space_covered_rate^2',
               type = 'error_scatter_avg_vs_x', nsamples = 1000)


# Trace plots and parameter distributions
#plot(quad_model)
trace1 <- mcmc_plot(quad_model, type = "trace")
dens1 <- mcmc_plot(quad_model, type = "dens")


# Investigate overdispersion
#loo_plot <- plot(loo(quadratic_model))


# Rhat
rhat_vals_quad <- rhat(quadratic_model)
rhat_table_quad <- as.data.table(mcmc_rhat_data(rhat_vals_quad))
# Display tables
rhat_table_quad


# Effective sample sizes
neff_vals_quad <- neff_ratio(quadratic_model)
neff_table_quad <- as.data.table(mcmc_neff_data(neff_vals_quad))
# Display tables
neff_table_quad
# -------------------



# Export plots and tables
# -------------------
pp_figure1 <- ggarrange(speed1,
                        space1,
                        guard1,
                        hook1,
                        survspeed1,
                        survspace1,
                        ncol = 3, nrow = 2)

ggexport(pp_figure1, filename = "03C_pp_diagnose1.tiff",
         width = 4500, height = 2500, res = 500) # more res = bigger plot zoom


pp_figure2 <- ggarrange(speed2,
                        space2,
                        guard2,
                        hook2,
                        survspeed2,
                        survspace2,
                        ncol = 3, nrow = 2)

ggexport(pp_figure2, filename = "03C_pp_diagnose2.tiff",
         width = 5500, height = 3500, res = 500) # more res = bigger plot zoom


ggexport(trace1, filename = "03C_trace1.tiff", 
          width = 6500, height = 3500, res = 800)
ggexport(dens1, filename = "03C_dens1.tiff", 
          width = 6500, height = 3500, res = 800)
ggexport(trace2, filename = "03C_trace2.tiff", 
          width = 6500, height = 3500, res = 800)
ggexport(dens2, filename = "03C_dens2.tiff", 
          width = 6500, height = 3500, res = 800)
# -------------------

# =======================================================================
# =======================================================================
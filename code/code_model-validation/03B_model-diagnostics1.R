# =======================================================================

#                       Base model 1 diagnostics                        #

# =======================================================================





# =======================================================================
# 1. Load libraries and models
# =======================================================================


# Librairies ------------------------------------------------------------

library(data.table)
library(brms)
library(bayesplot)
library(ggpubr)
#library(broom.helpers)



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
ggexport(dens_fig,
         filename = "./outputs/model_diagnostics/03B_diagnostic1.png",
         width = 3000, height = 2500, res = 300)


# Error scatter for predicted values
error1 <- brms::pp_check(model, resp = "Zspeed",
                         type = 'error_scatter_avg', ndraws = 100)
error2 <- brms::pp_check(model, resp = "Zspacecoveredrate",
                         type = 'error_scatter_avg', ndraws = 100)
error3 <- brms::pp_check(model, resp = "ZproxmidPreyGuarding",
                         type = 'error_scatter_avg', ndraws = 100)
error4 <- brms::pp_check(model, resp = "Zhookstarttime",
                         type = 'error_scatter_avg', ndraws = 100)

# Arrange a figure
error_fig <- ggarrange(error1,
                       error2,
                       error3,
                       error4,
                       ncol = 2, nrow = 2)

# Export the figure
ggexport(error_fig,
         filename = "./outputs/model_diagnostics/03A_diagnostic2.png",
         width = 3000, height = 2500, res = 300) # more 


# Parameter value around posterior distribution
stat1 <- brms::pp_check(model,  
                        resp = "Zspeed",
                        type = 'stat', stat = 'mean', ndraws = 100)
stat2 <- brms::pp_check(model, 
                        resp = "Zspacecoveredrate",
                        type = 'stat', stat = 'mean', ndraws = 100)
stat3 <- brms::pp_check(model, 
                        resp = "ZproxmidPreyGuarding",
                        type = 'stat', stat = 'mean',  ndraws = 100)
stat4 <- brms::pp_check(model,
                        resp = "Zhookstarttime",
                        type = 'stat', stat = 'mean',  ndraws = 100)

# Arrange a figure
stat_fig <- ggarrange(stat1,
                      stat2,
                      stat3,
                      stat4,
                      ncol = 2, nrow = 2)

# Export the figure
ggexport(stat_fig,
         filename = "./outputs/model_diagnostics/03A_diagnostic3.png",
         width = 3000, height = 2500, res = 300) # more 



# Residual vs covariate plots -------------------------------------------

# Prey speed
speed1 <- brms::pp_check(model,
                         x = "Zprey_avg_speed", 
                         resp = "Zspeed",
                         type = "error_scatter_avg_vs_x",
                         ndraws = 100)

space1 <- brms::pp_check(model,
                         x = "Zprey_avg_speed",
                         resp = "Zspacecoveredrate", 
                         type = "error_scatter_avg_vs_x",
                         ndraws = 100)

guard1 <- brms::pp_check(model,
                         x = "Zprey_avg_speed", 
                         resp = "ZproxmidPreyGuarding",
                         type = "error_scatter_avg_vs_x",
                         ndraws = 100)

hook1 <-  brms::pp_check(model,
                         x = "Zprey_avg_speed",
                         resp = "Zhookstarttime",
                         type = "error_scatter_avg_vs_x",
                         ndraws = 100)

# Arrange a figure
resid_1 <- ggarrange(speed1,
                     space1,
                     guard1,
                     hook1,
                     ncol = 2, nrow = 2)

# Export the figure
ggexport(resid_1,
         filename = "./outputs/model_diagnostics/03A_diagnostic4.png",
         width = 3000, height = 2500, res = 300) # more 

# Prey space covered
speed2 <- brms::pp_check(model,
                         x = "Zprey_avg_space_covered_rate",
                         resp = "Zspeed",
                         type = "error_scatter_avg_vs_x",
                         ndraws = 100)

space2 <- brms::pp_check(model,
                         x = "Zprey_avg_space_covered_rate",
                         resp = "Zspacecoveredrate", 
                         type = "error_scatter_avg_vs_x",
                         ndraws = 100)

guard2 <- brms::pp_check(model,
                         x = "Zprey_avg_space_covered_rate",
                         resp = "ZproxmidPreyGuarding",
                         type = "error_scatter_avg_vs_x",
                         ndraws = 100)

hook2 <- brms::pp_check(model,
                        x = "Zprey_avg_space_covered_rate",
                        resp = "Zhookstarttime",
                        type = "error_scatter_avg_vs_x",
                        ndraws = 100)

# Arrange a figure
resid_2 <- ggarrange(speed2,
                     space2,
                     guard2,
                     hook2,
                     ncol = 2, nrow = 2)

# Export the figure
ggexport(resid_2,
         filename = "./outputs/model_diagnostics/03A_diagnostic5.png",
         width = 3000, height = 2500, res = 300) # more 


# =======================================================================
# =======================================================================





# =======================================================================
# 1. Visualize the BLUPs
# =======================================================================

# The code below is from model 2 (2a and 2b)
# I computed the blups from the first model (1a and 1b) with the same code

# Because there were NA's, I extract only the samples that were used
# to compute the correlations [1:2351]

# Extract blups of speed ------------------------------------------------

player_blup <- as_draws_df(model, variable = "r_player_id__Zspeed")[1:2351]
player_blup <- data.table(player_blup)

# Reshape the table
player_blup <- melt(player_blup,
                    variable.name = "player_id",
                    measure = patterns("r_player_id__Zspeed"))

# Change variable name
setnames(player_blup, "value", "blup")

# Compute the confidence intervals and posterior means
lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}

player_blup[, ":=" (posterior_mean = mean(blup), 
                    upper_CI = upper_interval(blup), 
                    lower_CI = lower_interval(blup)), 
              by = player_id]

#player_blup <- unique(player_blup[, -c(1:3)])

# Keep only the ID string
player_blup[, player_id := gsub("r_player_id__Zspeed", "", player_id)]
player_blup[, player_id := gsub(",Intercept", "", player_id)]



# Extract blups of guarding ---------------------------------------------

player_blup1 <- as_draws_df(model, variable = "r_player_id__ZproxmidPreyGuarding")[1:2351]
player_blup1 <- data.table(player_blup1)

# Reshape the table
player_blup1 <- melt(player_blup1,
                    variable.name = "player_id",
                    measure = patterns("r_player_id__ZproxmidPreyGuarding"))

# Change variable name
setnames(player_blup1, "value", "blup")

# Compute the confidence intervals and posterior means
player_blup1[, ":=" (posterior_mean = mean(blup), 
                    upper_CI = upper_interval(blup), 
                    lower_CI = lower_interval(blup)), 
              by = player_id]

#player_blup1 <- unique(player_blup1[, -c(1:3)])

# Keep only the ID string
player_blup1[, player_id := gsub("r_player_id__ZproxmidPreyGuarding", "", player_id)]
player_blup1[, player_id := gsub(",Intercept", "", player_id)]



# Extract blups of space covered ----------------------------------------

player_blup2 <- as_draws_df(model, variable = "r_player_id__Zspacecoveredrate")[1:2351]
player_blup2 <- data.table(player_blup2)

# Reshape the table
player_blup2 <- melt(player_blup2,
                     variable.name = "player_id",
                     measure = patterns("r_player_id__Zspacecoveredrate"))

# Change variable name
setnames(player_blup2, "value", "blup")

# Compute the confidence intervals and posterior means
player_blup2[, ":=" (posterior_mean = mean(blup), 
                    upper_CI = upper_interval(blup), 
                    lower_CI = lower_interval(blup)), 
              by = player_id]

#player_blup2 <- unique(player_blup2[, -c(1:3)])

# Keep only the ID string
player_blup2[, player_id := gsub("r_player_id__Zspacecoveredrate", "", player_id)]
player_blup2[, player_id := gsub(",Intercept", "", player_id)]



# Extract blups of space covered ----------------------------------------

player_blup3 <- as_draws_df(model, variable = "r_player_id__Zhookstarttime")[1:2351]
player_blup3 <- data.table(player_blup3)

# Reshape the table
player_blup3 <- melt(player_blup3,
                     variable.name = "player_id",
                     measure = patterns("r_player_id__Zhookstarttime"))

# Change variable name
setnames(player_blup3, "value", "blup")

# Compute the confidence intervals and posterior means
player_blup3[, ":=" (posterior_mean = mean(blup), 
                     upper_CI = upper_interval(blup), 
                     lower_CI = lower_interval(blup)), 
              by = player_id]

#player_blup3 <- unique(player_blup3[, -c(1:3)])

# Keep only the ID string
player_blup3[, player_id := gsub("r_player_id__Zhookstarttime", "", player_id)]
player_blup3[, player_id := gsub(",Intercept", "", player_id)]



# Plot the relationships ------------------------------------------------

# Plot 1
png("outputs/figures/model_diagnostics/03A_player-blups2a.png",
     res = 300,
     width = 3000,
     height = 2500)

par(mar = c(4, 4, 4, 4),
    oma = c(1, 1, 1, 1),
    mfrow = c(2, 2))

plot(player_blup$posterior_mean~player_blup1$posterior_mean,
          xlab = "speed", ylab = "prey guarding")


plot(player_blup$posterior_mean~player_blup2$posterior_mean,
          xlab = "speed", ylab = "space covered")


plot(player_blup$posterior_mean~player_blup3$posterior_mean,
          xlab = "speed", ylab = "latency 1st capture")

dev.off()

# Plot 2
png("outputs/figures/model_diagnostics/03A_player-blups2b.png",
     res = 300,
     width = 3000,
     height = 2500)

par(mar = c(4, 4, 4, 4),
    oma = c(1, 1, 1, 1),
    mfrow = c(2, 2))

plot(player_blup2$posterior_mean~player_blup1$posterior_mean,
          xlab = "space covered", ylab = "prey guarding")


plot(player_blup2$posterior_mean~player_blup3$posterior_mean,
          xlab = "space covered", ylab = "latency 1st capture")


plot(player_blup1$posterior_mean~player_blup3$posterior_mean,
          xlab = "prey guarding", ylab = "latency 1st capture")

dev.off()

# =======================================================================
# =======================================================================
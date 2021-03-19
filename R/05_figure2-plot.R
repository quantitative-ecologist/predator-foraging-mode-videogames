#########################################################################

#                Panel plot of hunting success analyses                 #

#########################################################################

# Code to produce Figure 2 in Fraser Franco et al. 2021.
# Plot marginal effects of the base and quadratic hunting success models

# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec
# -----------------------------------------------------------------------




# =======================================================================
# 1. Set working directory, load libraries, datasets, and models
# =======================================================================

setwd("C:/Users/maxim/OneDrive/Documents/GitHub/Chapter2") # personal computer onedrive UQAM Montiglio lab

library(data.table)
library(brms)
library(tidybayes)
library(modelr)
#library(Matrix)
#library(arm)
library(ggplot2)
library(viridis)
#library(ggpubr)
#library(tidyr)

# Load dataset
data <- fread("C:/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/PhD_project/project_data/02_merged-data.csv",
              select = c("mirrors_id", "match_id", 
                         "map_name", "hunting_success", "Zspeed", 
                         "Zprox_mid_guard", "Zspace_covered_rate",
                         "Zsurv_speed", "Zsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Load both models
load("base-model.rda")
load("quadratic-model.rda")
load("base_model_threads.rda")
# =======================================================================
# =======================================================================

# Set custom theme for plots
custom_theme <- theme(axis.text.x = element_text(face = "plain", 
                                                 size = 14,
                                                 color = "black"), # axis tick numbers size
                      axis.text.y = element_text(face = "plain", 
                                                 size = 14,
                                                 color = "black"),
                      axis.ticks.length = unit(.15, "cm"), # axis ticks lenght
                      axis.ticks = element_line(size = 0.90, 
                                                color = "black"), # axis ticks width
                      axis.title = element_text(size = 14, 
                                                face = "plain"), # axis titles size
                      axis.line = element_line(size = 0.95),
                      plot.margin = unit(c(2, 1.2, 2, 2), "lines"),
                      legend.position = "none",
                      panel.grid = element_blank(),
                      panel.background = element_blank())

# colorblind friendly palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Small subset of the data for testing the models
data_sub <- data[mirrors_id %in% c("JATHS5909D", "OZDOD9085O", "ZETSA0228O", "YZGIN4008I", "IMVTR2511Q"),]
data_sub$obs <- 1:nrow(data_sub)
data_sub[, prop_captures := hunting_success/4]

# Create a variable for the y axis
data[, prop_captures := hunting_success/4]
# =======================================================================
# =======================================================================





# =======================================================================
# Fixed effects plots for the base model
# =======================================================================

# -----------------------------------
# Predator average movement speed
# -----------------------------------
# Fixed effects table for main effects variance
fe_speed <- tibble(Zspeed = seq(min(data$Zspeed), 
                               max(data$Zspeed),
                               length.out = 100), 
                  Zspace_covered_rate = mean(data$Zspace_covered_rate),             
                  Zprox_mid_guard = mean(data$Zprox_mid_guard),             
                  Zsurv_speed = mean(data$Zsurv_speed),            
                  Zsurv_space_covered_rate = mean(data$Zsurv_space_covered_rate)) %>%
  add_fitted_draws(base_model,
                   re_formula = NA, 
                   value = ".value",
                   scale = "linear", 
                   n = 1e3)

# Random effects table for random effect variance
re_speed <- tibble(Zspeed = seq(min(data$Zspeed), 
                                max(data$Zspeed),
                                length.out = 100),
                  Zspace_covered_rate = mean(data$Zspace_covered_rate),           
                  Zprox_mid_guard = mean(data$Zprox_mid_guard),             
                  Zsurv_speed = mean(data$Zsurv_speed),             
                  Zsurv_space_covered_rate = mean(data$Zsurv_space_covered_rate)) %>%
  add_fitted_draws(base_model,
                   scale = "linear", n = 1e3,
                   re_formula = NULL,
                   allow_new_levels = TRUE)

# Calculate fitted values
fe_speed_mean <- fe_speed %>% 
  group_by(Zspeed) %>%
  summarize(.value = mean(.value))

# Marginal effect of Zspeed
 speed <- ggplot(data, 
                 mapping = aes(y = prop_captures, 
                               x = Zspeed)) +
              geom_point(shape = 16, 
                         alpha = 0.1, 
                         color = "black") +
              geom_line(data = fe_speed,
                         mapping = aes(x = Zspeed, 
                                       y = plogis(.value),
                                       group = .draw), 
                         alpha = 0.1) +
              geom_line(data = fe_speed_mean, 
                        mapping = aes(x = Zspeed,
                                      y = plogis(.value)),
                        color = cbp1[7], 
                        lwd = 2, 
                        group = 1) +
              ylab("") +
              xlab("\nSpeed") +
              custom_theme +
              scale_y_continuous(breaks = seq(0, 1, .25),
                                 limits = c(0, 1)) +
              custom_theme
  
# Plot with fixed and random effect variance
speed <- ggplot(data, 
                aes(y = prop_captures, 
                    x = Zspeed)) +
            geom_point(shape = 16, 
                       alpha = 0.1, 
                       color = "black") +
            geom_line(data = re_speed, 
                      mapping = aes(y = plogis(.value), 
                                    x = Zspeed, 
                                    group = .draw),
                      color = cbp1[6], 
                      alpha = 0.1) +
            geom_line(data = fe_speed, 
                      mapping = aes(y = plogis(.value), 
                                    x = Zspeed, 
                                    group = .draw), 
                      color = cbp1[2], 
                      alpha = 0.1) +
            geom_line(data = fe_speed_mean, 
                      mapping = aes(y = plogis(.value), 
                                    x = Zspeed),
                      color = cbp1[6], 
                      lwd = 2, 
                      group = 1, 
                      linetype = "dashed") +
                    ylab("") +
                    xlab("\nSpeed") +
                    scale_y_continuous(breaks = seq(0, 1, .25),
                                       limits = c(0, 1)) +
                    custom_theme
# -----------------------------------


# -----------------------------------
# Predator rate of space covered
# -----------------------------------
# Fixed effects table for main effects variance
fe_space <- tibble(Zspace_covered_rate = seq(min(data$Zspace_covered_rate), 
                                         max(data$Zspace_covered_rate),
                                         length.out = 100),
                  Zspeed = mean(data$Zspeed),           
                  Zprox_mid_guard = mean(data$Zprox_mid_guard),             
                  Zsurv_speed = mean(data$Zsurv_speed),             
                  Zsurv_space_covered_rate = mean(data$Zsurv_space_covered_rate)) %>%
  add_fitted_draws(base_model,
                   re_formula = NA, 
                   value = ".value",
                   scale = "linear", 
                   n = 1e3)

# Random effects table for random effect variance
re_space <- tibble(Zspace_covered_rate = seq(min(data$Zspace_covered_rate), 
                                         max(data$Zspace_covered_rate),
                                         length.out = 100),
                  Zspeed = mean(data$Zspeed),           
                  Zprox_mid_guard = mean(data$Zprox_mid_guard),             
                  Zsurv_speed = mean(data$Zsurv_speed),             
                  Zsurv_space_covered_rate = mean(data$Zsurv_space_covered_rate)) %>%
  add_fitted_draws(base_model,
                   scale = "linear", n = 1e3,
                   re_formula = NULL,
                   allow_new_levels = TRUE)

# Calculate fitted values
fe_space_mean <- fe_space %>% 
  group_by(Zspace_covered_rate) %>%
  summarize(.value = mean(.value))

# # Marginal effect of Zspace
 space <- ggplot(data, 
                 mapping = aes(y = prop_captures, 
                               x = Zspace_covered_rate)) +
              geom_point(shape = 16, 
                         alpha = 0.1, 
                         color = "black") +
              geom_line(data = fe_space,
                         mapping = aes(x = Zspace_covered_rate, 
                                       y = plogis(.value),
                                       group = .draw), 
                         alpha = 0.1) +
              geom_line(data = fe_space_mean, 
                        mapping = aes(x = Zspace_covered_rate,
                                      y = plogis(.value)),
                        color = cbp1[7], 
                        lwd = 2, 
                        group = 1) +
              ylab("") +
              xlab("\nSpace") +
              custom_theme +
              scale_y_continuous(breaks = seq(0, 1, .25),
                                 limits = c(0, 1)) +
              custom_theme

# Plot with fixed and random effect variance
space <- ggplot(data, 
                aes(y = prop_captures, 
                    x = Zspace_covered_rate)) +
            geom_point(shape = 16, 
                       alpha = 0.1, 
                       color = "black") +
            geom_line(data = re_space, 
                      mapping = aes(y = plogis(.value), 
                                    x = Zspace_covered_rate, 
                                    group = .draw),
                      color = cbp1[6], 
                      alpha = 0.1) +
            geom_line(data = fe_space, 
                      mapping = aes(y = plogis(.value), 
                                    x = Zspace_covered_rate, 
                                    group = .draw), 
                      color = cbp1[2], 
                      alpha = 0.1) +
            geom_line(data = fe_space_mean, 
                      mapping = aes(y = plogis(.value), 
                                    x = Zspace_covered_rate),
                      color = cbp1[6], 
                      lwd = 2, 
                      group = 1, 
                      linetype = "dashed") +
                    ylab("") +
                    xlab("\nSpace") +
                    scale_y_continuous(breaks = seq(0, 1, .25),
                                       limits = c(0, 1)) +
                    custom_theme
# -----------------------------------


# -----------------------------------
# Predator proportion of time spent guarding
# -----------------------------------
# Fixed effects table for main effects variance
fe_guard <- tibble(Zprox_mid_guard = seq(min(data_sub$Zprox_mid_guard), 
                                         max(data_sub$Zprox_mid_guard),
                                         length.out = 100),
                  Zspeed = mean(data_sub$Zspeed),           
                  Zspace_covered_rate = mean(data_sub$Zspace_covered_rate),             
                  Zsurv_speed = mean(data_sub$Zsurv_speed),             
                  Zsurv_space_covered_rate = mean(data_sub$Zsurv_space_covered_rate)) %>%
  add_fitted_draws(base_model,
                   re_formula = NA, 
                   value = ".value",
                   scale = "linear", 
                   n = 1e3)

# Random effects table for random effect variance
re_guard <- tibble(Zprox_mid_guard = seq(min(data_sub$Zprox_mid_guard), 
                                         max(data_sub$Zprox_mid_guard),
                                         length.out = 100),
                  Zspeed = mean(data_sub$Zspeed),           
                  Zspace_covered_rate = mean(data_sub$Zspace_covered_rate),             
                  Zsurv_speed = mean(data_sub$Zsurv_speed),             
                  Zsurv_space_covered_rate = mean(data_sub$Zsurv_space_covered_rate)) %>%
  add_fitted_draws(base_model,
                   scale = "linear", n = 1e3,
                   re_formula = NULL,
                   allow_new_levels = TRUE)

# Calculate fitted values
fe_guard_mean <- fe_guard %>% 
  group_by(Zprox_mid_guard) %>%
  summarize(.value = mean(.value))

# Marginal effect of Zspeed
 guard <- ggplot(data_sub, 
                 mapping = aes(y = prop_captures, 
                               x = Zprox_mid_guard)) +
              geom_point(shape = 16, 
                         alpha = 0.1, 
                         color = "black") +
              geom_line(data = fe_guard,
                         mapping = aes(x = Zprox_mid_guard, 
                                       y = plogis(.value),
                                       group = .draw), 
                         alpha = 0.1) +
              geom_line(data = fe_guard_mean, 
                        mapping = aes(x = Zprox_mid_guard,
                                      y = plogis(.value)),
                        color = cbp1[7], 
                        lwd = 2, 
                        group = 1) +
              ylab("") +
              xlab("\nGuard") +
              custom_theme +
              scale_y_continuous(breaks = seq(0, 1, .25),
                                 limits = c(0, 1)) +
              custom_theme

# Plot with fixed and random effect variance
guard <- ggplot(data_sub, 
                aes(y = prop_captures, 
                    x = Zprox_mid_guard)) +
            geom_point(shape = 16, 
                       alpha = 0.1, 
                       color = "black") +
            geom_line(data = re_guard, 
                      mapping = aes(y = plogis(.value), 
                                    x = Zprox_mid_guard, 
                                    group = .draw),
                      color = cbp1[6], 
                      alpha = 0.1) +
            geom_line(data = fe_guard, 
                      mapping = aes(y = plogis(.value), 
                                    x = Zprox_mid_guard, 
                                    group = .draw), 
                      color = cbp1[2], 
                      alpha = 0.1) +
            geom_line(data = fe_guard_mean, 
                      mapping = aes(y = plogis(.value), 
                                    x = Zprox_mid_guard),
                      color = cbp1[6], 
                      lwd = 2, 
                      group = 1, 
                      linetype = "dashed") +
                    ylab("") +
                    xlab("\nGuard") +
                    scale_y_continuous(breaks = seq(0, 1, .25),
                                       limits = c(0, 1)) +
                    custom_theme
# -----------------------------------

# =======================================================================
# =======================================================================






















# =======================================================================
# 2. Base hunting success plots
# =======================================================================

# Build model matrixes and coefficient tables

# Z speed gradient model matrix
# -------------------------------------------------
speed_newdat <- data.frame(speed_x = seq(min(data$Zspeed), max(data$Zspeed), length = 100))

# Compute fitted values
speed_mm <- model.matrix(~speed_x, speed_newdat)
speed_y <- speed_mm%*%fixef(directional_model)[c(1,2)] # I'm extracting only Zspeed and the intercept

# Compute 95% confidence intervals and prediction intervals
speed_pvar1 <- diag(speed_mm %*% tcrossprod(vcov(directional_model)[c(1,2), c(1,2)], speed_mm)) # 2x2 matrix of intercept and Zspeed
speed_tvar1 <- speed_pvar1 + VarCorr(directional_model)$obs[1] + VarCorr(directional_model)$mirrors_id[1] + VarCorr(directional_model)$map_name[1]

# Create the table used for the plot
speed_newdat <- data.frame(
  speed_x = speed_newdat$speed_x,
  speed_y = invlogit(speed_y),
  speed_plo = invlogit(speed_y - 1.96 * sqrt(speed_pvar1)),
  speed_phi = invlogit(speed_y + 1.96 * sqrt(speed_pvar1)),
  speed_tlo = invlogit(speed_y - 1.96 * sqrt(speed_tvar1)),
  speed_thi = invlogit(speed_y + 1.96 * sqrt(speed_tvar1))
)

# Z space covered rate gradient model matrix
# -------------------------------------------------
space_newdat <- data.frame(space_x = seq(min(data$Zspace_covered_rate), max(data$Zspace_covered_rate), length = 100))

# Compute fitted values
space_mm <- model.matrix(~space_x, space_newdat)
space_y <- space_mm%*%fixef(directional_model)[c(1,4)] # I'm extracting only Z space covered and the intercept

# Compute 95% confidence intervals and prediction intervals
space_pvar1 <- diag(space_mm %*% tcrossprod(vcov(directional_model)[c(1,4), c(1,4)], space_mm)) # 2x2 matrix of intercept and Z space covered
space_tvar1 <- space_pvar1 + VarCorr(directional_model)$obs[1] + VarCorr(directional_model)$mirrors_id[1] + VarCorr(directional_model)$map_name[1]

# Create the table used for the plot
space_newdat <- data.frame(
  space_x = space_newdat$space_x,
  space_y = invlogit(space_y),
  space_plo = invlogit(space_y - 1.96 * sqrt(space_pvar1)),
  space_phi = invlogit(space_y + 1.96 * sqrt(space_pvar1)),
  space_tlo = invlogit(space_y - 1.96 * sqrt(space_tvar1)),
  space_thi = invlogit(space_y + 1.96 * sqrt(space_tvar1))
)

# Z prey guarding gradient model matrix
# -------------------------------------------------
guard_newdat <- data.frame(guard_x = seq(min(data$Zprox_mid_guard), max(data$Zprox_mid_guard), length = 100))

# Compute fitted values
guard_mm <- model.matrix(~guard_x, guard_newdat)
guard_y <- guard_mm%*%fixef(directional_model)[c(1,3)] # I'm extracting only Z space covered and the intercept

# Compute 95% confidence intervals and prediction intervals
guard_pvar1 <- diag(guard_mm %*% tcrossprod(vcov(directional_model)[c(1,3), c(1,3)], guard_mm)) # 2x2 matrix of intercept and Z guarding
guard_tvar1 <- guard_pvar1 + VarCorr(directional_model)$obs[1] + VarCorr(directional_model)$mirrors_id[1] + VarCorr(directional_model)$map_name[1]

# Create the table used for the plot
guard_newdat <- data.frame(
  guard_x = guard_newdat$guard_x,
  guard_y = invlogit(guard_y),
  guard_plo = invlogit(guard_y - 1.96 * sqrt(guard_pvar1)),
  guard_phi = invlogit(guard_y + 1.96 * sqrt(guard_pvar1)),
  guard_tlo = invlogit(guard_y - 1.96 * sqrt(guard_tvar1)),
  guard_thi = invlogit(guard_y + 1.96 * sqrt(guard_tvar1))
)
# -----------------------------------------------------------------------------------------





# 3. Quadratic selection plots ==========================================
# =======================================================================
# -----------------------------------------------------------------------------------------
# Build model matrixes and coefficient tables

# Z speed^2 gradient model matrix
# -------------------------------------------------
# Covariates fixed at average values
speed_newdat <- expand.grid(speed = seq(min(data$Zspeed),
                                        max(data$Zspeed),
                                        length = 20),
                            space = seq(mean(data$Zspace_covered_rate),
                                        mean(data$Zspace_covered_rate)),
                            guard = seq(mean(data$Zprox_mid_guard),
                                        mean(data$Zprox_mid_guard)))

# Compute fitted values
speed_mm <- model.matrix(~
                           I(speed^2) +
                           I(space^2) +
                           I(guard^2) +
                           speed * space +
                           speed * guard +
                           space * guard, speed_newdat)
speed_y <- speed_mm%*%fixef(quadratic_model)

# Compute 95% confidence intervals and prediction intervals
speed_pvar1 <- diag(speed_mm %*% tcrossprod(vcov(quadratic_model), speed_mm))
speed_tvar1 <- speed_pvar1 + VarCorr(quadratic_model)$obs[1] + VarCorr(quadratic_model)$mirrors_id[1] + VarCorr(quadratic_model)$map_name[1]

# Create the table used for the plot
speed_newdat <- data.frame(
  speed_x = speed_newdat$speed,
  space_x = speed_newdat$space,
  guard_x = speed_newdat$guard,
  speed_y = invlogit(speed_y),
  speed_plo = invlogit(speed_y - 1.96 * sqrt(speed_pvar1)),
  speed_phi = invlogit(speed_y + 1.96 * sqrt(speed_pvar1)),
  speed_tlo = invlogit(speed_y - 1.96 * sqrt(speed_tvar1)),
  speed_thi = invlogit(speed_y + 1.96 * sqrt(speed_tvar1))
)

speed_newdat <- gather(speed_newdat, variable, estimate, c(2:3))
# -----------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------
# Create the plots

# Z speed^2 plot
# -------------------------------------------------
quadratic_speed_plot <- ggplot() +
  geom_point(data = data,
             aes(x = Zspeed, y = prop_bloodpoints),
             shape = 16,
             alpha = 0.1,
             color = "black") + # or point shape 20
  geom_line(data = speed_newdat,
            aes(x = speed_x, y = speed_y),
            size = 1.5,
            color = "#F8766D") +
  geom_line(data = speed_newdat,
            aes(x = speed_x, y = speed_plo),
            linetype = "dashed",
            size = 1,
            color = "#00BFC4") +
  geom_line(data = speed_newdat,
            aes(x = speed_x, y = speed_phi),
            linetype = "dashed",
            size = 1, 
            color = "#00BFC4") +
  geom_ribbon(data = speed_newdat,
              aes(x = speed_x, ymin = speed_tlo, ymax = speed_thi),
              alpha = 0.2,
              fill = "#F8766D") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     expand = c(0, 0),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-8, 6, 4),
                     expand = c(0, 0),
                     limits = c(-8, 6)) +
  #ylab("Probability of gaining bloodpoints\n") +
  ylab("") +
  xlab("\nSpeed") +
  theme(axis.text.x = element_text(face = "plain",
                                   size = 14,
                                   color = "black"), # axis tick numbers size
        axis.text.y = element_text(face = "plain",
                                   size = 14,
                                   color = "black"),
        axis.ticks.length = unit(.15, "cm"), # axis ticks lenght
        axis.ticks = element_line(size = 0.90,
                                  color = "black"), # axis ticks width
        axis.title = element_text(size = 14,
                                  face = "plain"), # axis titles size
        axis.line = element_line(size = 0.95),
        plot.margin = unit(c(2, 1.2, 2, 2), "lines"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_blank())
# =======================================================================
# =======================================================================





# 4. Create the figure with 6 panels ====================================
# =======================================================================
# Create the 3 paneled figure
panel_plot <- ggarrange(speed,
                        space,
                        guard,
                        quad_speed,
                        quad_space,
                        quad_guard,
                        ncol = 3, nrow = 2,
                        widths = c(2.8, 2.5, 2.5),
                        heights = c(2.8, 2.8, 2.8),
                        labels = c("(a)", "(b)", "(c)", 
                                   "(d)", "(e)", "(f)"))

panel_plot <- annotate_figure(panel_plot,
                              left = text_grob("Hunting success", 
                                               rot = 90,
                                               size = 14,
                                               hjust = -0.55, vjust = 0.5))
# almost perfect
panel_plot <- annotate_figure(panel_plot,
                              left = text_grob("Hunting success", 
                                               rot = 90,
                                               size = 14,
                                               hjust = 1.23, vjust = 2.1))
# 1.11 avant
# Save and export figure
ggexport(panel_plot, filename = "06D_Figure1.tiff",
         width = 3500, height = 2500, res = 300) # more res = bigger plot zoom


# End of script =========================================================
# =======================================================================
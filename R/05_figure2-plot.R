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

setwd("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/masters_project/chapter1/outputs") # personal computer onedrive UQAM Montiglio lab

library(data.table)
library(lme4)
library(Matrix)
library(arm)
library(ggplot2)
library(ggpubr)
library(tidyr)

# Load the dataset
data <- fread("02_merged-data.csv",
                        select = c("cohort", "mirrors_id", "match_id", "map_name", "sum_bloodpoints",
                                   "Zspeed", "Zprox_mid_guard", "Zspace_covered_rate",
                                   "sqrtspeed", "sqrtprox_mid_guard", "sqrtspace_covered_rate"))

# Create the proportion of bloodpoints obtained for plotting
data[, prop_bloodpoints := sum_bloodpoints / 32000]

# Character variables to factor variables
char_as_factor <- names(data)[sapply(data, is.character)] # extract columns that are characters
data[, (char_as_factor) := lapply(.SD, as.factor), .SDcols = char_as_factor] # columns as factors

# Load both models
load("05B_directional-model.rda")
load("05C_quadratic-model.rda")
# =======================================================================
# =======================================================================





# 2 Directional selection plots =========================================
# =======================================================================
# -----------------------------------------------------------------------------------------
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


# -----------------------------------------------------------------------------------------
# Create the plots

# Z speed plot
# -------------------------------------------------
speed_plot <- ggplot() +
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
 # ylab("Probability of gaining bloodpoints\n") +
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

# Z space covered rate plot
# -------------------------------------------------
space_plot <- ggplot() +
  geom_point(data = data,
             aes(x = Zspace_covered_rate, y = prop_bloodpoints),
             shape = 16, 
             alpha = 0.1, 
             color = "black") + # or point shape 20
  geom_line(data = space_newdat,
            aes(x = space_x, y = space_y),
            size = 1.5,
            color = "#F8766D") +
  geom_line(data = space_newdat,
            aes(x = space_x, y = space_plo),
            linetype = "dashed", 
            size = 1, 
            color = "#00BFC4") +
  geom_line(data = space_newdat,
            aes(x = space_x, y = space_phi),
            linetype = "dashed", 
            size = 1, 
            color = "#00BFC4") +
  geom_ribbon(data = space_newdat,
              aes(x = space_x, ymin = space_tlo, ymax = space_thi),
              alpha = 0.2,
              fill = "#F8766D") +
  scale_y_continuous(breaks = seq(0, 1, .25), 
                     expand = c(0, 0),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-4, 8, 4), 
                     expand = c(0, 0),
                     limits = c(-4, 8)) +
  # ylab("Probability of gaining bloodpoints\n") +
  ylab("") +
  xlab("\nSpace") +
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
        plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_blank())

# Z prey guarding plot
# -------------------------------------------------
guard_plot <- ggplot() +
  geom_point(data = data,
             aes(x = Zprox_mid_guard, y = prop_bloodpoints),
             shape = 16, 
             alpha = 0.1, 
             color = "black") + # or point shape 20
  geom_line(data = guard_newdat,
            aes(x = guard_x, y = guard_y), 
            size = 1.5,
            color = "#F8766D") +
  geom_line(data = guard_newdat,
            aes(x = guard_x, y = guard_plo),
            linetype = "dashed", 
            size = 1,
            color = "#00BFC4") +
  geom_line(data = guard_newdat,
            aes(x = guard_x, y = guard_phi),
            linetype = "dashed", 
            size = 1,
            color = "#00BFC4") +
  geom_ribbon(data = guard_newdat,
              aes(x = guard_x, ymin = guard_tlo, ymax = guard_thi),
              alpha = 0.2,
              fill = "#F8766D") +
  scale_y_continuous(breaks = seq(0, 1, .25), 
                     expand = c(0, 0),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-2, 14, 4), 
                     expand = c(0, 0),
                     limits = c(-2, 14)) +
  # ylab("Probability of gaining bloodpoints\n") +
  ylab("") +
  xlab("\nGuard") +
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
        plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_blank())
# -----------------------------------------------------------------------------------------
# =======================================================================
# =======================================================================





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


# Z space covered rate^2 gradient model matrix
# -------------------------------------------------
# Covariates fixed at average values
space_newdat <- expand.grid(space = seq(min(data$Zspace_covered_rate),
                                        max(data$Zspace_covered_rate),
                                        length = 20),
                            speed = seq(mean(data$Zspeed),
                                        mean(data$Zspeed)),
                            guard = seq(mean(data$Zprox_mid_guard),
                                        mean(data$Zprox_mid_guard)))
# Fitted values
space_mm <- model.matrix(~
                           I(speed^2) +
                           I(space^2) +
                           I(guard^2) +
                           speed * space +
                           speed * guard +
                           space * guard, space_newdat)
space_y <- space_mm%*%fixef(quadratic_model)

# Compute 95% confidence intervals and prediction intervals
space_pvar1 <- diag(space_mm %*% tcrossprod(vcov(quadratic_model), space_mm))
space_tvar1 <- space_pvar1 + VarCorr(quadratic_model)$obs[1] + VarCorr(quadratic_model)$mirrors_id[1] + VarCorr(quadratic_model)$map_name[1]

# Create the table used for the plot
space_newdat <- data.frame(
  space_x = space_newdat$space,
  speed_x = space_newdat$speed,
  guard_x = space_newdat$guard,
  space_y = invlogit(space_y),
  space_plo = invlogit(space_y - 1.96 * sqrt(space_pvar1)),
  space_phi = invlogit(space_y + 1.96 * sqrt(space_pvar1)),
  space_tlo = invlogit(space_y - 1.96 * sqrt(space_tvar1)),
  space_thi = invlogit(space_y + 1.96 * sqrt(space_tvar1))
)

space_newdat <- gather(space_newdat, variable, estimate, c(2:3))

# Z prey guarding^2 gradient model matrix
# -------------------------------------------------
# Covariates fixed at average values
guard_newdat <- expand.grid(guard = seq(min(data$Zprox_mid_guard),
                                        max(data$Zprox_mid_guard),
                                        length = 20),
                            speed = seq(mean(data$Zspeed),
                                        mean(data$Zspeed)),
                            space = seq(mean(data$Zspace_covered_rate),
                                        mean(data$Zspace_covered_rate)))
# Fitted values
guard_mm <- model.matrix(~
                           I(speed^2) +
                           I(space^2) +
                           I(guard^2) +
                           speed * space +
                           speed * guard +
                           space * guard, guard_newdat)
guard_y <- guard_mm%*%fixef(quadratic_model)

# Compute 95% confidence intervals and prediction intervals
guard_pvar1 <- diag(guard_mm %*% tcrossprod(vcov(quadratic_model), guard_mm))
guard_tvar1 <- guard_pvar1 + VarCorr(quadratic_model)$obs[1] + VarCorr(quadratic_model)$mirrors_id[1] + VarCorr(quadratic_model)$map_name[1]

# Create the table used for the plot
guard_newdat <- data.frame(
  guard_x = guard_newdat$guard,
  speed_x = guard_newdat$speed,
  space_x = guard_newdat$space,
  guard_y = invlogit(guard_y),
  guard_plo = invlogit(guard_y - 1.96 * sqrt(guard_pvar1)),
  guard_phi = invlogit(guard_y + 1.96 * sqrt(guard_pvar1)),
  guard_tlo = invlogit(guard_y - 1.96 * sqrt(guard_tvar1)),
  guard_thi = invlogit(guard_y + 1.96 * sqrt(guard_tvar1))
)

guard_newdat <- gather(guard_newdat, variable, estimate, c(2:3))
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

# Z space covered rate^2 plot
# -------------------------------------------------
quadratic_space_plot <- ggplot() +
  geom_point(data = data,
             aes(x = Zspace_covered_rate, y = prop_bloodpoints),
             shape = 16, 
             alpha = 0.1, 
             color = "black") + # or point shape 20
  geom_line(data = space_newdat,
            aes(x = space_x, y = space_y),
            size = 1.5, 
            color = "#F8766D") +
  geom_line(data = space_newdat,
            aes(x = space_x, y = space_plo),
            linetype = "dashed", 
            size = 1, 
            color = "#00BFC4") +
  geom_line(data = space_newdat,
            aes(x = space_x, y = space_phi),
            linetype = "dashed", 
            size = 1, 
            color = "#00BFC4") +
  geom_ribbon(data = space_newdat,
              aes(x = space_x, ymin = space_tlo, ymax = space_thi),
              alpha = 0.2,
              fill = "#F8766D") +
  scale_y_continuous(breaks = seq(0, 1, .25), 
                     expand = c(0, 0),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-4, 8, 4), 
                     expand = c(0, 0),
                     limits = c(-4, 8)) +
  # ylab("Probability of obtaining bloodpoints\n\n") +
  ylab("") +
  xlab("\nSpace") +
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
        plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_blank())

# Z prey guarding^2 plot
# -------------------------------------------------
quadratic_guard_plot <- ggplot() +
  geom_point(data = data,
             aes(x = Zprox_mid_guard, y = prop_bloodpoints),
             shape = 16, 
             alpha = 0.1,
             color = "black") + # or point shape 20
  geom_line(data = guard_newdat,
            aes(x = guard_x, y = guard_y), 
            size = 1.5,
            color = "#F8766D") +
  geom_line(data = guard_newdat,
            aes(x = guard_x, y = guard_plo),
            linetype = "dashed",
            size = 1,
            color = "#00BFC4") +
  geom_line(data = guard_newdat,
            aes(x = guard_x, y = guard_phi),
            linetype = "dashed",
            size = 1, 
            color = "#00BFC4") +
  geom_ribbon(data = guard_newdat,
              aes(x = guard_x, ymin = guard_tlo, ymax = guard_thi),
              alpha = 0.2,
              fill = "#F8766D") +
  scale_y_continuous(breaks = seq(0, 1, .25), 
                     expand = c(0, 0),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-2, 14, 4),
                     expand = c(0, 0),
                     limits = c(-2, 14)) +
  ylab("") +
  xlab("\nGuard") +
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
        plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"), #top right bottom left
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_blank())
# -----------------------------------------------------------------------------------------
# =======================================================================
# =======================================================================





# 4. Create the figure with 6 panels ====================================
# =======================================================================
# Create the 3 paneled figure
panel_plot <- ggarrange(speed_plot,
                        space_plot,
                        guard_plot,
                        quadratic_speed_plot,
                        quadratic_space_plot,
                        quadratic_guard_plot,
                        ncol = 3, nrow = 2,
                        widths = c(2.8, 2.5, 2.5),
                        heights = c(2.8, 2.8, 2.8),
                        labels = c("(a)", "(b)", "(c)", 
                                   "(d)", "(e)", "(f)"))

panel_plot <- annotate_figure(panel_plot,
                              left = text_grob("Probability of earning points", 
                                               rot = 90,
                                               size = 14,
                                               hjust = -0.55, vjust = 0.5))
# almost perfect
panel_plot <- annotate_figure(panel_plot,
                              left = text_grob("Probability of earning points", 
                                               rot = 90,
                                               size = 14,
                                               hjust = 1.23, vjust = 2.1))
# 1.11 avant
# Save and export figure
ggexport(panel_plot, filename = "06D_performance_panel-plot.tiff",
         width = 3500, height = 2500, res = 300) # more res = bigger plot zoom


# End of script =========================================================
# =======================================================================
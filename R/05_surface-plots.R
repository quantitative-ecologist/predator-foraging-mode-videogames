
###########################################################################
#### A. Chapter 1. Quadratic/correlational performance surfaces  ####
###########################################################################
# Produce plots from correlational performance analysis
# All graphs are saved directly from the offline webhost into my outputs

# I produced the final 3D figure by combining the individual plots on powerpoint****

# stop speed à -4
# stop guard à 6 stdev


# 1. Set working directory, load libraries, dataset and model =============
# =========================================================================
setwd("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/masters_project/chapter1/outputs") # personal computer onedrive UQAM Montiglio lab

library(data.table)
library(lme4)
library(Matrix)
library(arm)
library(tidyr)
library(plotly)
library(htmlwidgets)

data <- fread("02_merged-data.csv",
                        select = c("cohort", "mirrors_id", "match_id", "map_name", "sum_bloodpoints",
                                   "Zspeed", "Zprox_mid_guard", "Zspace_covered_rate",
                                   "Zsurv_speed", "Zsurv_space_covered_rate"))
data[, prop_bloodpoints := sum_bloodpoints / 32000]
data[, weight := 32000]

# loading the models to work with
load("05C_quadratic-model.rda")
load("05C_quadratic_prey-model.rda")

# Character variables to factor variables
char_as_factor <- names(data)[sapply(data, is.character)] # extract columns that are characters
data[, (char_as_factor) := lapply(.SD, as.factor), .SDcols = char_as_factor] # columns as factors
# =========================================================================
# =========================================================================





# 2. Create 3d fitness surfaces for best quadratic approximation ==========
# =========================================================================
# For all graphs
# tick font
tickfont <- list(
  ticklen = 3,
  tickwidth = 3,
  size = 14,
  color =  "black" # color of numbers
)

# title font
titlefont <- list(
  size = 15,
  color = "black"
)


# 2.1 speed and space covered -------------------------------------------------
#------------------------------------------------------------------------------
# Calculate z-values from model equation
# speed <- seq(min(data$Zspeed), max(data$Zspeed), length = 50)
speed <- seq(-4.84053374, max(data$Zspeed), length = 50) # to select values for the surface
space <- seq(min(data$Zspace_covered_rate), max(data$Zspace_covered_rate), length = 50)

z1 = outer(speed, space, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[2] * x^2) + 
                                                     (fixef(quadratic_model)[5] * x) + 
                                                     (fixef(quadratic_model)[3] * y^2) + 
                                                     (fixef(quadratic_model)[6] * y) + 
                                                     (x * y * fixef(quadratic_model)[8]))
                                               })

# Compute font and plot parameters for the 3d graph

# z axis parameters
axz1 <- list(
    range = c(0, 1), # tick par.
    nticks = 5,
    tickvals = c(0.00, 0.25, 0.50, 0.75, 1),
    tickfont = tickfont,
    ticks = "outside",
    tickcolor = "black",
    title = "Probability of gaining bloodpoints", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# x axis parameters
axx1 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    tickvals = c(-2, 0, 2, 4),
    nticks = 4,
    title = "Space covered rate", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par.
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# y axis parameters
axy1 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    tickvals = c(-4, -2, 0, 2, 4),
    nticks = 5,
    title = "Average speed", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par.
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# Plot, specify axis paremeters using scene
speed_space_plot <- plot_ly() %>%
                      add_trace(x = ~Zspace_covered_rate, 
                                y = ~Zspeed, 
                                z = ~prop_bloodpoints, 
                                data = data[Zspeed > -5], 
                                type = "scatter3d", 
                                mode = "markers",
                                marker = list(size = 2, 
                                              color = "#571A44",
                                              opacity = 0.3),
                                showlegend = FALSE) %>%
                      add_surface(z = z1[,1:40], # 1:40 is a good starting point
                                  x = space, 
                                  y = speed,
                                  opacity = 0.8) %>%
                      layout(scene = list(zaxis = axz1, 
                                          xaxis = axx1, 
                                          yaxis = axy1,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ---




# 2.2 speed and guarding -------------------------------------------------------
#------------------------------------------------------------------------------
# Calculate z-values from model equation
#speed <- seq(min(data$Zspeed), max(data$Zspeed), length = 50)
speed <- seq(-4.84053374, max(data$Zspeed), length = 50) # to select values for the surface
guard <- seq(min(data$Zprox_mid_guard), max(data$Zprox_mid_guard), length = 50)

z2 = outer(speed, guard, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[2] * x^2) + 
                                                     (fixef(quadratic_model)[5] * x) + 
                                                     (fixef(quadratic_model)[4] * y^2) + 
                                                     (fixef(quadratic_model)[7] * y) + 
                                                     (x * y * fixef(quadratic_model)[9]))
                                               })

# z axis parameters
axz2 <- list(
    range = c(0, 1), # tick par.
    nticks = 5,
    tickvals = c(0.00, 0.25, 0.50, 0.75, 1),
    tickfont = tickfont,
    ticks = "outside",
    tickcolor = "black",
    title = "Probability of gaining bloodpoints", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# x axis parameters
axx2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 5,
    tickvals = c(0, 2, 4, 6, 8),
    title = "Prey guarding", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# y axis parameters
axy2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    tickvals = c(-6, -4, -2, 0, 2, 4),
    title = "Average speed", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par.
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# Plot, specify axis paremeters using scene
speed_guard_plot <- plot_ly() %>%
                      add_trace(x = ~Zprox_mid_guard, 
                                y = ~Zspeed, 
                                z = ~prop_bloodpoints, 
                                data = data[Zprox_mid_guard < 8 & 
                                            Zspeed > -5], 
                                type = "scatter3d", 
                                mode = "markers",
                                marker = list(size = 2, 
                                              color = "#571A44", 
                                              opacity = 0.3), # best opacity when saving
                                showlegend = FALSE) %>%
                      add_surface(z = z2[,1:32], # remove Z values where guard is too high
                                  x = guard, 
                                  y = speed, 
                                  opacity = 0.8) %>% # add opacity if I want (0.8 is good)
                      layout(scene = list(zaxis = axz2, 
                                          xaxis = axx2, 
                                          yaxis = axy2,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ---




# 2.3 space and guard ---------------------------------------------------------
#------------------------------------------------------------------------------
# Calculate z-values from model equation
#space <- seq(min(data$Zspace_covered_rate), max(data$Zspace_covered_rate), length = 50)
space <- seq(min(data$Zspace_covered_rate), 3.83371006, length = 50) # to cut the surface
guard <- seq(min(data$Zprox_mid_guard), max(data$Zprox_mid_guard), length = 50)

z3 = outer(space, guard, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[3] * x^2) + 
                                                     (fixef(quadratic_model)[6] * x) + 
                                                     (fixef(quadratic_model)[4] * y^2) + 
                                                     (fixef(quadratic_model)[7] * y) + 
                                                     (x * y * fixef(quadratic_model)[10]))
                                               })

# z axis parameters
axz3 <- list(
    range = c(0, 1), # tick par.
    nticks = 5,
    tickvals = c(0.00, 0.25, 0.50, 0.75, 1),
    tickfont = tickfont,
    ticks = "outside",
    tickcolor = "black",
    title = "Probability of gaining bloodpoints", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# x axis parameters
axx3 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 4,
    tickvals = c(0, 2, 4, 6),
    title = "Prey guarding", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# y axis parameters
axy3 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 4,
    tickvals = c(-2, 0, 2, 4),
    title = "Space covered rate", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par.
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# Plot, specify axis paremeters using scene
space_guard_plot <- plot_ly() %>%
                      add_trace(x = ~Zprox_mid_guard, 
                                y = ~Zspace_covered_rate, 
                                z = ~prop_bloodpoints, 
                                data = data[Zprox_mid_guard < 8 & 
                                            Zspace_covered_rate < 4], 
                                type = "scatter3d", 
                                mode = "markers",
                                marker = list(size = 2, 
                                              color = "#571A44", 
                                              opacity = 0.3),
                                showlegend = FALSE) %>%
                      add_surface(z = z3[,1:32], 
                                  x = guard, 
                                  y = space,
                                  opacity = 0.8) %>%
                      layout(scene = list(zaxis = axz3, 
                                          xaxis = axx3, 
                                          yaxis = axy3,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# =========================================================================
# =========================================================================





# 3. Predator-prey fitness surfaces =======================================
# =========================================================================

# 3.1 speed and speed ---------------------------------------------------------
#------------------------------------------------------------------------------
speed <- seq(-4.84053374, 3.5, length = 50) # to select values for the surface
surv_speed <- seq(-5, 3, length = 50)

fixef(quadratic_model_surv)

z2 = outer(speed, surv_speed, FUN = function(x, y) {plogis(fixef(quadratic_model_surv)[1] + 
                                                     (fixef(quadratic_model_surv)[2] * x^2) + 
                                                     (fixef(quadratic_model_surv)[7] * x) + 
                                                     (fixef(quadratic_model_surv)[5] * y^2) + 
                                                     (fixef(quadratic_model_surv)[10] * y) + 
                                                     (x * y * fixef(quadratic_model_surv)[15]))
                                               })

# z axis parameters
axz2 <- list(
    range = c(0, 1), # tick par.
    nticks = 5,
    tickvals = c(0.00, 0.25, 0.50, 0.75, 1),
    tickfont = tickfont,
    ticks = "outside",
    tickcolor = "black",
    title = "Probability of gaining bloodpoints", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# x axis parameters
axx2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 4,
    tickvals = c(-4, -2, 0, 2),
    title = "Prey speed", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# y axis parameters
axy2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 5,
    tickvals = c(-4, -2, 0, 2, 4),
    title = "Predator speed", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par.
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# Plot, specify axis paremeters using scene
speed_speed_plot <- plot_ly() %>%
                      add_trace(x = ~Zsurv_speed, 
                                y = ~Zspeed, 
                                z = ~prop_bloodpoints, 
                                data = data[Zspeed > -4.85 & 
                                            Zspeed < 3.6 &
                                            Zsurv_speed > -5.1 &
                                            Zsurv_speed < 3.1], 
                                type = "scatter3d", 
                                mode = "markers",
                                marker = list(size = 2, 
                                              color = "#571A44", 
                                              opacity = 0.3), # best opacity when saving
                                showlegend = FALSE) %>%
                      add_surface(z = z2, # remove Z values where surv speed is too high
                                  x = surv_speed, 
                                  y = speed, 
                                  opacity = 0.8) %>% # add opacity if I want (0.8 is good)
                      layout(scene = list(zaxis = axz2, 
                                          xaxis = axx2, 
                                          yaxis = axy2,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ---




# 3.2 Guard and speed ---------------------------------------------------------
#------------------------------------------------------------------------------
guard <- seq(min(data$Zprox_mid_guard), 8, length = 50) # to select values for the surface
surv_speed <- seq(-4.8, 3, length = 50)

fixef(quadratic_model_surv)

z2 = outer(guard, surv_speed, FUN = function(x, y) {plogis(fixef(quadratic_model_surv)[1] + 
                                                     (fixef(quadratic_model_surv)[4] * x^2) + 
                                                     (fixef(quadratic_model_surv)[9] * x) + 
                                                     (fixef(quadratic_model_surv)[5] * y^2) + 
                                                     (fixef(quadratic_model_surv)[10] * y) + 
                                                     (x * y * fixef(quadratic_model_surv)[16]))
                                               })

# z axis parameters
axz2 <- list(
    range = c(0, 1), # tick par.
    nticks = 5,
    tickvals = c(0.00, 0.25, 0.50, 0.75, 1),
    tickfont = tickfont,
    ticks = "outside",
    tickcolor = "black",
    title = "Probability of gaining bloodpoints", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# x axis parameters
axx2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 5,
    tickvals = c(-4, -2, 0, 2),
    title = "Prey speed", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# y axis parameters
axy2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 5,
    tickvals = c(0, 2, 4, 6, 8),
    title = "Predator guard", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par.
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# Plot, specify axis paremeters using scene
preyspeed_guard_plot <- plot_ly() %>%
                      add_trace(x = ~Zsurv_speed, 
                                y = ~Zprox_mid_guard, 
                                z = ~prop_bloodpoints, 
                                data = data[Zprox_mid_guard < 8 & 
                                            Zsurv_speed > -5.1 & 
                                            Zsurv_speed < 3.1], 
                                type = "scatter3d", 
                                mode = "markers",
                                marker = list(size = 2, 
                                              color = "#571A44", 
                                              opacity = 0.3), # best opacity when saving
                                showlegend = FALSE) %>%
                      add_surface(z = z2, # remove Z values where guard is too high
                                  x = surv_speed, 
                                  y = guard, 
                                  opacity = 0.8) %>% # add opacity if I want (0.8 is good)
                      layout(scene = list(zaxis = axz2, 
                                          xaxis = axx2, 
                                          yaxis = axy2,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ---




# 3.3 speed and space ---------------------------------------------------------
#------------------------------------------------------------------------------
speed <- seq(-4.84053374, 3.5, length = 50) # to select values for the surface
surv_space <- seq(-3, 4.5, length = 50)

fixef(quadratic_model_surv)

z2 = outer(speed, surv_space, FUN = function(x, y) {plogis(fixef(quadratic_model_surv)[1] + 
                                                     (fixef(quadratic_model_surv)[2] * x^2) + 
                                                     (fixef(quadratic_model_surv)[7] * x) + 
                                                     (fixef(quadratic_model_surv)[6] * y^2) + 
                                                     (fixef(quadratic_model_surv)[11] * y) + 
                                                     (x * y * fixef(quadratic_model_surv)[17]))
                                               })

# z axis parameters
axz2 <- list(
    range = c(0, 1), # tick par.
    nticks = 5,
    tickvals = c(0.00, 0.25, 0.50, 0.75, 1),
    tickfont = tickfont,
    ticks = "outside",
    tickcolor = "black",
    title = "Probability of gaining bloodpoints", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# x axis parameters
axx2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 4,
    tickvals = c(-4, -2, 0, 2, 4),
    title = "Prey space", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# y axis parameters
axy2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    tickvals = c(-6, -4, -2, 0, 2, 4),
    title = "Predator speed", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par.
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# Plot, specify axis paremeters using scene
speed_space_plot <- plot_ly() %>%
                      add_trace(x = ~Zsurv_space_covered_rate, 
                                y = ~Zspeed, 
                                z = ~prop_bloodpoints, 
                                data = data[Zspeed > -5 &
                                            Zspeed < 3.6 &
                                            Zsurv_space_covered_rate < 4.1 &
                                            Zsurv_space_covered_rate > -3.1], 
                                type = "scatter3d", 
                                mode = "markers",
                                marker = list(size = 2, 
                                              color = "#571A44", 
                                              opacity = 0.3), # best opacity when saving
                                showlegend = FALSE) %>%
                      add_surface(z = z2, # remove Z values where surv speed is too high
                                  x = surv_space, 
                                  y = speed, 
                                  opacity = 0.8) %>% # add opacity if I want (0.8 is good)
                      layout(scene = list(zaxis = axz2, 
                                          xaxis = axx2, 
                                          yaxis = axy2,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ---

# End of script ----------------------------------------------------------------
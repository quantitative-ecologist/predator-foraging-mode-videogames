
#########################################################################

#              Surface plots of hunting success analyses                #

#########################################################################

# Code to produce Figure 3 and 4 in Fraser Franco et al. 2021.
# Plot interactions between predator traits and predator-prey traits of the quadratic hunting success models

# All graphs are saved directly from the offline webhost into my outputs

# *I produced the final 3D figure by combining the individual plots on powerpoint*

# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec
# -----------------------------------------------------------------------



# stop speed à -4
# stop guard à 6 stdev
# space max = 5



# =======================================================================
# 1. Load libraries, datasets, and models
# =======================================================================

# Librairies
library(data.table)
library(brms)
library(plotly)
library(htmlwidgets)

# Load dataset
data <- fread("./data/merged-data.csv",
              select = c("mirrors_id", "match_id", 
                         "map_name", "hunting_success", "Zspeed", 
                         "Zprox_mid_guard", "Zspace_covered_rate",
                         "Zhook_start_time",
                         "Zsurv_speed", "Zsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Create a variable for the y axis
data[, prop_captures := hunting_success/4]

# Load quadratic model
load("./outputs/03C_hunting_success_quadratic-model.rda")

# =========================================================================
# =========================================================================

#with(data, plot(hunting_success~Zspeed))
#with(data[Zspeed < 3.9], plot(hunting_success~Zspeed)) # good
#
#
#with(data, plot(hunting_success~Zspace_covered_rate)) # good
#with(data[Zspace_covered_rate < 5], plot(hunting_success~Zspace_covered_rate)) # good
#
#with(data, plot(hunting_success~Zprox_mid_guard)) # good
#with(data[Zprox_mid_guard < 7], plot(hunting_success~Zprox_mid_guard)) # good

# =======================================================================
# 2. 3D surface plots for predator traits' correlated effect
# =======================================================================
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

# z axis parameters (same for all plots)
axz <- list(
            range = c(-0.1, 1), # tick par.
            nticks = 5,
            tickvals = c(0.00, 0.25, 0.50, 0.75, 1),
            tickfont = tickfont,
            ticks = "outside",
            tickcolor = "black",
            title = "Probability of capturing all prey", # titles par.
            titlefont = titlefont,
            backgroundcolor = "white", # cube par
            gridcolor = "darkgray",
            gridwidth = 3,
            showbackground = TRUE,
            zerolinecolor = "darkgray", # lines at 0
            linecolor = "black", # black axis line
            linewidth = 4
          )



# ----------------------------------------------------------------------
# 2.1 speed and space
# ----------------------------------------------------------------------
# Calculate z-values from model equation
# speed <- seq(min(data$Zspeed), max(data$Zspeed), length = 50)
speed <- seq(-5, 3.9, length = 50) # to select values for the surface
space <- seq(min(data$Zspace_covered_rate), 4, length = 50)

z1 = outer(speed, space, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[2] * x^2) + 
                                                     (fixef(quadratic_model)[8] * x) + 
                                                     (fixef(quadratic_model)[3] * y^2) + 
                                                     (fixef(quadratic_model)[9] * y) + 
                                                     (x * y * fixef(quadratic_model)[14]))
                                               })

# Compute font and plot parameters for the 3d graph
# x axis parameters
axx1 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    tickvals = c(-2, 0, 2, 4),
    nticks = 4,
    title = "Space", # titles par.
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
    tickvals = c(-4, -2, 0, 2),
    nticks = 4,
    title = "Speed", # titles par.
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
speed_space_plot <- plot_ly(x = ~Zspace_covered_rate, 
                            y = ~Zspeed, 
                            z = ~prop_captures, 
                            data = data[Zspeed < 3.9 & 
                                        Zspeed > -6 & 
                                        Zspace_covered_rate < 5]
                            ) %>%
                     # add_trace(x = ~Zspace_covered_rate, 
                     #           y = ~Zspeed, 
                     #           z = ~prop_captures, 
                     #           data = data[Zspeed < 3.9 & Zspeed > -6 & Zspace_covered_rate < 5], 
                     #           type = "scatter3d", 
                     #           mode = "markers")
                     #           marker = list(size = 2, 
                     #                         color = "#571A44",
                     #                         opacity = 0.3),
                    #            showlegend = FALSE) %>%
                      add_surface(z = z1, # 1:40 is a good starting point
                                  x = space, 
                                  y = speed,
                                  opacity = 0.8) %>%
                      layout(scene = list(zaxis = axz, 
                                          xaxis = axx1, 
                                          yaxis = axy1,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ----------------------------------------------------------------------




# ----------------------------------------------------------------------
# 2.2 speed and guarding
# ----------------------------------------------------------------------
# Calculate z-values from model equation
#speed <- seq(min(data$Zspeed), max(data$Zspeed), length = 50)
speed <- seq(-5, 3.9, length = 50) # to select values for the surface
guard <- seq(min(data$Zprox_mid_guard), 6, length = 50)

z2 = outer(speed, guard, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[2] * x^2) + 
                                                     (fixef(quadratic_model)[8] * x) + 
                                                     (fixef(quadratic_model)[4] * y^2) + 
                                                     (fixef(quadratic_model)[10] * y) + 
                                                     (x * y * fixef(quadratic_model)[15]))
                                               })

# x axis parameters
axx2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    tickvals = c(0, 2, 4, 6),
    nticks = 4,
    title = "Guard", # titles par.
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
    tickvals = c(-4, -2, 0, 2),
    nticks = 4,
    title = "Speed", # titles par.
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
speed_guard_plot <- plot_ly(x = ~Zprox_mid_guard, 
                            y = ~Zspeed, 
                            z = ~prop_captures, 
                            data = data[Zspeed < 3.9 & 
                                        Zspeed > -6 & 
                                        Zprox_mid_guard <= 6.8]) %>%
                    #  add_trace(x = ~Zprox_mid_guard, 
                    #            y = ~Zspeed, 
                    #            z = ~prop_captures, 
                    #            data = data[Zprox_mid_guard < 8 & 
                    #                        Zspeed > -5], 
                    #            type = "scatter3d", 
                    #            mode = "markers",
                    #            marker = list(size = 2, 
                    #                          color = "#571A44", 
                    #                          opacity = 0.3), # best opacity when saving
                    #            showlegend = FALSE) %>%
                      add_surface(z = z2[1:42,], # cut parts of predictions with no data
                                  x = guard, 
                                  y = speed, 
                                  opacity = 0.8) %>% # add opacity (0.8 is good)
                      layout(scene = list(zaxis = axz, 
                                          xaxis = axx2, 
                                          yaxis = axy2,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ----------------------------------------------------------------------




# ----------------------------------------------------------------------
# 2.3 space and guard [interaction not strong]
# ----------------------------------------------------------------------
# Calculate z-values from model equation
#space <- seq(min(data$Zspace_covered_rate), max(data$Zspace_covered_rate), length = 50)
space <- seq(min(data$Zspace_covered_rate), 4, length = 50) # to cut the surface
guard <- seq(min(data$Zprox_mid_guard), 6, length = 50)

z3 = outer(space, guard, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[3] * x^2) + 
                                                     (fixef(quadratic_model)[9] * x) + 
                                                     (fixef(quadratic_model)[4] * y^2) + 
                                                     (fixef(quadratic_model)[10] * y) + 
                                                     (x * y * fixef(quadratic_model)[17]))
                                               })

# x axis parameters
axx3 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 4,
    tickvals = c(0, 2, 4, 6),
    title = "Guard", # titles par.
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
    title = "Space", # titles par.
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
space_guard_plot <- plot_ly(x = ~Zprox_mid_guard, 
                            y = ~Zspace_covered_rate, 
                            z = ~prop_captures, 
                            data = data[Zprox_mid_guard <= 6.8 & 
                                        Zspace_covered_rate <= 4]) %>%
                    #  add_trace(x = ~Zprox_mid_guard, 
                    #            y = ~Zspace_covered_rate, 
                    #            z = ~prop_captures, 
                    #            data = data[Zprox_mid_guard <= 6.8 & 
                    #                        Zspace_covered_rate <= 4], 
                    #            type = "scatter3d", 
                    #            mode = "markers",
                    #            marker = list(size = 2, 
                    #                          color = "#571A44", 
                    #                          opacity = 0.3),
                    #            showlegend = FALSE) %>%
                      add_surface(z = z3, #[,1:32]
                                  x = guard, 
                                  y = space,
                                  opacity = 0.8) %>%
                      layout(scene = list(zaxis = axz, 
                                          xaxis = axx3, 
                                          yaxis = axy3,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ----------------------------------------------------------------------
# =========================================================================
# =========================================================================





# =========================================================================
# 3. 3D surface plots for predator and prey traits' correlated effect
# =========================================================================

# ---------------------------------------------------------------------- 
# 3.1 speed and prey speed
# ----------------------------------------------------------------------
speed <- seq(-5, 3.9, length = 50) # to select values for the surface
surv_speed <- seq(-5, 3, length = 50)


z2 = outer(speed, surv_speed, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[2] * x^2) + 
                                                     (fixef(quadratic_model)[8] * x) + 
                                                     (fixef(quadratic_model)[6] * y^2) + 
                                                     (fixef(quadratic_model)[12] * y) + 
                                                     (x * y * fixef(quadratic_model)[20]))
                                               })

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
    nticks = 4,
    tickvals = c(-4, -2, 0, 2),
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
speed_preyspeed_plot <- plot_ly(x = ~Zsurv_speed, 
                                y = ~Zspeed, 
                                z = ~prop_captures, 
                                data = data[Zspeed < 3.9 & 
                                            Zspeed > -6 &
                                            Zsurv_speed > -5.1 &
                                            Zsurv_speed < 3.1]) %>%
                    #  add_trace(x = ~Zsurv_speed, 
                    #            y = ~Zspeed, 
                    #            z = ~prop_captures, 
                    #            data = data[Zspeed < 3.9 & 
                    #                        Zspeed > -6 &
                    #                        Zsurv_speed > -5.1 &
                    #                        Zsurv_speed < 3.1], 
                    #            type = "scatter3d", 
                    #            mode = "markers",
                    #            marker = list(size = 2, 
                    #                          color = "#571A44", 
                    #                          opacity = 0.3), # best opacity when saving
                    #            showlegend = FALSE) %>%
                      add_surface(z = z2,
                                  x = surv_speed, 
                                  y = speed, 
                                  opacity = 0.8) %>% # add opacity if I want (0.8 is good)
                      layout(scene = list(zaxis = axz, 
                                          xaxis = axx2, 
                                          yaxis = axy2,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ----------------------------------------------------------------------



# ----------------------------------------------------------------------
# 3.2 Guard and prey speed
# ----------------------------------------------------------------------
guard <- seq(min(data$Zprox_mid_guard), 6, length = 50) # to select values for the surface
surv_speed <- seq(-4, 3, length = 50)



z2 = outer(guard, surv_speed, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[5] * x^2) + 
                                                     (fixef(quadratic_model)[11] * x) + 
                                                     (fixef(quadratic_model)[6] * y^2) + 
                                                     (fixef(quadratic_model)[12] * y) + 
                                                     (x * y * fixef(quadratic_model)[24]))
                                               })

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
    nticks = 4,
    tickvals = c(0, 2, 4, 6),
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
guard_survspeed_plot <- plot_ly(x = ~Zsurv_speed, 
                                y = ~Zprox_mid_guard, 
                                z = ~prop_captures, 
                                data = data[Zprox_mid_guard <= 6.8 & 
                                            Zsurv_speed >= -4.5 & 
                                            Zsurv_speed < 3.1]) %>%
                      #  add_trace(x = ~Zsurv_speed, 
                      #            y = ~Zprox_mid_guard, 
                      #            z = ~prop_captures, 
                      #            data = data[Zprox_mid_guard <= 6.8 & 
                      #                        Zsurv_speed >= -4.5 & 
                      #                        Zsurv_speed < 3.1], 
                      #            type = "scatter3d", 
                      #            mode = "markers",
                      #            marker = list(size = 2, 
                      #                          color = "#571A44", 
                      #                          opacity = 0.3), # best opacity when saving
                      #            showlegend = FALSE) %>%
                        add_surface(z = z2,
                                    x = surv_speed, 
                                    y = guard, 
                                    opacity = 0.8) %>% # add opacity if I want (0.8 is good)
                        layout(scene = list(zaxis = axz, 
                                            xaxis = axx2, 
                                            yaxis = axy2,
                                            showscale = FALSE, 
                                            showlegend = FALSE)) %>%
                        hide_colorbar()
# ----------------------------------------------------------------------




# ----------------------------------------------------------------------
# 3.3 speed and prey space
# ----------------------------------------------------------------------
speed <- seq(-5, 3.9, length = 50) # to select values for the surface
surv_space <- seq(-3, 4, length = 50)

z2 = outer(speed, surv_space, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[2] * x^2) + 
                                                     (fixef(quadratic_model)[8] * x) + 
                                                     (fixef(quadratic_model)[7] * y^2) + 
                                                     (fixef(quadratic_model)[13] * y) + 
                                                     (x * y * fixef(quadratic_model)[21]))
                                               })

# x axis parameters
axx2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 4,
    tickvals = c(-2, 0, 2, 4),
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
    tickvals = c(-4, -2, 0, 2),
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
speed_survspace_plot <- plot_ly(x = ~Zsurv_space_covered_rate, 
                                y = ~Zspeed, 
                                z = ~prop_captures, 
                                data = data[Zspeed < 3.9 & 
                                            Zspeed > -6 &
                                            Zsurv_space_covered_rate < 4.5 &
                                            Zsurv_space_covered_rate > -3.5]) %>%
                    #  add_trace(x = ~Zsurv_space_covered_rate, 
                    #            y = ~Zspeed, 
                    #            z = ~prop_captures, 
                    #            data = data[Zspeed < 3.9 & 
                    #                        Zspeed > -6 &
                    #                        Zsurv_space_covered_rate < 4.5 &
                    #                        Zsurv_space_covered_rate > -3.5], 
                    #            type = "scatter3d", 
                    #            mode = "markers",
                    #            marker = list(size = 2, 
                    #                          color = "#571A44", 
                    #                          opacity = 0.3), # best opacity when saving
                    #            showlegend = FALSE) %>%
                      add_surface(z = z2, # remove Z values where surv speed is too high
                                  x = surv_space, 
                                  y = speed, 
                                  opacity = 0.8) %>% # add opacity if I want (0.8 is good)
                      layout(scene = list(zaxis = axz, 
                                          xaxis = axx2, 
                                          yaxis = axy2,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ----------------------------------------------------------------------




# ----------------------------------------------------------------------
# 3.4 guarding and prey space
# ----------------------------------------------------------------------
guard <- seq(min(data$Zprox_mid_guard), 6, length = 50) # to select values for the surface
surv_space <- seq(-3, 4, length = 50)

z2 = outer(guard, surv_space, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[5] * x^2) + 
                                                     (fixef(quadratic_model)[11] * x) + 
                                                     (fixef(quadratic_model)[7] * y^2) + 
                                                     (fixef(quadratic_model)[13] * y) + 
                                                     (x * y * fixef(quadratic_model)[21]))
                                               })

# x axis parameters
axx2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 4,
    tickvals = c(-2, 0, 2, 4),
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
    nticks = 4,
    tickvals = c(0, 2, 4, 6),
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
guard_preyspace_plot <- plot_ly(x = ~Zsurv_space_covered_rate, 
                                y = ~Zprox_mid_guard, 
                                z = ~prop_captures, 
                                data = data[Zprox_mid_guard <= 6.8 & 
                                            Zsurv_space_covered_rate < 4.5 &
                                            Zsurv_space_covered_rate > -3.5]) %>%
                    #  add_trace(x = ~Zsurv_space_covered_rate, 
                    #            y = ~Zprox_mid_guard, 
                    #            z = ~prop_captures, 
                    #            data = data[Zprox_mid_guard <= 6.8 & 
                    #                        Zsurv_space_covered_rate < 4.5 &
                    #                        Zsurv_space_covered_rate > -3.5], 
                    #            type = "scatter3d", 
                    #            mode = "markers",
                    #            marker = list(size = 2, 
                    #                          color = "#571A44", 
                    #                          opacity = 0.3), # best opacity when saving
                    #            showlegend = FALSE) %>%
                      add_surface(z = z2, # remove Z values where surv speed is too high
                                  x = surv_space, 
                                  y = guard, 
                                  opacity = 0.8) %>% # add opacity if I want (0.8 is good)
                      layout(scene = list(zaxis = axz, 
                                          xaxis = axx2, 
                                          yaxis = axy2,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ----------------------------------------------------------------------




# ----------------------------------------------------------------------
# 3.5 space and prey space
# ----------------------------------------------------------------------
space <- seq(min(data$Zspace_covered_rate), 4, length = 50) # to cut the surface
surv_space <- seq(-3, 4, length = 50)

z2 = outer(space, surv_space, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[3] * x^2) + 
                                                     (fixef(quadratic_model)[9] * x) + 
                                                     (fixef(quadratic_model)[7] * y^2) + 
                                                     (fixef(quadratic_model)[13] * y) + 
                                                     (x * y * fixef(quadratic_model)[23]))
                                               })

# x axis parameters
axx2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 4,
    tickvals = c(-2, 0, 2, 4),
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
    nticks = 4,
    tickvals = c(-2, 0, 2, 4),
    title = "Predator space", # titles par.
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
space_survspace_plot <- plot_ly(x = ~Zsurv_space_covered_rate, 
                                y = ~Zspace_covered_rate, 
                                z = ~prop_captures, 
                                data = data[Zspace_covered_rate <= 4 & 
                                            Zsurv_space_covered_rate < 4.5 &
                                            Zsurv_space_covered_rate > -3.5]) %>%
                    #  add_trace(x = ~Zsurv_space_covered_rate, 
                    #            y = ~Zspace_covered_rate, 
                    #            z = ~prop_captures, 
                    #            data = data[Zspace_covered_rate <= 4 & 
                    #                        Zsurv_space_covered_rate < 4.5 &
                    #                        Zsurv_space_covered_rate > -3.5], 
                    #            type = "scatter3d", 
                    #            mode = "markers",
                    #            marker = list(size = 2, 
                    #                          color = "#571A44", 
                    #                          opacity = 0.3), # best opacity when saving
                    #            showlegend = FALSE) %>%
                      add_surface(z = z2, # remove Z values where surv speed is too high
                                  x = surv_space, 
                                  y = space, 
                                  opacity = 0.8) %>% # add opacity if I want (0.8 is good)
                      layout(scene = list(zaxis = axz, 
                                          xaxis = axx2, 
                                          yaxis = axy2,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ----------------------------------------------------------------------





# ----------------------------------------------------------------------
# 3.5 space and prey speed
# ----------------------------------------------------------------------
space <- seq(min(data$Zspace_covered_rate), 4, length = 50) # to cut the surface
surv_speed <- seq(-4, 3, length = 50)

z2 = outer(space, surv_speed, FUN = function(x, y) {plogis(fixef(quadratic_model)[1] + 
                                                     (fixef(quadratic_model)[3] * x^2) + 
                                                     (fixef(quadratic_model)[9] * x) + 
                                                     (fixef(quadratic_model)[6] * y^2) + 
                                                     (fixef(quadratic_model)[12] * y) + 
                                                     (x * y * fixef(quadratic_model)[22]))
                                               })

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
    nticks = 4,
    tickvals = c(-2, 0, 2, 4),
    title = "Predator space", # titles par.
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
space_survspeed_plot <- plot_ly(x = ~Zsurv_speed, 
                                y = ~Zspace_covered_rate, 
                                z = ~prop_captures, 
                                data = data[Zspace_covered_rate <= 4 & 
                                            Zsurv_speed >= -4.5 & 
                                            Zsurv_speed < 3.1]) %>%
                    #  add_trace(x = ~Zsurv_space_covered_rate, 
                    #            y = ~Zspace_covered_rate, 
                    #            z = ~prop_captures, 
                    #            data = data[Zspace_covered_rate <= 4 & 
                    #                        Zsurv_space_covered_rate < 4.5 &
                    #                        Zsurv_space_covered_rate > -3.5], 
                    #            type = "scatter3d", 
                    #            mode = "markers",
                    #            marker = list(size = 2, 
                    #                          color = "#571A44", 
                    #                          opacity = 0.3), # best opacity when saving
                    #            showlegend = FALSE) %>%
                      add_surface(z = z2, # remove Z values where surv speed is too high
                                  x = surv_speed, 
                                  y = space, 
                                  opacity = 0.8) %>% # add opacity if I want (0.8 is good)
                      layout(scene = list(zaxis = axz, 
                                          xaxis = axx2, 
                                          yaxis = axy2,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()
# ----------------------------------------------------------------------


# predator-prey
speed_survspace_plot
speed_preyspeed_plot
space_survspeed_plot
space_survspace_plot
guard_preyspace_plot
guard_survspeed_plot
# End of script ========================================================
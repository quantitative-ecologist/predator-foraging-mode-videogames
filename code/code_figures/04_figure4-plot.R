# ==========================================================================

#                Surface plots of hunting success analyses                 #

# ==========================================================================

# Code to produce Figure 4
# Plot interactions between predator 
# and prey traits of the quadratic hunting success model 2

# All graphs are saved directly from the offline webhost into my outputs

# *I produced the final 3D figure by combining the individual plots on powerpoint*
# -------------------------------------------------------------------------



# stop speed at -4
# stop guard at 6 stdev
# space max = 5



# ==========================================================================
# 1. Load libraries, datasets, and models
# ==========================================================================


# Librairies ---------------------------------------------------------------

library(data.table)
library(brms)
library(plotly)
library(htmlwidgets)


# Import the data ----------------------------------------------------------

data <- fread("./data/merged-data2021.csv",
              select = c("hunting_success",
                         "game_duration", "speed",
                         "space_covered_rate",
                         "prox_mid_PreyGuarding",
                         "hook_start_time",
                         "prey_avg_speed",
                         "prey_avg_space_covered_rate"))

# Create a variable for the y axis
data[, prop_captures := hunting_success/4]

# Load quadratic model
model <- readRDS("./outputs/models/03C_hunting_success_quadratic-model2.rds")

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Prepare the variables
# ==========================================================================


# Transform ----------------------------------------------------------------

data[, ":=" (prox_mid_PreyGuarding = log(prox_mid_PreyGuarding + 1),
             hook_start_time = log(hook_start_time + 1),
             game_duration = sqrt(game_duration))]



# Standardise the variables (Z-scores) -------------------------------------

standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / 
                              sd(x, na.rm = TRUE)}

data[, c("Zgame_duration", "Zspeed",
         "Zspace_covered_rate", "Zprox_mid_PreyGuarding",
         "Zhook_start_time", "Zprey_avg_speed",
         "Zprey_avg_space_covered_rate") :=
                lapply(.SD, standardize), 
                .SDcols = c(2:8)]

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 3. 3D surface plot parameters
# ==========================================================================


# Fonts --------------------------------------------------------------------

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


# z axis parameters --------------------------------------------------------

ax_z <- list(
             range = c(-0.1, 1), # tick par.
             nticks = 5,
             tickvals = c(0.00, 0.25, 0.50, 0.75, 1),
             tickfont = tickfont,
             ticks = "outside",
             tickcolor = "black",
             title = "Hunting success", # titles par.
             titlefont = titlefont,
             backgroundcolor = "white", # cube parameters
             gridcolor = "darkgray",
             gridwidth = 3,
             showbackground = TRUE,
             zerolinecolor = "darkgray", # lines at 0
             linecolor = "black", # black axis line
             linewidth = 4
           )

# =========================================================================
# =========================================================================





# =========================================================================
# 3. 3D surface plots
# =========================================================================


# Predator speed and prey speed -------------------------------------------

# This interaction is not significant ***

 # Select values for the surface
#speed <- seq(min(data$Zspeed), max(data$Zspeed))
speed <- seq(-4.3257693, max(data$Zspeed), length.out = 10)
prey_speed <- seq(min(data$Zprey_avg_speed, na.rm = TRUE),
                  max(data$Zprey_avg_speed, na.rm = TRUE),
                  length.out = 10)

# Compute the z axis values
z1 <- outer(speed, 
            prey_speed, 
            FUN = function(x, y) {plogis(fixef(model)[1] + 
                                        (fixef(model)[2] * x^2) + 
                                        (fixef(model)[8] * x) + 
                                        (fixef(model)[6] * y^2) + 
                                        (fixef(model)[12] * y) + 
                                        (x * y * fixef(model)[21]))
                                   }
             )

# x axis parameters
ax_x1 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 5,
    tickvals = c(-6, -3, 0, 3, 5),
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
ax_y1 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 5,
    tickvals = c(-3, -1, 0, 1, 3),
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
speed_preyspeed_plot <- plot_ly(x = ~Zprey_avg_speed, 
                                y = ~Zspeed, 
                                z = ~prop_captures, 
                                data = data) %>%
                    #  add_trace(x = ~Zprey_avg_speed, 
                    #            y = ~Zspeed, 
                    #            z = ~prop_captures, 
                    #            data = data[Zspeed > -4], 
                    #            type = "scatter3d", 
                    #            mode = "markers",
                    #            marker = list(size = 2, 
                    #                          color = "#571A44",
                                # best opacity when saving 
                    #                          opacity = 0.3),
                    #            showlegend = FALSE) %>%
                      add_surface(z = z1,
                                  x = prey_speed, 
                                  y = speed,
                                  # add opacity if I want
                                  opacity = 0.8) %>%
                      layout(scene = list(zaxis = ax_z, 
                                          xaxis = ax_x1, 
                                          yaxis = ax_y1,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>%
                      hide_colorbar()



# Predator guard and prey speed -------------------------------------------

# This interaction is significant ***

 # Select values for the surface
#guard <- seq(min(data$Zprox_mid_PreyGuarding, na.rm = TRUE),
#             max(data$Zprox_mid_PreyGuarding, na.rm = TRUE))

guard <- seq(-1,
             max(data$Zprox_mid_PreyGuarding, na.rm = TRUE))

prey_speed <- seq(min(data$Zprey_avg_speed, na.rm = TRUE),
                  max(data$Zprey_avg_speed, na.rm = TRUE))

# Compute the z axis values
z2 <- outer(guard, 
            prey_speed,
            FUN = function(x, y) {plogis(fixef(model)[1] + 
                                         (fixef(model)[4] * x^2) + 
                                         (fixef(model)[10] * x) + 
                                         (fixef(model)[6] * y^2) + 
                                         (fixef(model)[12] * y) + 
                                         (x * y * fixef(model)[25]))
                                   }
             )

# x axis parameters
ax_x2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 4,
    tickvals = c(-6, -3, 0, 3),
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
ax_y2 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 4,
    tickvals = c(-1, 0, 1, 2),
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
                                y = ~Zprox_mid_PreyGuarding, 
                                z = ~prop_captures, 
                                data = data[Zprox_mid_PreyGuarding >=-1]) %>%
                        add_surface(z = z2,
                                    x = prey_speed, 
                                    y = guard, 
                                    opacity = 0.8) %>%
                        layout(scene = list(zaxis = ax_z, 
                                            xaxis = ax_x2, 
                                            yaxis = ax_y2,
                                            showscale = FALSE, 
                                            showlegend = FALSE)) %>%
                        hide_colorbar()



# Predator speed and prey space -------------------------------------------

# This interaction is significant ***

# Select values for the surface
speed <- seq(-4, max(data$Zspeed), length = 50)
prey_space <- seq(-3, 4, length = 50)
prey_space <- seq(min(data$Zprey_avg_space_covered_rate, na.rm = TRUE),
                  max(data$Zprey_avg_space_covered_rate, na.rm = TRUE))

z3 = outer(speed, 
           prey_space,
           FUN = function(x, y) {plogis(fixef(model)[1] + 
                                       (fixef(model)[3] * x^2) + 
                                       (fixef(model)[9] * x) + 
                                       (fixef(model)[7] * y^2) + 
                                       (fixef(model)[13] * y) + 
                                       (x * y * fixef(model)[22]))
                                 }
            )

# x axis parameters
ax_x3 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    nticks = 4,
    tickvals = c(-3, 0, 3, 6),
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
ax_y3 <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    tickvals = c(-2, 0, 2, 4),
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
speed_survspace_plot <- plot_ly(x = ~Zprey_avg_space_covered_rate, 
                                y = ~Zspeed, 
                                z = ~prop_captures, 
                                data = data) %>%
                      add_surface(z = z3,
                                  x = prey_space, 
                                  y = speed, 
                                  opacity = 0.8) %>% # add opacity if I want (0.8 is good)
                      layout(scene = list(zaxis = ax_z, 
                                          xaxis = ax_x3, 
                                          yaxis = ax_y3,
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
# End of script ========================================================
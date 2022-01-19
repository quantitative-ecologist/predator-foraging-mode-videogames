# ==========================================================================

#                Surface plots of hunting success analyses                 #

# ==========================================================================

# Code to produce Figure 4
# Plot interactions between predator 
# and prey traits of the quadratic hunting success model 2

# All graphs are saved directly from the offline webhost into my outputs

# *I produced the final 3D figure by combining the individual plots on powerpoint*
# -------------------------------------------------------------------------





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





# =========================================================================
# 3. Contour plots for each predator-prey behavioral interactions
# =========================================================================


# Predator speed and prey speed -------------------------------------------

# This interaction is not significant ***

 # Select values for the surface
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

# Plot
plot1 <- plot_ly(
       x = ~prey_speed, 
       y = ~speed, 
       z = ~z1,
       type = "contour",
       coloraxis = "coloraxis",
       autocontour = F,
       contours = list(start = 0,
                       end = 1,
                       size = 0.1)
         ) %>%
         layout(xaxis = list(title = "Prey speed"),
                yaxis = list(nticks = 5,
                             tickvals = c(-4, -2, 0, 2, 4),
                             title = "Predator speed")
         )



# Predator speed and prey space -------------------------------------------

# This interaction is significant ***

# Select values for the surface
speed <- seq(-4, max(data$Zspeed), length.out = 10)

prey_space <- seq(min(data$Zprey_avg_space_covered_rate, na.rm = TRUE),
                  3,
                  length.out = 10)

z2 = outer(speed, 
           prey_space,
           FUN = function(x, y) {plogis(fixef(model)[1] + 
                                       (fixef(model)[2] * x^2) + 
                                       (fixef(model)[8] * x) + 
                                       (fixef(model)[7] * y^2) + 
                                       (fixef(model)[13] * y) + 
                                       (x * y * fixef(model)[22]))
                                 }
            )

# Plot, specify axis paremeters using scene
plot2 <- plot_ly(x = ~prey_space, 
                 y = ~speed, 
                 z = ~z2,
                 type = "contour",
          coloraxis = "coloraxis",
          autocontour = F,
          contours = list(start = 0,
                          end = 1,
                          size = 0.1)) %>%
layout(xaxis = list(title = "Prey space"),
       yaxis = list(nticks = 5,
                    tickvals = c(-4, -2, 0, 2, 4),
                    title = "Predator speed"))



# Predator space and prey speed -------------------------------------------

# to select values for the surface
space <- seq(min(data$Zspace_covered_rate), 
             max(data$Zspace_covered_rate),
             length.out = 10)

prey_speed <- seq(min(data$Zprey_avg_speed, na.rm = TRUE),
                  max(data$Zprey_avg_speed, na.rm = TRUE),
                  length.out = 10)

z3 = outer(space, 
           prey_speed,
           FUN = function(x, y) {plogis(fixef(model)[1] + 
                                       (fixef(model)[3] * x^2) + 
                                       (fixef(model)[9] * x) + 
                                       (fixef(model)[6] * y^2) + 
                                       (fixef(model)[12] * y) + 
                                       (x * y * fixef(model)[23]))
                                 })


# Plot, specify axis paremeters using scene
plot3 <- plot_ly(x = ~prey_speed, 
                 y = ~space, 
                 z = ~z3,
                 type = "contour",
          coloraxis = "coloraxis",
          autocontour = F,
          contours = list(start = 0,
                          end = 1,
                          size = 0.1)) %>%
layout(xaxis = list(title = "Prey speed"),
       yaxis = list(nticks = 5,
                    tickvals = c(-2, 0, 2, 4, 6),
                    title = "Predator space"))



# Predator space and prey space -------------------------------------------

# to select values for the surface
space <- seq(min(data$Zspace_covered_rate), 
             max(data$Zspace_covered_rate),
             length.out = 10)

prey_space <- seq(min(data$Zprey_avg_space_covered_rate, na.rm = TRUE),
                  3,
                  length.out = 10)

z4 = outer(space,
           prey_space,
           FUN = function(x, y) {plogis(fixef(model)[1] + 
                                       (fixef(model)[3] * x^2) + 
                                       (fixef(model)[9] * x) + 
                                       (fixef(model)[7] * y^2) + 
                                       (fixef(model)[13] * y) + 
                                       (x * y * fixef(model)[24]))
                                 }
            )

# Plot, specify axis paremeters using scene
plot4 <- plot_ly(x = ~prey_space, 
                 y = ~space, 
                 z = ~z4,
                 type = "contour",
          coloraxis = "coloraxis",
          autocontour = F,
          contours = list(start = 0,
                          end = 1,
                          size = 0.1)) %>%
layout(xaxis = list(title = "Prey space"),
       yaxis = list(nticks = 5,
                    tickvals = c(-4, -2, 0, 2, 4),
                    title = "Predator space"))



# Predator guard and prey speed -------------------------------------------

# This interaction is significant ***

 # Select values for the surface
#guard <- seq(min(data$Zprox_mid_PreyGuarding, na.rm = TRUE),
#             max(data$Zprox_mid_PreyGuarding, na.rm = TRUE))

guard <- seq(-1,
             max(data$Zprox_mid_PreyGuarding, na.rm = TRUE),
             length.out = 10)

prey_speed <- seq(min(data$Zprey_avg_speed, na.rm = TRUE),
                  max(data$Zprey_avg_speed, na.rm = TRUE),
                  length.out = 10)

# Compute the z axis values
z5 <- outer(guard, 
            prey_speed,
            FUN = function(x, y) {plogis(fixef(model)[1] + 
                                         (fixef(model)[4] * x^2) + 
                                         (fixef(model)[10] * x) + 
                                         (fixef(model)[6] * y^2) + 
                                         (fixef(model)[12] * y) + 
                                         (x * y * fixef(model)[25]))
                                   }
             )

# Plot, specify axis paremeters using scene
plot5 <- plot_ly(x = ~prey_speed, 
                 y = ~guard, 
                 z = ~z5,
                 type = "contour",
          coloraxis = "coloraxis",
          autocontour = F,
          contours = list(start = 0,
                          end = 1,
                          size = 0.1))



# Predator guard and prey space -------------------------------------------

# this interaction is significant ***

# to select values for the surface
guard <- seq(-1,
             max(data$Zprox_mid_PreyGuarding, na.rm = TRUE),
             length.out = 10)

prey_space <- seq(min(data$Zprey_avg_space_covered_rate, na.rm = TRUE),
                  3,
                  length.out = 10)

z6 = outer(guard,
           prey_space,
           FUN = function(x, y) {plogis(fixef(model)[1] + 
                                        (fixef(model)[4] * x^2) + 
                                        (fixef(model)[10] * x) + 
                                        (fixef(model)[7] * y^2) + 
                                        (fixef(model)[13] * y) + 
                                        (x * y * fixef(model)[26]))
                                  }
            )

# Plot, specify axis paremeters using scene
plot6 <- plot_ly(x = ~prey_space, 
                 y = ~guard, 
                 z = ~z6,
                 type = "contour",
          coloraxis = "coloraxis",
          autocontour = F,
          contours = list(start = 0,
                          end = 1,
                          size = 0.1))

# =========================================================================
# =========================================================================





# =========================================================================
# Create the subplot for the figure
# =========================================================================

fig <- subplot(plot1, plot2, plot3,
               plot4, plot5, plot6,
               nrows = 2) %>% 
       layout(coloraxis = list(colorscale = 'Viridis'))



plot_ly(x = ~prey_space, 
                 y = ~guard, 
                 z = ~z6,
                 type = "contour",
          coloraxis = "coloraxis",
          autocontour = F,
          contours = list(start = 0,
                          end = 1,
                          size = 0.1)) %>%
layout(yaxis = list(nticks = 5,
                   # tickvals = c(-1, -0, 1, 2, 3),
                    title = "Predator guard"))

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


# =========================================================================
# =========================================================================
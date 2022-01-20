# ==========================================================================

#                Surface plots of hunting success analyses                 #

# ==========================================================================

# Code to produce Figure S2
# Plot interactions of predator trait interactions 
# of the quadratic hunting success model 2

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
library(export)


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

# Load quadratic model (takes couple seconds)
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
# 3. Contour plots for each predator behavioral interactions
# =========================================================================


# Predator speed and space ------------------------------------------------

 # Select values for the surface
speed <- seq(-4.3257693, max(data$Zspeed), length.out = 10)

space <- seq(min(data$Zspace_covered_rate), 
             max(data$Zspace_covered_rate),
             length.out = 10)

# Compute the z axis values
z1 <- outer(speed, 
            space, 
            FUN = function(x, y) {plogis(fixef(model)[1] + 
                                        (fixef(model)[2] * x^2) + 
                                        (fixef(model)[8] * x) + 
                                        (fixef(model)[3] * y^2) + 
                                        (fixef(model)[9] * y) + 
                                        (x * y * fixef(model)[15]))
                  }
            )

# Plot
plot1 <- plot_ly(x = ~space, 
                 y = ~speed, 
                 z = ~z1,
                 type = "contour",
                 coloraxis = "coloraxis",
                 autocontour = F,
                 contours = list(start = 0,
                                 end = 1,
                                 size = 0.1)) %>%
         
         layout(xaxis = list(title = "Predator space"),
                yaxis = list(nticks = 5,
                             tickvals = c(-4, -2, 0, 2, 4),
                             title = "Predator speed"))



# Predator speed and guard ------------------------------------------------

# Select values for the surface
speed <- seq(-4, max(data$Zspeed), length.out = 10)

guard <- seq(-1,
             max(data$Zprox_mid_PreyGuarding, na.rm = TRUE),
             length.out = 10)

z2 = outer(speed, 
           guard,
           FUN = function(x, y) {plogis(fixef(model)[1] + 
                                       (fixef(model)[2] * x^2) + 
                                       (fixef(model)[8] * x) + 
                                       (fixef(model)[4] * y^2) + 
                                       (fixef(model)[10] * y) + 
                                       (x * y * fixef(model)[16]))
                 }
            )

# Plot
plot2 <- plot_ly(x = ~guard, 
                 y = ~speed, 
                 z = ~z2,
                 type = "contour",
                 coloraxis = "coloraxis",
                 autocontour = F,
                 contours = list(start = 0,
                                 end = 1,
                                 size = 0.1)) %>%

          layout(xaxis = list(#nticks = ,
                              #tickvals = c(),
                              title = "Predator guard"),
                 yaxis = list(nticks = 5,
                              tickvals = c(-4, -2, 0, 2, 4),
                              title = "Predator speed"))




# Predator speed and hook ------------------------------------------------

# Select values for the surface
speed <- seq(-4, max(data$Zspeed), length.out = 10)

hook <- seq(min(data$Zhook_start_time, na.rm = TRUE),
            max(data$Zprox_mid_PreyGuarding, na.rm = TRUE),
            length.out = 10)

z3 = outer(speed, 
           hook,
           FUN = function(x, y) {plogis(fixef(model)[1] + 
                                       (fixef(model)[2] * x^2) + 
                                       (fixef(model)[8] * x) + 
                                       (fixef(model)[5] * y^2) + 
                                       (fixef(model)[11] * y) + 
                                       (x * y * fixef(model)[17]))
                 }
            )

# Plot
plot3 <- plot_ly(x = ~hook, 
                 y = ~speed, 
                 z = ~z3,
                 type = "contour",
                 coloraxis = "coloraxis",
                 autocontour = F,
                 contours = list(start = 0,
                                 end = 1,
                                 size = 0.1)) %>%

          layout(xaxis = list(#nticks = 6,
                              #tickvals = c(-3, -2, -1, 0, 1, 2),
                              title = "Predator hook"),
                 yaxis = list(nticks = 5,
                              tickvals = c(-4, -2, 0, 2, 4),
                              title = "Predator speed"))



# Predator space and guard ------------------------------------------------

# to select values for the surface
space <- seq(min(data$Zspace_covered_rate), 
             max(data$Zspace_covered_rate),
             length.out = 10)

guard <- seq(-1,
             max(data$Zprox_mid_PreyGuarding, na.rm = TRUE),
             length.out = 10)

z4 = outer(space, 
           guard,
           FUN = function(x, y) {plogis(fixef(model)[1] + 
                                       (fixef(model)[3] * x^2) + 
                                       (fixef(model)[9] * x) + 
                                       (fixef(model)[4] * y^2) + 
                                       (fixef(model)[10] * y) + 
                                       (x * y * fixef(model)[18]))
                 }
           )

# Plot
plot4 <- plot_ly(x = ~guard, 
                 y = ~space, 
                 z = ~z4,
                 type = "contour",
                 coloraxis = "coloraxis",
                 autocontour = F,
                 contours = list(start = 0,
                                 end = 1,
                                 size = 0.1)) %>%

         layout(xaxis = list(#nticks = ,
                             #tickvals = c(),
                             title = "Predator guard"),
                yaxis = list(nticks = 5,
                             tickvals = c(-2, 0, 2, 4, 6),
                             title = "Predator space"))



# Predator space and hook -------------------------------------------------

# to select values for the surface
space <- seq(min(data$Zspace_covered_rate), 
             max(data$Zspace_covered_rate),
             length.out = 10)

hook <- seq(min(data$Zhook_start_time, na.rm = TRUE),
            max(data$Zprox_mid_PreyGuarding, na.rm = TRUE),
            length.out = 10)

z5 = outer(space, 
           hook,
           FUN = function(x, y) {plogis(fixef(model)[1] + 
                                       (fixef(model)[3] * x^2) + 
                                       (fixef(model)[9] * x) + 
                                       (fixef(model)[5] * y^2) + 
                                       (fixef(model)[11] * y) + 
                                       (x * y * fixef(model)[19]))
                 }
           )


# Plot
plot5 <- plot_ly(x = ~hook, 
                 y = ~space, 
                 z = ~z5,
                 type = "contour",
                 coloraxis = "coloraxis",
                 autocontour = F,
                 contours = list(start = 0,
                                 end = 1,
                                 size = 0.1)) %>%

         layout(xaxis = list(#nticks = 5,
                             #tickvals = c(-6, -4, -2, 0, 2),
                             title = "Predator hook"),
                yaxis = list(nticks = 5,
                             tickvals = c(-2, 0, 2, 4, 6),
                             title = "Predator space"))



# Predator guard and hook -------------------------------------------------

# to select values for the surface
guard <- seq(-1,
             max(data$Zprox_mid_PreyGuarding, na.rm = TRUE),
             length.out = 10)

hook <- seq(min(data$Zhook_start_time, na.rm = TRUE),
            max(data$Zprox_mid_PreyGuarding, na.rm = TRUE),
            length.out = 10)

z6 = outer(guard, 
           hook,
           FUN = function(x, y) {plogis(fixef(model)[1] + 
                                       (fixef(model)[4] * x^2) + 
                                       (fixef(model)[10] * x) + 
                                       (fixef(model)[5] * y^2) + 
                                       (fixef(model)[11] * y) + 
                                       (x * y * fixef(model)[20]))
                 }
           )


# Plot
plot6 <- plot_ly(x = ~guard, 
                 y = ~hook, 
                 z = ~z6,
                 type = "contour",
                 coloraxis = "coloraxis",
                 autocontour = F,
                 contours = list(start = 0,
                                 end = 1,
                                 size = 0.1)) %>%

         layout(xaxis = list(#nticks = ,
                             #tickvals = c(),
                             title = "Predator guard"),
                yaxis = list(#nticks = 5,
                             #tickvals = c(-2, 0, 2, 4, 6),
                             title = "Predator hook"))

# =========================================================================
# =========================================================================





# =========================================================================
# Create the subplot for the figure
# =========================================================================


fig <- subplot(plot1, plot2, plot3,
               plot4, plot5, plot6,
               nrows = 2) %>%
               #titleY = TRUE,
               #titleX = TRUE) %>%

       layout(coloraxis = list(colorscale = 'Viridis'))

# rest done with capture output tool and powerpoint because
# of the bug with plotly::subplot()

# =========================================================================
# =========================================================================
# ==============================================================================

#                               Data Exploration                               #

# ==============================================================================





# =======================================================================
# 1. load libraries, and export dataset
# =======================================================================


# Activate project environment ------------------------------------------

renv::activate()



# Load librairies -------------------------------------------------------
library(data.table)
library(ggplot2)
library(lattice)



# Import the merged dataset ---------------------------------------------
data <- fread("./data/FraserFrancoetal2022-data.csv",
              stringsAsFactors = TRUE)

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Descriptive statistics
# =======================================================================


# Data structure --------------------------------------------------------
str(data)



# Range of values for the variables and standard deviation --------------
data[,. (range = range(na.omit(prox_mid_PreyGuarding)),
          sd = sd(na.omit(prox_mid_PreyGuarding)))]

data[,. (range = range(speed), sd = sd(speed))]

data[,. (range = range(space_covered_rate),
          sd = sd (space_covered_rate))]

data[,. (range = range(na.omit(hook_start_time)),
          sd = sd(na.omit(hook_start_time)))]

data[,. (range = range(sum_bloodpoints),
          sd = sd(sum_bloodpoints))]


# Verify the amount of unique players 
# that scored a high amount of bloodpoints
length(unique(data[sum_bloodpoints > 30000]$player_id))
# 345 players scored +30000 bpts



# Summary for the variables ---------------------------------------------
summary(data$prox_mid_PreyGuarding)
summary(data$speed)
summary(data$space_covered_rate)
summary(data$hook_start_time)
summary(data$sum_bloodpoints)



# Prey behavioural distributions ----------------------------------------

png("outputs/figures/data_exploration/01_prey-distributions.png", 
     res = 300, 
     width = 3000, 
     height = 1500)

par(mar = c(4, 4, 1, 1), 
    oma = c(1, 1, 1, 1),
    mfrow = c(1, 2))

hist(data$prey_avg_speed,
     col = "darkgray", breaks = 50,
     xlab = "Prey avg. speed",
     main = "")
hist(data$prey_avg_space_covered_rate,
     col = "darkgray", breaks = 50,
     xlab = "Prey avg. rate of space covered",
     main = "")

dev.off()



# Predator behavioural distributions ------------------------------------

# Points
hist(as.numeric(data$sum_bloodpoints),
     breaks = 100,
     col = "darkgray")

# Prey guarding
hist(data$prox_mid_PreyGuarding,
     breaks = 50,
     col = "darkgray")
# sqrt
hist(sqrt(data$prox_mid_PreyGuarding),
     breaks = 50,
     col = "darkgray")
# log+1 (seems to be the best transformation)
hist(log(data$prox_mid_PreyGuarding + 1),
     breaks = 50,
     col = "darkgray")

# Rate of space covered
hist(data$space_covered_rate, 
     breaks = 50,
     col = "darkgray")

# Speed
hist(data$speed,
     breaks = 50,
     col = "darkgray")

# Latency for 1st capture
hist(data$hook_start_time,
     breaks = 50,
     col = "darkgray")
# Log seems to be the best transformation
hist(log(data$hook_start_time + 1),
     breaks = 50,
     col = "darkgray")



# Save a figure ---------------------------------------------------------
png("outputs/figures/data_exploration/01_predator-distributions.png", 
     res = 300, 
     width = 2000, 
     height = 2000)

par(mar = c(4, 4, 1, 1), 
    oma = c(1, 1, 1, 1),
    mfrow = c(2, 2))

# Speed
hist(data$speed,
     breaks = 30,
     col = "darkgray",
     main = "",
     xlab = "Predator avg. speed")
# Rate of space covered
hist(data$space_covered_rate, 
     breaks = 30,
     col = "darkgray",
     main = "",
     xlab = "Predator rate of space covered")
# Log seems to be the best transformation
hist(log(data$hook_start_time + 1),
     breaks = 30,
     col = "darkgray",
     main = "",
     xlab = "log(Latency before the 1st capture)")
# log+1 (seems to be the best transformation)
hist(log(data$prox_mid_PreyGuarding + 1),
     breaks = 30,
     col = "darkgray",
     main = "",
     xlab = "log(Prey guarding + 1)")

dev.off()

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Dotcharts to identify outliers
# =======================================================================

# Set a list of colors to be attributed to characters in the plots
#char_colors <- c("red", "green", "blue", "yellow", "purple",
#                 "orange", "pink", "cyan", "magenta", "brown",
#                 "black", "aquamarine4", "gray", "darkolivegreen3",
#                 "lightgoldenrod4")



# Prey behaviours -------------------------------------------------------

png("outputs/figures/data_exploration/01_prey-dotplot.png", 
     res = 300, 
     width = 1500, 
     height = 2000)

par(mar = c(2, 2, 2, 2), 
    oma = c(2, 2, 2, 2),
    mfrow = c(2, 1))

dotchart(data$speed, 
         main = "Prey avg. speed", 
         pch = 20)

dotchart(data$prey_avg_space_covered_rate, 
         main = "Prey avg. rate of space covered", 
         pch = 20)
dev.off()



# Predator behaviours ---------------------------------------------------

png("outputs/figures/data_exploration/01_predator-dotplot.png",
     res = 300,
     width = 3000,
     height = 2000)

par(mar = c(2, 2, 2, 2),
    oma = c(2, 2, 2, 2),
    mfrow = c(2, 2))

dotchart(data$speed,
         main = "Speed", pch = 20)
dotchart(data$space_covered_rate,
         main = "Rate of space covered", pch = 20)
dotchart(data$prox_mid_PreyGuarding,
         main = "Prey guarding", pch = 20)
dotchart(data$hook_start_time,
         main = "Latency for 1st capture", pch = 20)

dev.off()

# =======================================================================
# =======================================================================


# End of script ---------------------------------------------------------

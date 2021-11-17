# ==============================================================================

#                               Data Exploration                               #

# ==============================================================================





# =======================================================================
# 1. load libraries, and export dataset
# =======================================================================


# Load librairies -------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggpubr)
library(GGally)
library(cowplot)
library(lattice)



# Import the merged dataset ---------------------------------------------
data <- fread("./data/merged-data2021.csv",
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
# 344 players scored +30000 bpts



# Summary for the variables ---------------------------------------------
summary(data$prox_mid_PreyGuarding)
summary(data$speed)
summary(data$space_covered_rate)
summary(data$hook_start_time)
summary(data$sum_bloodpoints)



# Prey behavioural distributions ----------------------------------------

hist(data$prey_avg_speed,
     col = "darkgray", breaks = 100)
hist(data$prey_avg_space_covered_rate,
     col = "darkgray", breaks = 100)



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
hist(log(data$hook_start_time),
     breaks = 50,
     col = "darkgray")

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
tiff("outputs/figures/data_exploration/prey-dotplot.tiff", 
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
#tiff("03_predator-dotplot.tiff", res = 300, width = 3000, height = 2000)
#par(mar = c(2, 2, 2, 2), oma = c(2, 2, 2, 2), mfrow = c(3,3))
dotchart(data$sum_bloodpoints,
         color = char_colors[data$character_name], main = "Bloodpoints", pch = 16)
dotchart(data$Zspeed,
         color = char_colors[data$character_name], main = "Z Speed", pch = 16)
dotchart(data$Zspace_covered_rate,
         color = char_colors[data$character_name], main = "Z Space covered rate", pch = 16)
dotchart(data$Zprox_mid_guard,
         color = char_colors[data$character_name], main = "Z Prox-mid guard", pch = 16)
dotchart(data$Zhook_start_time,
         color = char_colors[data$character_name], main = "Z Prox-mid guard", pch = 16)
#dev.off()

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Pairplots
# =======================================================================

# Plot variable relationships
# With standardized variables
pairplot_Z <- ggpairs(data, 
                      columns = c(47:49, 56:58), 
                      columnLabels = c("Time guarding", 
                                        "Speed", 
                                        "Rate space cvd.",
                                        "Latency 1st cap.",
                                        "Prey speed",
                                        "Prey space cvd."),
                      title = "Z-scored variables",
                      diag = list(mapping = aes(color = character_name)),
                      lower = list(mapping = aes(color = character_name),
                                   continuous = wrap("points", 
                                                      alpha = 0.3, 
                                                      size = 0.4))) +
                      theme_bw() +
                      theme(panel.grid.major = element_blank())


# With sqrt variables
pairplot_sqrt <- ggpairs(data, 
                         columns = c(35:37, 44:46), 
                         columnLabels = c("Time guarding", 
                                          "Speed", 
                                          "Rate space cvd.",
                                          "Latency 1st cap.",
                                          "Prey speed",
                                          "Prey space cvd."),
                         title = "Sqrt variables",
                         diag = list(mapping = aes(color = character_name)),
                         lower = list(mapping = aes(color = character_name),
                                      continuous = wrap("points", 
                                                        alpha = 0.3, 
                                                        size = 0.4))) +
                         theme_bw() +
                         theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank())

# Save and export figures
#ggexport(pairplot_Z, filename = "03_Ztraits-pairplot.tiff",
#         width = 2200, height = 2200, res = 300) # more res = bigger plot zoom

#ggexport(pairplot_sqrt, filename = "03_sqrttraits-pairplot.tiff",
#         width = 2200, height = 2200, res = 300) # more res = bigger plot zoom

# End of script ------------------------------------------------------

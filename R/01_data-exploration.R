################################################################################

#                        A. Chapter 1. Data Exploration                        #

################################################################################





# =======================================================================
# 1. Set working directory, load libraries, and export dataset
# =======================================================================
# personal computer onedrive UQAM Montiglio lab
setwd("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/PhD_project/chapter1/outputs") 


library(data.table)
library(ggplot2)
library(ggpubr)
library(GGally)
library(cowplot)
library(lattice)

# import the merged dataset
merged_data <- fread("02_merged-data.csv")

# extract columns that are characters
char_as_factor <- names(merged_data)[sapply(merged_data, is.character)]
# columns as factors
merged_data[, (char_as_factor) := lapply(.SD, as.factor), .SDcols = char_as_factor]
# =======================================================================
# =======================================================================





# =======================================================================
# 2. Descriptive statistics
# =======================================================================
# Data structure
str(merged_data)
# Range of values for the variables and standard deviation
merged_data[,. (range = range(Zprox_guard), sd = sd(Zprox_guard))]
merged_data[,. (range = range(Zmid_guard), sd = sd(Zmid_guard))]
merged_data[,. (range = range(Zprox_mid_guard), sd = sd(Zprox_mid_guard))]
merged_data[,. (range = range(Zspeed), sd = sd(Zspeed))]
merged_data[,. (range = range(Zspace_covered_rate), sd = sd (Zspace_covered_rate))]
merged_data[,. (range = range(sum_bloodpoints), sd = sd(sum_bloodpoints))]

# Verify the amount of unique players that scored a high amount of bloodpoints (without accounting for multiplyers)
length(unique(merged_data[sum_bloodpoints > 30000]$mirrors_id)) # 344 players scored +30000 bpts

# Summary for the variables
summary(merged_data$Zprox_guard)
summary(merged_data$Zmid_guard)
summary(merged_data$Zprox_mid_guard)
summary(merged_data$Zspeed)
summary(merged_data$Zspace_covered_rate)
summary(merged_data$sum_bloodpoints)

# Investigate predator and prey behaviour distribution
# Prey
hist(merged_data$sqrtsurv_speed,
     col = "darkgray", breaks = 100)
hist(merged_data$sqrtsurv_space_covered_rate,
     col = "darkgray", breaks = 100)
hist(merged_data$Zsurv_speed,
     col = "darkgray", breaks = 100)
hist(merged_data$Zsurv_space_covered_rate,
     col = "darkgray", breaks = 100)

# Predator
hist(as.numeric(merged_data$sum_bloodpoints),
     ylim = range(0,400), breaks = 400, col = "darkgray") ?
hist(merged_data$proportion_prox_guard,
     ylim = range(0,30000), breaks = 200,
     col = "darkgray") 
hist(merged_data$proportion_mid_guard,
     ylim = range(0,8000), breaks = 200,
     col = "darkgray") 
hist(merged_data$proportion_prox_mid,
     ylim = range(0,6000), breaks = 200,
     col = "darkgray") 
hist(merged_data$speed,
     breaks = 200, col = "darkgray")
hist(merged_data$Zprox_guard,
     ylim = range(0, 10000), xlim = range(-5, 25),
     breaks = 200, col = "darkgray")
hist(merged_data$Zmid_guard,
     ylim = range(0, 2000), xlim = range(-5, 15),
     breaks = 200, col = "darkgray")
hist(merged_data$Zprox_mid_guard,
     ylim = range(0, 7000), xlim = range(-5, 15),
     breaks = 200, col = "darkgray")
hist(merged_data$Zspeed,
     ylim = range(0, 800), breaks = 500,
     col = "darkgray")
hist(merged_data$Zspace_covered_rate,
     ylim = range(0, 1500), breaks = 200,
     col = "darkgray") 
# =======================================================================
# =======================================================================





# =======================================================================
# 3. Dotcharts to identify outliers
# =======================================================================
# Set a list of colors to be attributed to characters in the plots
char_colors <- c("red", "green", "blue", "yellow", "purple",
                 "orange", "pink", "cyan", "magenta", "brown",
                 "black", "aquamarine4", "gray", "darkolivegreen3", "lightgoldenrod4")

# Prey behaviours
tiff("03_prey-dotplot.tiff", res = 300, width = 1500, height = 2000)
par(mar = c(2, 2, 2, 2), 
    oma = c(2, 2, 2, 2),
    mfrow = c(2, 1))
dotchart(merged_data$Zsurv_speed, 
         main = "Z average speed", 
         pch = 16)
dotchart(merged_data$Zsurv_space_covered_rate, 
         main = "Z space covered rate.", 
         pch = 16)
dev.off()

# Predator behaviours
tiff("03_predator-dotplot.tiff", res = 300, width = 3000, height = 2000)
par(mar = c(2, 2, 2, 2), oma = c(2, 2, 2, 2), mfrow = c(3,3))
dotchart(merged_data$sum_bloodpoints,
         color = char_colors[merged_data$character_name], main = "Bloodpoints", pch = 16)
dotchart(merged_data$Zspeed,
         color = char_colors[merged_data$character_name], main = "Z Speed", pch = 16)
dotchart(merged_data$Zspace_covered_rate,
         color = char_colors[merged_data$character_name], main = "Z Space covered rate", pch = 16)
dotchart(merged_data$Zprox_guard,
         color = char_colors[merged_data$character_name], main = "Z Prox guard", pch = 16)
dotchart(merged_data$Zmid_guard,
         color = char_colors[merged_data$character_name], main = "Z Mid guard", pch = 16)
dotchart(merged_data$Zprox_mid_guard,
         color = char_colors[merged_data$character_name], main = "Z Prox-mid guard", pch = 16)
dev.off()
# =======================================================================
# =======================================================================





# =======================================================================
# 3. Pairplots
# =======================================================================
# Plot variable relationships
# With standardized variables
pairplot_Z <- ggpairs(merged_data, 
                      columns = c(65:67, 76:77), 
                      columnLabels = c("Time guarding", 
                                        "Speed", 
                                        "Space cvd. rate",
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
pairplot_sqrt <- ggpairs(merged_data, 
                         columns = c(48:50, 59:60), 
                         columnLabels = c("Time guarding", 
                                          "Speed", 
                                          "Space cvd. rate",
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
ggexport(pairplot_Z, filename = "03_Ztraits-pairplot.tiff",
         width = 2200, height = 2200, res = 300) # more res = bigger plot zoom

ggexport(pairplot_sqrt, filename = "03_sqrttraits-pairplot.tiff",
         width = 2200, height = 2200, res = 300) # more res = bigger plot zoom

# End of script ------------------------------------------------------

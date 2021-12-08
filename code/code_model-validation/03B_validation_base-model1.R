# =======================================================================

#                    K-fold cross validation on the                     #
#                     hunting success base-model 1                      #

# =======================================================================





# =======================================================================
# 1. Load the libraries, the dataset, and the model
# =======================================================================


# Import libraries ------------------------------------------------------ 

library(data.table)
library(brms)
library(ggplot2)
library(ggpubr)



# Import the data -------------------------------------------------------

data <- fread("./data/merged-data2021.csv",
              select = c("player_id", "match_id",
                         "hunting_success", "map_name", 
                         "game_duration", "speed",
                         "space_covered_rate",
                         "prox_mid_PreyGuarding",
                         "hook_start_time"),
                         stringsAsFactors = TRUE)



# Transform --------------------------------------------------------------

# Transform the data even though it is not perfect
data[, ":=" (prox_mid_PreyGuarding = log(prox_mid_PreyGuarding + 1),
             hook_start_time = log(hook_start_time + 1),
             game_duration = sqrt(game_duration))]



# Standardise the variables (Z-scores) ----------------------------------

standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / 
                              sd(x, na.rm = TRUE)}

data[, c("Zgame_duration", "Zspeed",
         "Zspace_covered_rate", "Zprox_mid_PreyGuarding",
         "Zhook_start_time") :=
                lapply(.SD, standardize), 
                .SDcols = c(5:9)]

# Add observation-level random effect
data$obs <- 1:nrow(data)



# Load the model --------------------------------------------------------

base_model <- readRDS("./outputs/models/03B_hunting_success_base-model1.rds")
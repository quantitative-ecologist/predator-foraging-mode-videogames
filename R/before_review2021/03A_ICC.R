#########################################################################

#                   ICCs for the multivariate model                      #

#########################################################################





# =======================================================================
# 1. Load libraries, and model
# =======================================================================

# Librairies
library(data.table)
library(brms)

# Load models
load("./outputs/03A_multivariate-model1.rda")
mv_model1 <- mv_model
rm(mv_model)

load("./outputs/03A_multivariate-model2.rda")
mv_model2 <- mv_model
rm(mv_model)

load("./outputs/03A_multivariate-model3.rda")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Compute ICCs and their 95% credibility intervals
# =======================================================================

# Extract random effect standard deviations
ran_var1 <- data.table(posterior_samples(mv_model1)[,c(13:24, 43:46)])
ran_var2 <- data.table(posterior_samples(mv_model2)[,c(5:16, 35:38)])
ran_var3 <- data.table(posterior_samples(mv_model3)[,c(17:28, 47:50)])

# Compute variances for each random effect
ran_var1[, c("speedvar_char", "spacevar_char", "guardvar_char", "hookvar_char",
            "speedvar_map", "spacevar_map", "guardvar_map", "hookvar_map", 
            "speedvar_id", "spacevar_id", "guardvar_id", "hookvar_id",
            "speedvar_resid", "spacevar_resid", "guardvar_resid", "hookvar_resid") := 
          lapply(.SD, function(x) {x^2}),
            .SDcols = c(1:16)][, c(1:16) := NULL]

ran_var2[, c("speedvar_char", "spacevar_char", "guardvar_char", "hookvar_char",
            "speedvar_map", "spacevar_map", "guardvar_map", "hookvar_map", 
            "speedvar_id", "spacevar_id", "guardvar_id", "hookvar_id",
            "speedvar_resid", "spacevar_resid", "guardvar_resid", "hookvar_resid") := 
          lapply(.SD, function(x) {x^2}),
            .SDcols = c(1:16)][, c(1:16) := NULL]

ran_var3[, c("speedvar_char", "spacevar_char", "guardvar_char", "hookvar_char",
            "speedvar_map", "spacevar_map", "guardvar_map", "hookvar_map", 
            "speedvar_id", "spacevar_id", "guardvar_id", "hookvar_id",
            "speedvar_resid", "spacevar_resid", "guardvar_resid", "hookvar_resid") := 
          lapply(.SD, function(x) {x^2}),
            .SDcols = c(1:16)][, c(1:16) := NULL]

# Compute total variance            
ran_var1[, speedvar_total := rowSums(ran_var1[, c(1,5,9,13)])]
ran_var1[, spacevar_total := rowSums(ran_var1[, c(2,6,10,14)])]
ran_var1[, guardvar_total := rowSums(ran_var1[, c(3,7,11,15)])]
ran_var1[, hookvar_total := rowSums(ran_var1[, c(4,8,12,16)])]

ran_var2[, speedvar_total := rowSums(ran_var2[, c(1,5,9,13)])]
ran_var2[, spacevar_total := rowSums(ran_var2[, c(2,6,10,14)])]
ran_var2[, guardvar_total := rowSums(ran_var2[, c(3,7,11,15)])]
ran_var2[, hookvar_total := rowSums(ran_var2[, c(4,8,12,16)])]

ran_var3[, speedvar_total := rowSums(ran_var3[, c(1,5,9,13)])]
ran_var3[, spacevar_total := rowSums(ran_var3[, c(2,6,10,14)])]
ran_var3[, guardvar_total := rowSums(ran_var3[, c(3,7,11,15)])]
ran_var3[, hookvar_total := rowSums(ran_var3[, c(4,8,12,16)])]

# Calculate ICCs
# ID
ran_var1[, speedicc_id := speedvar_id / speedvar_total]
ran_var1[, spaceicc_id := spacevar_id / spacevar_total]
ran_var1[, guardicc_id := guardvar_id / guardvar_total]
ran_var1[, hookicc_id := hookvar_id / hookvar_total]

ran_var2[, speedicc_id := speedvar_id / speedvar_total]
ran_var2[, spaceicc_id := spacevar_id / spacevar_total]
ran_var2[, guardicc_id := guardvar_id / guardvar_total]
ran_var2[, hookicc_id := hookvar_id / hookvar_total]

ran_var3[, speedicc_id := speedvar_id / speedvar_total]
ran_var3[, spaceicc_id := spacevar_id / spacevar_total]
ran_var3[, guardicc_id := guardvar_id / guardvar_total]
ran_var3[, hookicc_id := hookvar_id / hookvar_total]

# map
ran_var1[, speedicc_map := speedvar_map / speedvar_total]
ran_var1[, spaceicc_map := spacevar_map / spacevar_total]
ran_var1[, guardicc_map := guardvar_map / guardvar_total]
ran_var1[, hookicc_map := hookvar_map / hookvar_total]

ran_var2[, speedicc_map := speedvar_map / speedvar_total]
ran_var2[, spaceicc_map := spacevar_map / spacevar_total]
ran_var2[, guardicc_map := guardvar_map / guardvar_total]
ran_var2[, hookicc_map := hookvar_map / hookvar_total]

ran_var3[, speedicc_map := speedvar_map / speedvar_total]
ran_var3[, spaceicc_map := spacevar_map / spacevar_total]
ran_var3[, guardicc_map := guardvar_map / guardvar_total]
ran_var3[, hookicc_map := hookvar_map / hookvar_total]

# character
ran_var1[, speedicc_char := speedvar_char / speedvar_total]
ran_var1[, spaceicc_char := spacevar_char / spacevar_total]
ran_var1[, guardicc_char := guardvar_char / guardvar_total]
ran_var1[, hookicc_char := hookvar_char / hookvar_total]

ran_var2[, speedicc_char := speedvar_char / speedvar_total]
ran_var2[, spaceicc_char := spacevar_char / spacevar_total]
ran_var2[, guardicc_char := guardvar_char / guardvar_total]
ran_var2[, hookicc_char := hookvar_char / hookvar_total]

ran_var3[, speedicc_char := speedvar_char / speedvar_total]
ran_var3[, spaceicc_char := spacevar_char / spacevar_total]
ran_var3[, guardicc_char := guardvar_char / guardvar_total]
ran_var3[, hookicc_char := hookvar_char / hookvar_total]

# residuals
ran_var1[, speedicc_resid := speedvar_resid / speedvar_total]
ran_var1[, spaceicc_resid := spacevar_resid / spacevar_total]
ran_var1[, guardicc_resid := guardvar_resid / guardvar_total]
ran_var1[, hookicc_resid := hookvar_resid / hookvar_total]

ran_var2[, speedicc_resid := speedvar_resid / speedvar_total]
ran_var2[, spaceicc_resid := spacevar_resid / spacevar_total]
ran_var2[, guardicc_resid := guardvar_resid / guardvar_total]
ran_var2[, hookicc_resid := hookvar_resid / hookvar_total]

ran_var3[, speedicc_resid := speedvar_resid / speedvar_total]
ran_var3[, spaceicc_resid := spacevar_resid / spacevar_total]
ran_var3[, guardicc_resid := guardvar_resid / guardvar_total]
ran_var3[, hookicc_resid := hookvar_resid / hookvar_total]

# Create table with mean icc and credibility interval
lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}

icc_tab1 <- data.table(group = c("speedicc_id", "spaceicc_id", "guardicc_id", "hookicc_id",
                                "speedicc_map", "spaceicc_map", "guardicc_map", "hookicc_map",
                                "speedicc_char", "spaceicc_char", "guardicc_char", "hookicc_char",
                                "speedicc_resid", "spaceicc_resid", "guardicc_resid", "hookicc_resid"),
                       mean = as.numeric(ran_var1[, lapply(.SD, mean),
                                                   .SDcols = c(21:36)]),
                       lower = as.numeric(ran_var1[, lapply(.SD, lower_interval),
                                                   .SDcols = c(21:36)]),
                       upper = as.numeric(ran_var1[, lapply(.SD, upper_interval),
                                                   .SDcols = c(21:36)])
                        )

icc_tab2 <- data.table(group = c("speedicc_id", "spaceicc_id", "guardicc_id", "hookicc_id",
                                "speedicc_map", "spaceicc_map", "guardicc_map", "hookicc_map",
                                "speedicc_char", "spaceicc_char", "guardicc_char", "hookicc_char",
                                "speedicc_resid", "spaceicc_resid", "guardicc_resid", "hookicc_resid"),
                       mean = as.numeric(ran_var2[, lapply(.SD, mean),
                                                   .SDcols = c(21:36)]),
                       lower = as.numeric(ran_var2[, lapply(.SD, lower_interval),
                                                   .SDcols = c(21:36)]),
                       upper = as.numeric(ran_var2[, lapply(.SD, upper_interval),
                                                   .SDcols = c(21:36)])
                        )

icc_tab3 <- data.table(group = c("speedicc_id", "spaceicc_id", "guardicc_id", "hookicc_id",
                                "speedicc_map", "spaceicc_map", "guardicc_map", "hookicc_map",
                                "speedicc_char", "spaceicc_char", "guardicc_char", "hookicc_char",
                                "speedicc_resid", "spaceicc_resid", "guardicc_resid", "hookicc_resid"),
                       mean = as.numeric(ran_var3[, lapply(.SD, mean),
                                                   .SDcols = c(21:36)]),
                       lower = as.numeric(ran_var3[, lapply(.SD, lower_interval),
                                                   .SDcols = c(21:36)]),
                       upper = as.numeric(ran_var3[, lapply(.SD, upper_interval),
                                                   .SDcols = c(21:36)])
                        )

icc_tab1[, ranef_variable := c(rep("id", 4),
                               rep("map", 4),
                               rep("character", 4),
                               rep("resid", 4))]

icc_tab2[, ranef_variable := c(rep("id", 4),
                               rep("map", 4),
                               rep("character", 4),
                               rep("resid", 4))]

icc_tab3[, ranef_variable := c(rep("id", 4),
                               rep("map", 4),
                               rep("character", 4),
                               rep("resid", 4))]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Save values in r object
# =======================================================================

saveRDS(icc_tab1, file = "./outputs/03A_icc-table1.rds")

saveRDS(icc_tab2, file = "./outputs/03A_icc-table2.rds")

saveRDS(icc_tab3, file = "./outputs/03A_icc-table3.rds")

# =======================================================================
# =======================================================================
############################################################################

#              Tables with 95% CIs for fixed effects and ICCs              #

############################################################################





# =======================================================================
# 1. Set working directory, load libraries, and import dataset 
# =======================================================================
setwd("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/PhD_project/chapter1/outputs") # personal computer onedrive UQAM Montiglio lab

# R packages
library(data.table)
library(lme4)

# Dataset
data <- fread("02_merged-data.csv",
                        select = c("cohort", "mirrors_id", "match_id", 
                                   "map_name", "hunting_success", "Zspeed", 
                                   "Zprox_mid_guard", "Zspace_covered_rate",
                                   "Zsurv_speed", "Zsurv_space_covered_rate"))


# Add an observation-level random effect (OLRE)
data$obs <- 1:nrow(data)

# Character variables to factor variables
# extract columns that are characters
char_as_factor <- names(data)[sapply(data, is.character)]
# columns as factors
data[, (char_as_factor) := lapply(.SD, as.factor), .SDcols = char_as_factor]

# Load simulation outputs
# a. Fixed effects
load("05D_base_fixef_boot.rda") # save output
load("05D_quad_fixef_boot.rda") # save output

# b. ICCs
load("05D_base_icc_boot.rda")
load("05D_quad_icc_boot.rda")
# ======================================================================
# ======================================================================





# =======================================================================
# 2. Compute 95% confidence intervals 
# =======================================================================
# Compute confidence intervals from the bootMer object
# a. Base model
CI_fixef_lower_base <- apply(fixef_base_boot$t, 2, function(x) as.numeric(quantile(x, probs = .025, na.rm = TRUE)))
CI_fxef_upper_base <- apply(fixef_base_boot$t, 2, function(x) as.numeric(quantile(x, probs = .975, na.rm = TRUE)))

CI_icc_lower_base <- apply(icc_base_boot$t, 2, function(x) as.numeric(quantile(x, probs = .025, na.rm = TRUE)))
CI_icc_upper_base <- apply(icc_base_boot$t, 2, function(x) as.numeric(quantile(x, probs = .975, na.rm = TRUE)))

# b. Quadratic model
CI_fixef_lower_quad <- apply(fixef_quad_boot$t, 2, function(x) as.numeric(quantile(x, probs = .025, na.rm = TRUE)))
CI_fixef_upper_quad <- apply(fixef_quad_boot$t, 2, function(x) as.numeric(quantile(x, probs = .975, na.rm = TRUE)))

CI_icc_lower_quad <- apply(icc_quad_boot$t, 2, function(x) as.numeric(quantile(x, probs = .025, na.rm = TRUE)))
CI_icc_upper_quad <- apply(icc_quad_boot$t, 2, function(x) as.numeric(quantile(x, probs = .975, na.rm = TRUE)))



# ***** Écrire les autres coefficients des proies





# Create a table to save fixed effects with their 95% confidence intervals
boot_tab <- as.data.table(fixef_base_boot$t0)
boot_tab[, model := "base_model"]

# cbind boottab quad
cbind(boot_tab, fixef_quad_boot$t0)


setnames(boot_tab, "V1", "Value")
boot_tab[, ":="(fixed_effect = c("Intercept", "I(Zspeed^2)", 
                                  "I(Zspace_covered_rate^2)", "I(Zprox_mid_guard^2)",
                                  "Zspeed", "Zspace_covered_rate",
                                  "Zprox_mid_guard", "Zspeed:Zspace_covered_rate",
                                  "Zspeed:Zprox_mid_guard", 
                                  "Zspace_covered_rate:Zprox_mid_guard"),
                    upper_CI = CI_upper_base,
                    lower_CI = CI_lower_base)]
save(boot_tab, file = "05C_fixef-table.rda")




















# Combine the outputs
ICC_tab <- rbind(ICC_boot$t)

# Compute confidence intervals for the bootMer object
CI_lower <- apply(ICC_tab, 2, function(x) as.numeric(quantile(x, probs = .025, na.rm = TRUE)))
CI_upper <- apply(ICC_tab, 2, function(x) as.numeric(quantile(x, probs = .975, na.rm = TRUE)))

# Create a table to save ICC's and their 95% confidence intervals
ICC_boot_tab <- as.data.table(ICC_boot$t0)
setnames(ICC_boot_tab, "V1", "ICC")
ICC_boot_tab[, ":="(random_effect = c("mirrors_id", "map_name"),
                    upper_CI = CI_upper,
                    lower_CI = CI_lower)]
save(ICC_boot_tab, file = "05B_ICC-table.rda")










# Compute confidence intervals from the bootMer object
CI_lower <- apply(fixef_boot$t, 2, function(x) as.numeric(quantile(x, probs = .025, na.rm = TRUE)))
CI_upper <- apply(fixef_boot$t, 2, function(x) as.numeric(quantile(x, probs = .975, na.rm = TRUE)))

# Create a table to save fixef effects and their 95% confidence intervals
fixef_boot_tab <- as.data.table(fixef_boot$t0)
setnames(fixef_boot_tab, "V1", "Value")
fixef_boot_tab[, ":="(fixef_effect = c("Intercept", "Zspeed", 
                                     "Zprox_mid_guard", "Zspace_covered_rate"),
                      upper_CI = CI_upper,
                      lower_CI = CI_lower)]
save(fixef_boot_tab, file = "05B_fixef-table.rda")






# Compute the confidence intervals using quantiles
CI_lower_base <- apply(base_boot$t, 2, function(x) as.numeric(quantile(x, probs = .025, na.rm = TRUE)))
CI_upper_base <- apply(base_boot$t, 2, function(x) as.numeric(quantile(x, probs = .975, na.rm = TRUE)))

CI_lower_quad <- apply(quad_boot$t, 2, function(x) as.numeric(quantile(x, probs = .025, na.rm = TRUE)))
CI_upper_quad <- apply(quad_boot$t, 2, function(x) as.numeric(quantile(x, probs = .975, na.rm = TRUE)))


boot_tab <- as.data.table(base_boot$t0)
setnames(boot_tab, "V1", "Value")
boot_tab[, ":="(random_effect = c("Player ID", "Game environments"),
                    upper_CI = CI_upper_base,
                    lower_CI = CI_lower_base)]
save(boot_tab2, file = "05D_ICC-table.rda")
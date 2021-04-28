#########################################################################

#                   ICCs for the multivariate model                      #

#########################################################################


# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec
# -----------------------------------------------------------------------





# =======================================================================
# 1. Set working directory, load libraries, datasets, and models
# =======================================================================

# Librairies
library(data.table)
library(brms)
library(broom.helpers)

# Load dataset
data <- fread("./data/02_merged-data.csv",
              select = c("mirrors_id", "match_id", 
                         "map_name", "hunting_success", "Zsqrtspeed", 
                         "Zsqrtprox_mid_guard", "Zsqrtspace_covered_rate",
                         "Zsqrtsurv_speed", "Zsqrthook_start_time",
                         "Zsqrtsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Load model
load("./outputs/03A_multivariate-model.rda")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Compute ICCs and their 95% credibility intervals
# =======================================================================

# Extract random effect standard deviations
ran_var <- data.table(posterior_samples(mv_model)[,c(13:24, 43:46)])

# Compute variances for each random effect
ran_var[, c("speedvar_char", "spacevar_char", "guardvar_char", "hookvar_char",
            "speedvar_map", "spacevar_map", "guardvar_map", "hookvar_map", 
            "speedvar_id", "spacevar_id", "guardvar_id", "hookvar_id",
            "speedvar_resid", "spacevar_resid", "guardvar_resid", "hookvar_resid") := 
          lapply(.SD, function(x) x^2),
            .SDcols = c(1:16)][, c(1:16) := NULL]

# Compute total variance            
ran_var[, speedvar_total := rowSums(ran_var[, c(1,5,9,13)])]
ran_var[, spacevar_total := rowSums(ran_var[, c(2,6,10,14)])]
ran_var[, guardvar_total := rowSums(ran_var[, c(3,7,11,15)])]
ran_var[, hookvar_total := rowSums(ran_var[, c(4,8,12,16)])]

# Calculate ICCs
# ID
ran_var[, speedicc_id := speedvar_id / speedvar_total]
ran_var[, spaceicc_id := spacevar_id / spacevar_total]
ran_var[, guardicc_id := guardvar_id / guardvar_total]
ran_var[, hookicc_id := hookvar_id / hookvar_total]

# map
ran_var[, speedicc_map := speedvar_map / speedvar_total]
ran_var[, spaceicc_map := spacevar_map / spacevar_total]
ran_var[, guardicc_map := guardvar_map / guardvar_total]
ran_var[, hookicc_map := hookvar_map / hookvar_total]

# character
ran_var[, speedicc_char := speedvar_char / speedvar_total]
ran_var[, spaceicc_char := spacevar_char / spacevar_total]
ran_var[, guardicc_char := guardvar_char / guardvar_total]
ran_var[, hookicc_char := hookvar_char / hookvar_total]

# residuals
ran_var[, speedicc_resid := speedvar_resid / speedvar_total]
ran_var[, spaceicc_resid := spacevar_resid / spacevar_total]
ran_var[, guardicc_resid := guardvar_resid / guardvar_total]
ran_var[, hookicc_resid := hookvar_resid / hookvar_total]

# Create table with mean icc and credibility interval
icc_tab <- data.table(group = c("speedicc_id", "spaceicc_id", "guardicc_id", "hookicc_id",
                                "speedicc_map", "spaceicc_map", "guardicc_map", "hookicc_map",
                                "speedicc_char", "spaceicc_char", "guardicc_char", "hookicc_char",
                                "speedicc_resid", "spaceicc_resid", "guardicc_resid", "hookicc_resid"),
                      mean = as.numeric(ran_var[, lapply(.SD, mean),
                                                  .SDcols = c(21:36)]),
                      rbind(coda::HPDinterval(as.mcmc(ran_var[,21]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,22]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,23]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,24]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,25]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,26]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,27]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,28]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,29]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,30]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,31]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,32]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,33]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,34]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,35]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,36]), 0.95)
                            )
                       )

icc_tab[, ranef_variable := c(rep("id", 4),
                              rep("map", 4),
                              rep("character", 4),
                              rep("resid", 4))]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Save values in table
# =======================================================================

save(icc_tab, file = "./outputs/03A_icc-table.RDS")

# =======================================================================
# =======================================================================

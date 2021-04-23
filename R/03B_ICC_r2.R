#########################################################################

#                  Model fit and ICCs for the base model                #

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
                         "map_name", "hunting_success", "Zspeed", 
                         "Zprox_mid_guard", "Zspace_covered_rate",
                         "Zsurv_speed", "Zhook_start_time",
                         "Zsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Load model
load("./outputs/03B_hunting_success_base-model.rda")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Assess model fit (R-squared)
# =======================================================================
# Calculation by hand 
# Based on the supplementary material 2 from Nakagawa & al. 2017

# Compute the model matrixes
mm1 <- model_get_model_matrix(base_model)

# Variance components :
# 1. Fixed effects variance
# --------------------------
# Compute variance in fitted values (Fixed effects variance)
VarF1 <- var(as.vector(mm1%*%fixef(base_model)))

# 2. Distribution-specific variance
# --------------------------
# For binomial model with OLRE
VarDS1 <- pi^2/3

# For beta binomial model
# (phi from the distribution)
#VarDS1 <- summary(base_model)$spec_pars[1]

# 3. Random effects variance
# --------------------------
VarR1 <- VarCorr(base_model)$mirrors_id$sd[1]^2 + 
         VarCorr(base_model)$map_name$sd[1]^2

VarSE1 <- VarCorr(base_model)$obs$sd[1]^2

# 4. Total variance
# --------------------------
# binomial model with OLRE
VarT1 <- VarF1 + VarR1 + VarSE1 + VarDS1

# beta-binomial model
#VarT1 <- VarF1 + VarR1 + VarDS1

# 5. Compute R-squared values
# --------------------------
# Marginal R2
R2_M1 <- VarF1 /
         VarT1

# Conditional R2 (OLRE is excluded in the numerator to only account for random effects)
R2_C1 <- (VarF1 + VarR1) / # Fixed effect variance + random effect variance
          VarT1                   # Total variance

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Compute ICCs and their 95% credibility intervals
# =======================================================================

# Extract random effect standard deviations
ran_var <- data.table(posterior_samples(base_model, 
                  pars = c("sd_mirrors_id__Intercept",
                            "sd_map_name__Intercept",
                            "sd_obs__Intercept")))

# Compute variances for each random effect + VarDS
ran_var[, c("var_id", "var_map", "var_obs") := 
          lapply(.SD, function(x) x^2),
            .SDcols = c(1:3)][, var_DS := VarDS1]

# Compute total variance            
ran_var[, var_tot := rowSums(ran_var[, 4:7])]

# Calculate ICCs
ran_var[, c("icc_id", "icc_map", "icc_obs") := 
          lapply(.SD, function(x) x / var_tot),
            .SDcols = c(4:6)]

# Create table with mean icc and credibility interval
icc_tab <- data.table(group = c("id", "map", "obs"),
                      mean = as.numeric(ran_var[, lapply(.SD, mean),
                                                  .SDcols = c(9:11)]),
                      rbind(coda::HPDinterval(as.mcmc(ran_var[,9]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,10]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,11]), 0.95)
                            )
                       )

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Save values in table
# =======================================================================

r2_tab <- as.data.table(cbind(R2_M1, R2_C1))
icc_tab <- round(icc_tab[, 2:4], digits = 3)

capture.output(r2_tab, file = "./outputs/03B_r2-table.txt")
capture.output(icc_tab, file = "./outputs/03B_icc-table.txt")

# =======================================================================
# =======================================================================
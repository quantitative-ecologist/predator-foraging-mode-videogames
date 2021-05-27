#########################################################################

#              Model fit and ICCs for the quadratic model               #

#########################################################################


# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec
# -----------------------------------------------------------------------





# =======================================================================
# 1. Load libraries, datasets, and models
# =======================================================================

# Librairies
library(data.table)
library(brms)
library(broom.helpers)

# Load dataset
data <- fread("./data/merged-data.csv",
              select = c("mirrors_id", "match_id", 
                         "map_name", "hunting_success", "Zspeed", 
                         "Zprox_mid_guard", "Zspace_covered_rate",
                         "Zsurv_speed", "Zhook_start_time",
                         "Zsurv_space_covered_rate"),
                         stringsAsFactors = TRUE)

# Load model
quadratic_model1 <- readRDS("./outputs/03C_hunting_success_quadratic-model1.rds")
load("./outputs/03C_hunting_success_quadratic-model2.rda")
quadratic_model2 <- quadratic_model
rm(quadratic_model)

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Assess model fit (R-squared)
# =======================================================================
# Calculation by hand 
# Based on the supplementary material 2 from Nakagawa & al. 2017

# Compute the model matrixes
mm1 <- model_get_model_matrix(quadratic_model1)
mm2 <- model_get_model_matrix(quadratic_model2)

# Variance components :
# 1. Fixed effects variance
# --------------------------
# Compute variance in fitted values (Fixed effects variance)
VarF1 <- var(as.vector(mm1%*%fixef(quadratic_model1)))
VarF2 <- var(as.vector(mm2%*%fixef(quadratic_model2)))

# 2. Distribution-specific variance
# --------------------------
# For binomial model with OLRE
VarDS <- pi^2/3

# For beta binomial model
# (phi from the distribution)
#VarDS2 <- summary(quadratic_model)$spec_pars[1]

# 3. Random effects variance
# --------------------------
VarR1 <- VarCorr(quadratic_model1)$mirrors_id$sd[1]^2 + 
         VarCorr(quadratic_model1)$map_name$sd[1]^2

VarR2 <- VarCorr(quadratic_model2)$mirrors_id$sd[1]^2 + 
         VarCorr(quadratic_model2)$map_name$sd[1]^2

VarSE1 <- VarCorr(quadratic_model1)$obs$sd[1]^2
VarSE2 <- VarCorr(quadratic_model2)$obs$sd[1]^2

# 4. Total variance
# --------------------------
# binomial model with OLRE
VarT1 <- VarF1 + VarR1 + VarSE1 + VarDS
VarT2 <- VarF2 + VarR2 + VarSE2 + VarDS

# beta-binomial model
#VarT2 <- VarF2 + VarR2 + VarDS2

# 5. Compute R-squared values
# --------------------------
# Marginal R2
R2_M1 <- VarF1 /
         VarT1

R2_M2 <- VarF2 /
         VarT2

# Conditional R2 (OLRE is excluded in the numerator to only account for random effects)
R2_C1 <- (VarF1 + VarR1) / # Fixed effect variance + random effect variance
          VarT1                   # Total variance

R2_C2 <- (VarF2 + VarR2) / # Fixed effect variance + random effect variance
          VarT2 

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Compute ICCs and their 95% credibility intervals
# =======================================================================

# Extract random effect standard deviations
ran_var <- data.table(posterior_samples(quadratic_model1, 
                                        pars = c("sd_mirrors_id__Intercept",
                                                 "sd_map_name__Intercept",
                                                 "sd_obs__Intercept")))

ran_var2 <- data.table(posterior_samples(quadratic_model2, 
                                         pars = c("sd_mirrors_id__Intercept",
                                                  "sd_map_name__Intercept",
                                                  "sd_obs__Intercept")))


# Compute variances for each random effect + VarDS
ran_var[, c("var_id", "var_map", "var_obs") := 
          lapply(.SD, function(x) x^2),
            .SDcols = c(1:3)][, var_DS := VarDS]

ran_var2[, c("var_id", "var_map", "var_obs") := 
          lapply(.SD, function(x) x^2),
        .SDcols = c(1:3)][, var_DS := VarDS]


# Compute total variance            
ran_var[, var_tot := rowSums(ran_var[, 4:7])]
ran_var2[, var_tot := rowSums(ran_var2[, 4:7])]


# Calculate ICCs
ran_var[, c("icc_id", "icc_map", "icc_obs") := 
          lapply(.SD, function(x) x / var_tot),
        .SDcols = c(4:6)]

ran_var2[, c("icc_id", "icc_map", "icc_obs") := 
           lapply(.SD, function(x) x / var_tot),
         .SDcols = c(4:6)]


# Create table with mean icc and credibility interval
icc_tab_quad1 <- data.table(group = c("id", "map", "obs"),
                           mean = as.numeric(ran_var[, lapply(.SD, mean),
                                                      .SDcols = c(9:11)]),
                           rbind(coda::HPDinterval(as.mcmc(ran_var[,9]), 0.95),
                                 coda::HPDinterval(as.mcmc(ran_var[,10]), 0.95),
                                 coda::HPDinterval(as.mcmc(ran_var[,11]), 0.95)
                                 )
                            )

icc_tab_quad2 <- data.table(group = c("id", "map", "obs"),
                           mean = as.numeric(ran_var2[, lapply(.SD, mean),
                                                     .SDcols = c(9:11)]),
                           rbind(coda::HPDinterval(as.mcmc(ran_var2[,9]), 0.95),
                                 coda::HPDinterval(as.mcmc(ran_var2[,10]), 0.95),
                                 coda::HPDinterval(as.mcmc(ran_var2[,11]), 0.95)
                                 )
                            )

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Save values in table
# =======================================================================

r2_tab_quad <- as.data.table(cbind(R2_M1, R2_C1, R2_M2, R2_C2))

# Round values for icc table
round_val <- function (x) {round(x, digits = 3)}
icc_tab_quad1[, c("mean", "lower", "upper") :=
                lapply(.SD, round_val), 
              .SDcols = c(2:4)]

icc_tab_quad2[, c("mean", "lower", "upper") :=
                lapply(.SD, round_val), 
              .SDcols = c(2:4)]

# Bind both tables
icc_table <- rbind(icc_tab_quad1, icc_tab_quad2)

capture.output(r2_tab_quad, file = "./outputs/03C_r2-table.txt")
capture.output(icc_table, file = "./outputs/03C_icc-table.txt")

# =======================================================================
# =======================================================================
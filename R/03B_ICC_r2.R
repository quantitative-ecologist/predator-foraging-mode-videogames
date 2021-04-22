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
load("03B_hunting_success_base-model.rda")
load("./outputs/base_model1.rda")
load("./outputs/base_beta-model.rda")
print(object.size(base_model), units = "MB")

# =======================================================================
# =======================================================================


fonction <- function (x) {exp(x) / (1 + exp(x))}
VarDS1 <- summary(base_beta)$spec_pars[1]


fonction(VarDS1)


VarCorr(base_beta)$mirrors_id$sd[1]^2
VarCorr(base_beta)$map_name$sd[1]^2


# =======================================================================
# 3. Assess model fit (R-squared)
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
#VarDS <- pi^2/3

# For beta binomial model
# (phi from the distribution)
VarDS1 <- summary(base_model)$spec_pars[1]

# 3. Random effects variance
# --------------------------
VarR1 <- VarCorr(base_model)$mirrors_id$sd[1]^2 + 
         VarCorr(base_model)$map_name$sd[1]^2

#VarSE1 <- VarCorr(base_model)$obs$sd[1]

# 4. Total variance
# --------------------------
# binomial model with OLRE
#VarT1 <- VarF2 + VarR2 + VarSE2 + VarDS

# beta-binomial model
VarT1 <- VarF1 + VarR1 + VarDS1

# 5. Compute R-squared values
# --------------------------
# Marginal R2
R2_M1 <- VarF1 /
         VarT1

# Conditional R2 (OLRE is excluded in the numerator to only account for random effects)
R2_C1 <- (VarF1 + VarR1) / # Fixed effect variance + random effect variance
          VarT1                   # Total variance


# Save r-squared values into a table
r_squared <- as.data.table(cbind(R2_M1, R2_C1))
capture.output(r_squared, file = "03B_r2-table.txt")
# =======================================================================
# =======================================================================





# =======================================================================
# 4. Compute ICCs and their 95% credibility intervals
# =======================================================================

names(posterior_samples(base_model))[1:35]

ID_var <- posterior_samples(base_model)$"sd_mirrors_id__Intercept"^2
map_var <- posterior_samples(base_model)$"sd_map_name__Intercept"^2
res_var <- posterior_samples(base_model)$"phi"


ID_rpt <- map_var + 

mean(ID_var);coda::HPDinterval(as.mcmc(ID_var),0.95)


ID_var <- VarCorr(base_model)$mirrors_id$sd[1]^2
map_var <- VarCorr(base_model)$map_name$sd[1]^2


ID_var / VarT1
map_var / VarT1

# =======================================================================
# =======================================================================
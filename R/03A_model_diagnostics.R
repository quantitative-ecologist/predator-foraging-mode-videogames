

# =======================================================================
# 4. Model diagnostics 
# =======================================================================
# consider trying gelman.diag and gelman.plot for diagnostics too!


# First model
# ------------------------------------------------
# Visualize fixed effects and random effect variance components
plot(ModMV1$VCV)

# Check for autocorrelation
autocorr.diag(ModMV1$VCV)

# Check for effective sample size
# It should be equal to the number of times we sample (so 2000)
effectiveSize(ModMV1$VCV)

# Check linearity of residuals
corr <- as.data.table(posterior.cor(ModMV1$VCV[, 28:36]))
with(corr, plot(V2~V3))
with(corr, plot(V2~V6))
with(corr, plot(V3~V6))
# All good
# ----------------------

# Summary statistics
summary(ModMV1)

# Checking the variance covariance matrix.
# Values for the variances and covariances between and within IDs
posterior.mode(ModMV1$VCV)
HPDinterval(ModMV1$VCV)
# ------------------------------------------------



# Second model with effect of prey
# ------------------------------------------------
# Visualize fixed effects and random effect variance components
plot(ModMV2$Sol)
plot(ModMV2$VCV)

# Check for autocorrelation
autocorr.diag(ModMV2$Sol)
autocorr.diag(ModMV2$VCV)

# Check for effective sample size
# It should be equal to the number of times we sample (so 2000)
effectiveSize(ModMV2$Sol)
effectiveSize(ModMV2$VCV) 
# ----------------------

# Summary statistics
summary(ModMV2$Sol)

# Extracting model parameters
beta <- posterior.mode(ModMV2$Sol) # Point estimate for fixed effects
CI <- HPDinterval(ModMV2$Sol) # 95% confidence intervals for fixed effects
# making a table to work with
beta_table <- as.data.table(cbind(beta, CI))
beta_table <- cbind(parameter_name = rownames(CI), beta_table)
beta_table


# Checking the variance covariance matrix.
# Values for the variances and covariances between and within IDs
posterior.mode(ModMV2$VCV)
HPDinterval(ModMV2$VCV)
# ======================================================================
# ======================================================================





# =======================================================================
# 5. Variance-covariance and correlation matrixes 
# =======================================================================

# Model 1
# ------------------------------------------------
# Correlations AMONG individuals (it's a 3x3 matrix so we take the first 9)
among_corr <- matrix(posterior.mode(posterior.cor(ModMV1$VCV[, 1:9])), ncol = 3, byrow = TRUE)
among_corrCI <- HPDinterval(posterior.cor(ModMV1$VCV[, 1:9]))

# Correlations AMONG environments
env_corr <- matrix(posterior.mode(posterior.cor(ModMV1$VCV[, 10:18])), ncol = 3, byrow = TRUE)
env_corrCI <- HPDinterval(posterior.cor(ModMV1$VCV[, 10:18]))
# No syndrome caused by different avatars
matrix(posterior.mode(posterior.cor(ModMV1$VCV[, 19:27])), ncol = 3, byrow = TRUE)

# Correlations WITHIN individuals (it's a 3x3 matrix so we take the last 9)
within_corr <- matrix(posterior.mode(posterior.cor(ModMV1$VCV[, 28:36])), ncol = 3, byrow = TRUE)
within_corrCI <- HPDinterval(posterior.cor(ModMV1$VCV[, 28:36]))


# make a list of the among and within correlations and save them
IDcorr_list <- list(among_corr = among_corr, among_corrCI = among_corrCI, 
                    within_corr = within_corr, within_corrCI = within_corrCI,
                    env_corr = env_corr, env_corrCI = env_corrCI)
save(IDcorr_list, file = "05A_IDcorr_list.rda")
load("05A_IDcorr_list.rda")
# ------------------------------------------------



# Model 2
# ------------------------------------------------
# Correlations AMONG individuals (it's 3 3x3 matrix so we take the first 9)
matrix(posterior.mode(posterior.cor(ModMV2$VCV[, 1:9])), ncol = 3, byrow = TRUE)
HPDinterval(posterior.cor(ModMV2$VCV[, 1:9]))

# Correlations WITHIN individuals (taking residuals so its the last 28:36)
matrix(posterior.mode(posterior.cor(ModMV2$VCV[, 28:36])), ncol = 3, byrow = TRUE)
HPDinterval(posterior.cor(ModMV2$VCV[, 28:36]))

# Results are mostly the same

# ======================================================================
# ======================================================================





# =======================================================================
# 6. Calculate repeatabilities (ICCs) 
# =======================================================================

# First model
# ------------------------------------------------
# Speed player ID repeatability
rz1.1 <- ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] / 
         (ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
          ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
          ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] +
          ModMV1$VCV[,"traitZsqrtspeed:traitZsqrtspeed.units"])
CIrz1.1 <- HPDinterval(rz1.1)[, 1:2]
rz1.1 <- mean(rz1.1)

# map repeatability
map1.1 <- ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] / 
          (ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.units"])
CImap1.1 <- HPDinterval(map1.1)[, 1:2]
map1.1 <- mean(map1.1)

# character repeatability
char1.1 <- ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] / 
          (ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] + 
           ModMV1$VCV[, "traitZsqrtspeed:traitZsqrtspeed.units"])
CIchar1.1 <- HPDinterval(char1.1)[, 1:2]
char1.1 <- mean(char1.1)



# Space player ID repeatability
rz2.1 <- ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] / 
         (ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] + 
          ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
          ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] +
          ModMV1$VCV[,"traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CIrz2.1 <- HPDinterval(rz2.1)[, 1:2]
rz2.1 <- mean(rz2.1)

# map repeatability
map2.1 <- ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] / 
          (ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] +
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] +
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CImap2.1 <- HPDinterval(map2.1)[, 1:2]
map2.1 <- mean(map2.1)

# character repeatability
char2.1 <- ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] / 
          (ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] +
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] +
           ModMV1$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CIchar2.1 <- HPDinterval(char2.1)[, 1:2]
char2.1 <- mean(char2.1)



# Guard player ID repeatability
rz3.1 <- ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] / 
         (ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] + 
          ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] + 
          ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] +
          ModMV1$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CIrz3.1 <- HPDinterval(rz3.1)[, 1:2]
rz3.1 <- mean(rz3.1)

# map repeatability
map3.1 <- ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] / 
          (ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] + 
           ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] + 
           ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] + 
           ModMV1$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CImap3.1 <- HPDinterval(map3.1)[, 1:2]
map3.1 <- mean(map3.1)

# character repeatability
char3.1 <- ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] / 
          (ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] + 
           ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] + 
           ModMV1$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] + 
           ModMV1$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CIchar3.1 <- HPDinterval(char3.1)[, 1:2]
char3.1 <- mean(char3.1)
# ------------------------------------------------



# Second model with effect of prey
# ------------------------------------------------
# Speed 
# player ID repeatability
rz1.2 <- ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] / 
         (ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
          ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
          ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] + 
          ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.units"])
CIrz1.2 <- HPDinterval(rz1.2)[, 1:2]
rz1.2 <- mean(rz1.2)

# map repeatability
map1.2 <- ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] / 
          (ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.units"])
CImap1.2 <- HPDinterval(map1.2)[, 1:2]
map1.2 <- mean(map1.2)

# char repeatability
char1.2 <- ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] / 
          (ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.mirrors_id"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.map_name"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.character_name"] + 
           ModMV2$VCV[, "traitZsqrtspeed:traitZsqrtspeed.units"])
CIchar1.2 <- HPDinterval(char1.2)[, 1:2]
char1.2 <- mean(char1.2)



# Space 
# player ID repeatability
rz2.2 <- ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] / 
         (ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] + 
          ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
          ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] + 
          ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CIrz2.2 <- HPDinterval(rz2.2)[, 1:2]
rz2.2 <- mean(rz2.2)

# map repeatability
map2.2 <- ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] / 
          (ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] +
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] +
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CImap2.2 <- HPDinterval(map2.2)[, 1:2]
map2.2 <- mean(map2.2)

# char repeatability
char2.2 <- ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] / 
          (ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.mirrors_id"] +
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.map_name"] + 
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.character_name"] +
           ModMV2$VCV[, "traitZsqrtspace_covered_rate:traitZsqrtspace_covered_rate.units"])
CIchar2.2 <- HPDinterval(char2.2)[, 1:2]
char2.2 <- mean(char2.2)



# Guard 
# player ID repeatability
rz3.2 <- ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] / 
         (ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] +
          ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] +
          ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] +
          ModMV2$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CIrz3.2 <- HPDinterval(rz3.2)[, 1:2]
rz3.2 <- mean(rz3.2)

# map repeatability
map3.2 <- ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] / 
          (ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] + 
           ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] + 
           ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] + 
           ModMV2$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CImap3.2 <- HPDinterval(map3.2)[, 1:2]
map3.2 <- mean(map3.2)

# character repeatability
char3.2 <- ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] / 
          (ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.mirrors_id"] + 
           ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.map_name"] + 
           ModMV2$VCV[, "traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.character_name"] + 
           ModMV2$VCV[,"traitZsqrtprox_mid_guard:traitZsqrtprox_mid_guard.units"])
CIchar3.2 <- HPDinterval(char3.2)[, 1:2]
char3.2 <- mean(char3.2)



# Create a table and save it so I can check % variances
rpt <- as.data.table(rbind(rz1.1, rz2.1, rz3.1,
                           map1.1, map2.1, map3.1,
                           char1.1, char2.1, char3.1,
                           rz1.2, rz2.2, rz3.2,
                           map1.2, map2.2, map3.2,
                           char1.2, char2.2, char3.2)
                    )

CI <- as.data.table(rbind(CIrz1.1, CIrz2.1, CIrz3.1, 
                          CImap1.1, CImap2.1, CImap3.1,
                          CIchar1.1, CIchar2.1, CIchar3.1,
                          CIrz1.2, CIrz2.2, CIrz3.2,
                          CImap1.2, CImap2.2, CImap3.2,
                          CIchar1.2, CIchar2.2, CIchar3.2)
                   )

ranef_table <- cbind(parameter = c("CIrz1.1", "CIrz2.1", "CIrz3.1", 
                                   "CImap1.1", "CImap2.1", "CImap3.1",
                                   "CIchar1.1", "CIchar2.1", "CIchar3.1",
                                   "CIrz1.2", "CIrz2.2", "CIrz3.2",
                                   "CImap1.2", "CImap2.2", "CImap3.2",
                                   "CIchar1.2", "CIchar2.2", "CIchar3.2"), 
                     rpt = rpt, 
                     CI)

save(ranef_table, file = "03A_multivariate_ranef.rda")
load("03A_multivariate_ranef.rda")

# ======================================================================
# ======================================================================

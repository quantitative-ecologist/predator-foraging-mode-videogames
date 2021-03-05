
#########################################################################

#             A. Chapter 1. Quadratic performance analysis              #

#########################################################################
# Calculate quadratic performance gradients
# for behavioral variables expressed during matches.
# Hunting success = number of prey killed

# original scale from the model is LOG ODDS
# exp() is the ODDS RATIOS -> exp(fixef(mod))
# plogis() to obtain the probability scale from log odds (for plots)
# -------------------------------------------------------------------

# 1. The first part of the script performs quadratic performance analysis
# 2. The second part of the script performs quadratic performance analysis including prey behaviour





# 1. Set working directory, load libraries, and import dataset ==========
# =======================================================================

setwd("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/masters_project/chapter1/outputs") # personal computer onedrive UQAM Montiglio lab

library(data.table)
library(AICcmodavg)
library(lme4)
library(lattice)
library(MuMIn)
library(DHARMa)

data <- fread("02_merged-data.csv",
                        select = c("cohort", "mirrors_id", "match_id", 
                                   "map_name", "sum_bloodpoints",
                                   "Zspeed", "Zprox_mid_guard", "Zspace_covered_rate",
                                   "Zsurv_speed", "Zsurv_space_covered_rate"))


# Add an observation-level random effect (OLRE) (in case of overdispersion)
data$obs <- 1:nrow(data)

# Character variables to factor variables
char_as_factor <- names(data)[sapply(data, is.character)] # extract columns that are characters
data[, (char_as_factor) := lapply(.SD, as.factor), .SDcols = char_as_factor] # columns as factors


# Overdispersion function by B. Bolker from https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#overdispersion)
overdisp_fun <- function(model) {
   rdf <- df.residual(model)
   rp <- residuals(model, type = "pearson")
   Pearson.chisq <- sum(rp^2)
   prat <- Pearson.chisq/rdf
   pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
   c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

# To load models and tables directly once they have been computed
load("05C_quadratic-model.rda")
load("05C_quadratic_prey-model.rda")
load("05C_BIC-table.rda")
load("05C_fixef-table.rda")
load("05C_ICC-table.rda")
load("05C_r2-table.rda")
# =======================================================================
# =======================================================================





# 2. Build full model ===================================================
# =======================================================================
# Tested without an OLRE
# The model was overdispersed

# With an OLRE, it only gives the second warning (very large eigenvalue)
# Standard error estimates seem OK (not too large)
system.time(quadratic_model <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))

# ignore warnings
# =======================================================================
# =======================================================================





# 3. model diagnostics ==================================================
# =======================================================================
overdisp_fun(quadratic_model)
# the model is now underdispersed = OK


# Residuals have somewhat an odd shape
plot(fitted(quadratic_model), resid(quadratic_model))

# I extract them and bind them to the data
residuals <- resid(quadratic_model) # produces nas
sum(is.na(residuals)) # only three observations produced NAs


# Check the distribution of random effects
# dotplot(ranef(quadratic_model, condVar = TRUE))
# Plot residuals with random effects (they all seem homogeneous)
plot(residuals~data$map_name)
plot(residuals~data$mirrors_id)
# -------------------------------------



# Model diagnostics using DHARMa package
simulationOutput <- simulateResiduals(fittedModel = quadratic_model, n = 1000)
save(simulationOutput, file = "05C_simulated_resids.rda")
plot(simulationOutput)


# plot the simulated residuals with each fixed effect predicted values
speed_newdat <- data.frame(speed_x = data$Zspeed,
                           space_x = mean(data$Zspace_covered_rate),
                           guard_x = mean(data$Zprox_mid_guard))

space_newdat <- data.frame(space_x = data$Zspace_covered_rate,
                           speed_x = mean(data$Zspeed),
                           guard_x = mean(data$Zprox_mid_guard))

guard_newdat <- data.frame(guard_x = data$Zprox_mid_guard,
                           speed_x = mean(data$Zspeed),
                           space_x = mean(data$Zspace_covered_rate))
# Model matrixes operate from the first vector so I might have to change order???
# Test this to see. Test also if changing coefficient order changes something i.e. fixef()[c()]
speed_mm <- model.matrix(~
                         I(speed_x^2) +
                         I(space_x^2) +
                         I(guard_x^2) +
                         speed_x * space_x +
                         speed_x * guard_x +
                         space_x * guard_x, speed_newdat)
speed_y <- speed_mm%*%fixef(quadratic_model)

space_mm <- model.matrix(~
                         I(speed_x^2) +
                         I(space_x^2) +
                         I(guard_x^2) +
                         speed_x * space_x +
                         speed_x * guard_x +
                         space_x * guard_x, space_newdat)
space_y <- space_mm%*%fixef(quadratic_model)

guard_mm <- model.matrix(~
                         I(speed_x^2) +
                         I(space_x^2) +
                         I(guard_x^2) +
                         speed_x * space_x +
                         speed_x * guard_x +
                         space_x * guard_x, guard_newdat)
guard_y <- guard_mm%*%fixef(quadratic_model)

plotResiduals(simulationOutput, speed_y) # deviations not so bad 
plotResiduals(simulationOutput, space_y) # deviates starting at the center
plotResiduals(simulationOutput, guard_y) # very large deviation
# There is a lot of deviation overall. Again, it could be attributed to the fact that bloodpoints have large variation with factor not taken into account in this model

# Test model fit, dispersion and outliers
testDispersion(simulationOutput, plot = T) # tells me again that it's underdispersed
testOutliers(simulationOutput, plot = T)
testUniformity(simulationOutput, plot = T)
# =======================================================================
# =======================================================================


# 4. Build models where I take out each coefficient =====================
# =======================================================================
# This step is to use BIC to assess parameter importance in the model

# no speed
system.time(no_speed2 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                      I(Zspace_covered_rate^2) +
                                      I(Zprox_mid_guard^2) +
                                      Zspeed +
                                      Zspace_covered_rate +
                                      Zprox_mid_guard +
                                      Zspeed : Zspace_covered_rate +
                                      Zspeed : Zprox_mid_guard +
                                      Zspace_covered_rate : Zprox_mid_guard +
                                      (1 | map_name) +
                                      (1 | mirrors_id) +
                                      (1 | obs),
                              family = binomial,
                              data = data))

# no space
system.time(no_space2 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                       I(Zspeed^2) +
                                       I(Zprox_mid_guard^2) +
                                       Zspeed +
                                       Zspace_covered_rate +
                                       Zprox_mid_guard +
                                       Zspeed : Zspace_covered_rate +
                                       Zspeed : Zprox_mid_guard +
                                       Zspace_covered_rate : Zprox_mid_guard +
                                       (1 | map_name) +
                                       (1 | mirrors_id) +
                                       (1 | obs),
                               family = binomial,
                               data = data))

# no guard
system.time(no_guard2 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                       I(Zspeed^2) +
                                       I(Zspace_covered_rate^2) +
                                       Zspeed +
                                       Zspace_covered_rate +
                                       Zprox_mid_guard +
                                       Zspeed : Zspace_covered_rate +
                                       Zspeed : Zprox_mid_guard +
                                       Zspace_covered_rate : Zprox_mid_guard +
                                       (1 | map_name) +
                                       (1 | mirrors_id) +
                                       (1 | obs),
                               family = binomial,
                               data = data))

# no interaction speed*space
system.time(no_inter1 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                       I(Zspeed^2) +
                                       I(Zspace_covered_rate^2) +
                                       I(Zprox_mid_guard^2) +
                                       Zspeed +
                                       Zspace_covered_rate +
                                       Zprox_mid_guard +
                                       Zspeed : Zprox_mid_guard +
                                       Zspace_covered_rate : Zprox_mid_guard +
                                       (1 | map_name) +
                                       (1 | mirrors_id) +
                                       (1 | obs),
                               family = binomial,
                               data = data))

#  no interaction speed*guard
system.time(no_inter2 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                       I(Zspeed^2) +
                                       I(Zspace_covered_rate^2) +
                                       I(Zprox_mid_guard^2) +
                                       Zspeed +
                                       Zspace_covered_rate +
                                       Zprox_mid_guard +
                                       Zspeed : Zspace_covered_rate +
                                       Zspace_covered_rate : Zprox_mid_guard +
                                       (1 | map_name) +
                                       (1 | mirrors_id) +
                                       (1 | obs),
                               family = binomial,
                               data = data))

#  no interaction space*guard
system.time(no_inter3 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                       I(Zspeed^2) +
                                       I(Zspace_covered_rate^2) +
                                       I(Zprox_mid_guard^2) +
                                       Zspeed +
                                       Zspace_covered_rate +
                                       Zprox_mid_guard +
                                       Zspeed : Zspace_covered_rate +
                                       Zspeed : Zprox_mid_guard +
                                       (1 | map_name) +
                                       (1 | mirrors_id) +
                                       (1 | obs),
                               family = binomial,
                               data = data))

# no map_name
system.time(no_map <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                    I(Zspeed^2) +
                                    I(Zspace_covered_rate^2) +
                                    I(Zprox_mid_guard^2) +
                                    Zspeed +
                                    Zspace_covered_rate +
                                    Zprox_mid_guard +
                                    Zspeed : Zspace_covered_rate +
                                    Zspeed : Zprox_mid_guard +
                                    Zspace_covered_rate : Zprox_mid_guard +
                                    (1 | mirrors_id) +
                                    (1 | obs),
                            family = binomial,
                            data = data))

# no mirrors_id
system.time(no_id <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                   I(Zspeed^2) +
                                   I(Zspace_covered_rate^2) +
                                   I(Zprox_mid_guard^2) +
                                   Zspeed +
                                   Zspace_covered_rate +
                                   Zprox_mid_guard +
                                   Zspeed : Zspace_covered_rate +
                                   Zspeed : Zprox_mid_guard +
                                   Zspace_covered_rate : Zprox_mid_guard +
                                   (1 | map_name) +
                                   (1 | obs),
                           family = binomial,
                           data = data))

# compute AIC, QAIC, and BIC tables
model_list <- list(quadratic_model, no_speed2,
                   no_space2, no_guard2, no_inter1,
                   no_inter2, no_inter3, no_map, no_id)

model_names <- c("quadratic_model", "no_speed2", "no_space2",
                 "no_guard2", "no_inter1", "no_inter2",
                 "no_inter3", "no_map", "no_id")


# BIC method
# I generate the BIC tables
bic_table <- as.data.table(bictab(cand.set = model_list,
                                  modnames = model_names,
                                  sort = TRUE))

# Save the table in my outputs
save(bic_table, file = "05C_BIC-table.rda")


anova(quadratic_model, no_inter1)
anova(quadratic_model, no_inter2)
anova(quadratic_model, no_inter3)
# all interactions are important!
# =======================================================================
# =======================================================================





# 5. Report summary and coefficients, save outputs ======================
# =======================================================================
# save summary as a .txt file
s_quadratic_model <- summary(quadratic_model)
# capture.output(s_quadratic_model, file = "05C_quadratic_model-summary.txt")

# save(quadratic_model, file = "05C_quadratic-model.rda")
# =======================================================================
# =======================================================================





# =======================================================================
# 6. Random effect % variances (ICC)
# =======================================================================


# -----------------------------------------------------------------------------------------
# =======================================================================
# =======================================================================





# 7. Assess model fit (R-squared) =======================================
# =======================================================================
# Calculation by hand (supplementary material 2 from Nakagawa & al. 2017)

# Compute variance in fitted values (Fixed effects variance)
VarF <- var(as.vector(model.matrix(quadratic_model) %*% fixef(quadratic_model)))

# Getting the (theoretical) distribution-specific variance
VarDS <- pi^2/3

# Marginal R2
R2_M <- VarF / # Fixed effect variance
        (VarF + sum(as.numeric(VarCorr(quadratic_model))) + VarDS) # Total variance

# Conditional R2 (OLRE is excluded in the numerator to only account for random effects)
R2_C <- (VarF + sum(as.numeric(VarCorr(quadratic_model)[c(2,3)]))) / # Fixed effect variance + random effect variance
        (VarF + sum(as.numeric(VarCorr(quadratic_model))) + VarDS) # Total variance

# Save r-squared values into a table
r_squared <- as.data.table(cbind(R2_M, R2_C))
save(r_squared, file = "05C_r2-table.rda")
# =======================================================================
# =======================================================================




# Part 2 - Performance analysis including prey behaviour as covariates ==
# =======================================================================

# Build full model
system.time(quadratic_model_surv <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed +
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))
#
save(quadratic_model_surv, file = "06C_surv_model_quad.rda")

# Model summary
summary(quadratic_model_surv)
# =======================================================================
# =======================================================================





# Part 2.1 Bootstrap fixed effects for Table SIV ========================
# =======================================================================
system.time(
   boot_prey <- bootMer(quadratic_model_surv, # the model
                        boot_summary1, # the function we created (step 1)
                        seed = 20200904, 
                        type = "parametric",
                        parallel = "multicore",
                        ncpus = 16, 
                        nsim = 250) # some simulations failed
                )

# Save output
save(boot_prey, file = "05C_fixef-prey_boot.rda") 
# load("05C_fixef-prey_boot.rda")
# loading the file may display the name as "boot"
# because these simulations were run on a different computer
# change to boot_prey <- boot

CI_lower_prey <- apply(boot_prey$t, 2, function(x) as.numeric(quantile(x, probs = .025, na.rm = TRUE)))
CI_upper_prey <- apply(boot_prey$t, 2, function(x) as.numeric(quantile(x, probs = .975, na.rm = TRUE)))

# Create a table to save ICC's and their 95% confidence intervals
boot_prey_tab <- as.data.table(boot_prey$t0)
setnames(boot_prey_tab, "V1", "Value")
boot_prey_tab[, ":="(fixed_effect = c("Intercept", 
                                      "I(Zspeed^2)", 
                                      "I(Zspace_covered_rate^2)", 
                                      "I(Zprox_mid_guard^2)",
                                      "I(Zsurv_speed^2)",
                                      "I(Zsurv_space_covered_rate^2)",
                                      "Zspeed", 
                                      "Zspace_covered_rate",
                                      "Zprox_mid_guard", 
                                      "Zsurv_speed",
                                      "Zsurv_space_covered_rate",
                                      "Zspeed:Zspace_covered_rate",
                                      "Zspeed:Zprox_mid_guard", 
                                      "Zspace_covered_rate:Zprox_mid_guard",
                                      "Zspeed:Zsurv_speed",
                                      "Zprox_mid_guard:Zsurv_speed",
                                      "Zspeed:Zsurv_space_covered_rate",
                                      "Zprox_mid_guard:Zsurv_space_covered_rate",
                                      "Zspace_covered_rate:Zsurv_space_covered_rate"
                                      ),
                    lower_ci = CI_lower_prey,
                    upper_ci = CI_upper_prey)]

boot_prey_tab[, c(1,3,4) := round(.SD, 3), .SDcols = c(1,3,4)]
save(boot_prey_tab, file = "05C_fixef_prey-table.rda")



# =======================================================================
# =======================================================================





# Part 2.1 BICs for the model parameters ================================
# =======================================================================
# Correlational pred-prey parameters
# Zspeed : Zsurv_speed
system.time(cor1 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 #Zspeed : Zsurv_speed + # removed covariate
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))
save(cor1, file = "cor1.rda")

# Guard and speed
system.time(cor2 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed + 
                                                 #Zprox_mid_guard : Zsurv_speed + # removed covariate
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))
save(cor2, file = "cor2.rda")

# Speed and space
system.time(cor3 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed + 
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 #Zspeed : Zsurv_space_covered_rate +  # removed covariate
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))
save(cor3, file = "cor3.rda")

# Guard and space
system.time(cor4 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed + 
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 #Zprox_mid_guard : Zsurv_space_covered_rate +# removed covariate
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))
save(cor4, file = "cor4.rda")

# Space and space
system.time(cor5 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed + 
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 #Zspace_covered_rate : Zsurv_space_covered_rate + # removed covariate
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))
save(cor5, file = "cor5.rda")


# Other parameters
# -------------------------------
system.time(mod1 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 #I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed +
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))


system.time(mod2 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                # I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed +
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))


system.time(mod3 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                # I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed +
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))


system.time(mod4 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                              #   I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed +
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))


system.time(mod5 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                               #  I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed +
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))


system.time(mod6 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                           #      Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed +
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))


system.time(mod7 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                           #      Zspeed : Zprox_mid_guard +
                                                 Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed +
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))


system.time(mod8 <- glmer(cbind(sum_bloodpoints, 32000 - sum_bloodpoints) ~
                                                 # Quadratic terms
                                                 I(Zspeed^2) +
                                                 I(Zspace_covered_rate^2) +
                                                 I(Zprox_mid_guard^2) +
                                                 I(Zsurv_speed^2) +
                                                 I(Zsurv_space_covered_rate^2) +
                                                 # Linear terms
                                                 Zspeed +
                                                 Zspace_covered_rate +
                                                 Zprox_mid_guard +
                                                 Zsurv_speed +
                                                 Zsurv_space_covered_rate +
                                                 # pred traits covariances
                                                 Zspeed : Zspace_covered_rate +
                                                 Zspeed : Zprox_mid_guard +
                                             #    Zspace_covered_rate : Zprox_mid_guard +
                                                 # Pred-prey traits covariances
                                                 Zspeed : Zsurv_speed +
                                                 Zprox_mid_guard : Zsurv_speed +
                                                 Zspeed : Zsurv_space_covered_rate +
                                                 Zprox_mid_guard : Zsurv_space_covered_rate +
                                                 Zspace_covered_rate : Zsurv_space_covered_rate +
                                                 # random effects
                                                 (1 | map_name) +
                                                 (1 | mirrors_id) +
                                                 (1 | obs),
                                         family = binomial,
                                         data = data))


model_list <- list(quadratic_model_surv, cor1, cor2, cor3, cor4, cor5,
                   mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)
mod_names <- c("quadtratic_model_surv", "cor1", "cor2", "cor3", "cor4", "cor5",
                "mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "mod8")
bictab(cand.set = model_list, modnames = mod_names)







# Part 2.2 Assess model fit (R-squared) =================================
# =======================================================================
# Calculation by hand (supplementary material 2 from Nakagawa & al. 2017)

# Compute variance in fitted values (Fixed effects variance)
VarF <- var(as.vector(model.matrix(quadratic_model_surv) %*% fixef(quadratic_model_surv)))

# Getting the (theoretical) distribution-specific variance
VarDS <- pi^2/3

# Marginal R2
R2_M <- VarF / # Fixed effect variance
        (VarF + sum(as.numeric(VarCorr(quadratic_model_surv))) + VarDS) # Total variance

# Conditional R2 (OLRE is excluded in the numerator to only account for random effects)
R2_C <- (VarF + sum(as.numeric(VarCorr(quadratic_model_surv)[c(2,3)]))) / # Fixed effect variance + random effect variance
        (VarF + sum(as.numeric(VarCorr(quadratic_model_surv))) + VarDS) # Total variance


############################################################################

#     A. Chapter 1. Base hunting success analysis on scaled variables      #

############################################################################

# Calculate hunting success
# for behavioral variables expressed during matches.
# Hunting success = number of surv captured


# original scale from the model is LOG ODDS
# exp() is the ODDS RATIOS -> exp(fixef(mod))
# plogis() to obtain the probability scale from log odds (for plots)




# =======================================================================
# 1. Set working directory, load libraries, and import dataset 
# =======================================================================
setwd("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/PhD_project/chapter1/outputs") # personal computer onedrive UQAM Montiglio lab

library(data.table)
library(AICcmodavg)
library(lme4)
library(lattice)
library(MuMIn)
library(DHARMa)

data <- fread("02_merged-data.csv",
                        select = c("cohort", "mirrors_id", "match_id", 
                                   "map_name", "hunting_success", "Zspeed", 
                                   "Zprox_mid_guard", "Zspace_covered_rate",
                                   "Zsurv_speed", "Zsurv_space_covered_rate",
                                   "sqrtspeed", "sqrtprox_mid_guard", "sqrtspace_covered_rate"))


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

# To load the model and its components once it has been computed
load("05B_base-model.rda")
load("05B_r2-table.rda")
load("05B_BIC-table.rda")
load("05B_ICC-table.rda")
# =======================================================================
# =======================================================================





# =======================================================================
# 2. Build full model
# =======================================================================
# Check if cohort has an effect on the model
# Model cohort
system.time(model_cohort <- glmer(cbind(hunting_success, 4 - hunting_success) ~
                                         Zspeed +
                                         Zprox_mid_guard +
                                         Zspace_covered_rate +
                                         Zsurv_speed +
                                         Zsurv_space_covered_rate +
                                         (1 | map_name) +
                                         (1 | mirrors_id) +
                                         (1 | cohort),
                                  family = binomial,
                                  data = data)) #  149 seconds

# Model 1 
system.time(model1 <- glmer(cbind(hunting_success, 4 - hunting_success) ~
                                      Zspeed +
                                      Zprox_mid_guard +
                                      Zspace_covered_rate +
                                      Zsurv_speed +
                                      Zsurv_space_covered_rate +
                                      (1 | map_name) +
                                      (1 | mirrors_id),
                                  family = binomial,
                                  data = data)) # 68 seconds

# Test for effect of cohort
anova(model_cohort, model1)

# Cohort not significant, I can remove it from the analysis

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Model diagnostics
# =======================================================================
# Homogeneity of variance
plot(fitted(model1), resid(model1))
plot(model1, od = 0.05, idLabels=~.obs)
# Variance seems homogeneous

# Plot random effects estimates (mirrors_id and maps)
dotplot(ranef(model1, condVar = TRUE))
# there is a very small "S" pattern for both random effects (not even 1std dev from the mean)

# Test for overdispersion
overdisp_fun(model1) # seems there is large overdispersion
#        chisq        ratio          rdf            p
# 1.587717e+08 2.241747e+03 7.082500e+04 0.000000e+00
# =======================================================================
# =======================================================================





# =======================================================================
# 4. Fit the binomial model with an OLRE 
# ======================================================================
system.time(base_model <- glmer(cbind(hunting_success, 4 - hunting_success) ~
                                              Zspeed +
                                              Zprox_mid_guard +
                                              Zspace_covered_rate +
                                              Zsurv_speed +
                                              Zsurv_space_covered_rate +
                                              (1 | map_name) +
                                              (1 | mirrors_id) +
                                              (1 | obs),
                                              control = glmerControl(optimizer = "bobyqa"),
                                           family = binomial,
                                           data = data))

# Save the model for further use in plots
save(base_model, file = "05B_base-model.rda")
# =======================================================================
# =======================================================================





# =======================================================================
# 5. model diagnostics
# =======================================================================
overdisp_fun(base_model)
# the model is now underdispersed = OK


# Residuals have somewhat an odd shape
plot(fitted(base_model), resid(base_model))

# I extract them and bind them to the data
residuals <- resid(base_model)
sum(is.na(residuals))


# Check the distribution of random effects
# dotplot(ranef(base_model, condVar = TRUE))
# Plot residuals with random effects (they all seem homogeneous)
plot(residuals~data$map_name)
plot(residuals~data$mirrors_id)
# -------------------------------------



# Model diagnostics using DHARMa package
simulationOutput <- simulateResiduals(fittedModel = base_model, n = 1000)
save(simulationOutput, file = "05B_simulated_resids.rda")
plot(simulationOutput)


# plot the simulated residuals with each fixed effect predicted values
speed_newdat <- data.frame(speed_x = data$Zspeed,
                           space_x = mean(data$Zspace_covered_rate),
                           guard_x = mean(data$Zprox_mid_guard),
                           survspeed_x = mean(data$Zsurv_speed),
                           survspace_x = mean(data$Zsurv_space_covered_rate))

space_newdat <- data.frame(space_x = data$Zspace_covered_rate,
                           speed_x = mean(data$Zspeed),
                           guard_x = mean(data$Zprox_mid_guard),
                           survspeed_x = mean(data$Zsurv_speed),
                           survspace_x = mean(data$Zsurv_space_covered_rate))

guard_newdat <- data.frame(guard_x = data$Zprox_mid_guard,
                           speed_x = mean(data$Zspeed),
                           space_x = mean(data$Zspace_covered_rate),
                           survspeed_x = mean(data$Zsurv_speed),
                           survspace_x = mean(data$Zsurv_space_covered_rate))

survspeed_newdat <- data.frame(survspeed_x = data$Zsurv_speed,
                               speed_x = mean(data$Zspeed),
                               space_x = mean(data$Zspace_covered_rate),
                               guard_x = mean(data$Zprox_mid_guard),
                               survspace_x = mean(data$Zsurv_space_covered_rate))

survspace_newdat <- data.frame(survspace_x = data$Zsurv_space_covered_rate,
                               space_x = mean(data$Zspace_covered_rate),
                               speed_x = mean(data$Zspeed),
                               guard_x = mean(data$Zprox_mid_guard),
                               survspeed_x = mean(data$Zsurv_speed))
                           

speed_mm <- model.matrix(~speed_x + space_x + guard_x + survspeed_x + survspace_x, speed_newdat)
speed_y <- speed_mm%*%fixef(base_model)

space_mm <- model.matrix(~speed_x + guard_x + space_x + survspeed_x + survspace_x, space_newdat)
space_y <- space_mm%*%fixef(base_model)

guard_mm <- model.matrix(~speed_x + guard_x + space_x + survspeed_x + survspace_x, guard_newdat)
guard_y <- guard_mm%*%fixef(base_model)

survspeed_mm <- model.matrix(~speed_x + guard_x + space_x + survspeed_x + survspace_x, survspeed_newdat)
survspeed_y <- survspeed_mm%*%fixef(base_model)

survspace_mm <- model.matrix(~speed_x + guard_x + space_x + survspeed_x + survspace_x, survspace_newdat)
survspace_y <- survspace_mm%*%fixef(base_model)

plotResiduals(simulationOutput, speed_y) # around the line
plotResiduals(simulationOutput, space_y) # deviation a the tails
plotResiduals(simulationOutput, guard_y) # strong deviation right side
plotResiduals(simulationOutput, survspeed_y) # quadratic curve
plotResiduals(simulationOutput, survspace_y) # strong deviation on the right side
# These deviations could be considered normal since there is so much variation in the data
# We need to model quadratic and correlational effects!

# Test model fit, dispersion and outliers
testDispersion(simulationOutput, plot = T) # tells me again that it's underdispersed
testOutliers(simulationOutput, plot = T)
testUniformity(simulationOutput, plot = T)
# =======================================================================
# =======================================================================





# =======================================================================
# 6. Test main and random effects using BIC
# =======================================================================
system.time(model_nospeed <- glmer(cbind(hunting_success, 4 - hunting_success) ~
                                              Zprox_mid_guard +
                                              Zspace_covered_rate +
                                              Zsurv_speed +
                                              Zsurv_space_covered_rate +
                                              (1 | map_name) +
                                              (1 | mirrors_id) +
                                              (1 | obs),
                                           control = glmerControl(optimizer = "bobyqa"),
                                           family = binomial,
                                           data = data))

system.time(model_nospace <- glmer(cbind(hunting_success, 4 - hunting_success) ~
                                              Zspeed +
                                              Zprox_mid_guard +
                                              Zsurv_speed +
                                              Zsurv_space_covered_rate +
                                              (1 | map_name) +
                                              (1 | mirrors_id) +
                                              (1 | obs),
                                           control = glmerControl(optimizer = "bobyqa"),
                                           family = binomial,
                                           data = data))

system.time(model_noguard <- glmer(cbind(hunting_success, 4 - hunting_success) ~
                                              Zspeed +
                                              Zspace_covered_rate +
                                              Zsurv_speed +
                                              Zsurv_space_covered_rate +
                                              (1 | map_name) +
                                              (1 | mirrors_id) +
                                              (1 | obs),
                                           control = glmerControl(optimizer = "bobyqa"),
                                           family = binomial,
                                           data = data))

system.time(model_nosurvspeed <- glmer(cbind(hunting_success, 4 - hunting_success) ~
                                              Zspeed +
                                              Zprox_mid_guard +
                                              Zspace_covered_rate +
                                              Zsurv_space_covered_rate +
                                              (1 | map_name) +
                                              (1 | mirrors_id) +
                                              (1 | obs),
                                           control = glmerControl(optimizer = "bobyqa"),
                                           family = binomial,
                                           data = data))

system.time(model_nosurvspace <- glmer(cbind(hunting_success, 4 - hunting_success) ~
                                              Zspeed +
                                              Zprox_mid_guard +
                                              Zspace_covered_rate +
                                              Zsurv_speed +
                                              (1 | map_name) +
                                              (1 | mirrors_id) +
                                              (1 | obs),
                                           control = glmerControl(optimizer = "bobyqa"),
                                           family = binomial,
                                           data = data))

system.time(model_noID <- glmer(cbind(hunting_success, 4 - hunting_success) ~
                                              Zspeed +
                                              Zprox_mid_guard +
                                              Zspace_covered_rate +
                                              Zsurv_speed +
                                              Zsurv_space_covered_rate +
                                              (1 | map_name) +
                                              (1 | obs),
                                           control = glmerControl(optimizer = "bobyqa"),
                                           family = binomial,
                                           data = data))

system.time(model_nomaps <- glmer(cbind(hunting_success, 4 - hunting_success) ~
                                              Zspeed +
                                              Zspace_covered_rate +
                                              Zprox_mid_guard +
                                              Zsurv_speed +
                                              Zsurv_space_covered_rate +
                                              (1 | mirrors_id) +
                                              (1 | obs),
                                           control = glmerControl(optimizer = "bobyqa"),
                                           family = binomial,
                                           data = data))

# BIC method
# I create a list of my models
base_models <- list(base_model, model_nospeed, model_nospace,
                           model_noguard, model_noID, model_nomaps,
                           model_nosurvspeed, model_nosurvspace)

# I create a vector of model names for the BIC tables
modnames <- c("base_model", "no speed",
              "no space", "no guard", "no IDs", "no maps",
              "surv nospeed", "surv nospace")

# I generate the BIC tables
BIC <- as.data.table(bictab(cand.set = base_models,
                            modnames = modnames,
                            sort = TRUE))

# Save the table in my outputs
save(BIC, file = "05B_BIC-table.rda")
# =======================================================================
# =======================================================================





# =======================================================================
# 7. Report summary and coefficients, save outputs
# =======================================================================
# Save summary as a .txt file
summary_base_model <- summary(base_model)
capture.output(summary_base_model,
               file = "05B_model-summary.txt")
# =======================================================================
# =======================================================================





# =======================================================================
# 8. Assess model fit (R-squared)
# =======================================================================
# Calculation by hand (supplementary material 2 from Nakagawa & al. 2017)

# Compute variance in fitted values (Fixed effects variance)
VarF <- var(as.vector(model.matrix(base_model) %*% fixef(base_model)))

# Getting the (theoretical) distribution-specific variance
VarDS <- pi^2/3

# Marginal R2
R2_M <- VarF / # Fixed effect variance
        (VarF + sum(as.numeric(VarCorr(base_model))) + VarDS) # Total variance

# Conditional R2 (OLRE is excluded in the numerator to only account for random effects)
R2_C <- (VarF + sum(as.numeric(VarCorr(base_model)[c(2,3)]))) / # Fixed effect variance + random effect variance
        (VarF + sum(as.numeric(VarCorr(base_model))) + VarDS) # Total variance

# Save r-squared values into a table
r_squared <- as.data.table(cbind(R2_M, R2_C))
save(r_squared, file = "05B_r2-table.rda")
# End of script =========================================================











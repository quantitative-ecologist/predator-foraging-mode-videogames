
#########################################################################

#                  quadratic hunting success analysis                   #

#########################################################################

# Here, we calculate the quadratic relationship between hunting traits expressed during matches and hunting success. We also include covariance terms to estimate how traits covary to affect hunting success. Hunting success = number of prey captured.

# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec
# -------------------------------------------------------------------





# =======================================================================
# 1. Set working directory, load libraries, and import dataset
# =======================================================================

setwd("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/masters_project/chapter1/outputs") # personal computer onedrive UQAM Montiglio lab

library(data.table)
library(AICcmodavg)
library(lme4)
library(lattice)
library(MuMIn)
library(DHARMa)

data <- fread("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/PhD_project/chapter1/outputs/02_merged-data.csv",
              select = c("mirrors_id", "match_id", 
                         "map_name", "hunting_success",
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
#load("03C_quadratic-model.rda")
#load("03C_BIC-table.rda")
#load("03C_fixef-table.rda")
#load("03C_ICC-table.rda")
#load("03C_r2-table.rda")
# =======================================================================
# =======================================================================





# =======================================================================
# 2. Build full model
# =======================================================================
# Tested without an OLRE
# The model was overdispersed

# Standard error estimates seem OK (not too large)
system.time(quadratic_model <- glmer(cbind(hunting_success, 4 - hunting_success) ~
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
                                        # Predator trait covariances
                                        Zspeed : Zspace_covered_rate +
                                        Zspeed : Zprox_mid_guard +
                                        Zspace_covered_rate : Zprox_mid_guard +
                                        # Predator-prey trait covariances
                                        Zspeed : Zsurv_speed +
                                        Zspeed : Zsurv_space_covered_rate +
                                        Zspace_covered_rate : Zsurv_speed +
                                        Zspace_covered_rate : Zsurv_space_covered_rate +
                                        Zprox_mid_guard : Zsurv_speed +
                                        Zprox_mid_guard : Zsurv_space_covered_rate +
                                        (1 | map_name) +
                                        (1 | mirrors_id) +
                                        (1 | obs),
                                     control = glmerControl(optimizer = "nloptwrap", 
                                                            nAGQ0initStep = FALSE),
                                     family = binomial,
                                     data = data))

save(quadratic_model, file = "03C_quadratic-model.rda")
# =======================================================================
# =======================================================================





# =======================================================================
# 3. model diagnostics
# =======================================================================
# Check overdispersion
overdisp_fun(quadratic_model)
# ratio is lower than 1 = OK


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
save(simulationOutput, file = "03C_simulated_resids.rda")
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

# Model matrixes operate from the first vector so I might have to change order???
# Test this to see. Test also if changing coefficient order changes something i.e. fixef()[c()]
speed_mm <- model.matrix(~
                            I(speed_x^2) +
                            I(space_x^2) +
                            I(guard_x^2) +
                            I(survspeed_x^2) +
                            I(survspace_x^2) +
                            speed_x * space_x +
                            speed_x * guard_x +
                            space_x * guard_x +
                            speed_x * survspeed_x +
                            speed_x * survspace_x +
                            space_x * survspeed_x +
                            space_x * survspace_x +
                            guard_x * survspeed_x +
                            guard_x * survspace_x, speed_newdat)
speed_y <- speed_mm%*%fixef(quadratic_model)

space_mm <- model.matrix(~
                            I(speed_x^2) +
                            I(space_x^2) +
                            I(guard_x^2) +
                            I(survspeed_x^2) +
                            I(survspace_x^2) +
                            speed_x * space_x +
                            speed_x * guard_x +
                            space_x * guard_x+
                            speed_x * survspeed_x +
                            speed_x * survspace_x +
                            space_x * survspeed_x +
                            space_x * survspace_x +
                            guard_x * survspeed_x +
                            guard_x * survspace_x, space_newdat)
space_y <- space_mm%*%fixef(quadratic_model)

guard_mm <- model.matrix(~
                            I(speed_x^2) +
                            I(space_x^2) +
                            I(guard_x^2) +
                            I(survspeed_x^2) +
                            I(survspace_x^2) +
                            speed_x * space_x +
                            speed_x * guard_x +
                            space_x * guard_x +
                            speed_x * survspeed_x +
                            speed_x * survspace_x +
                            space_x * survspeed_x +
                            space_x * survspace_x +
                            guard_x * survspeed_x +
                            guard_x * survspace_x, guard_newdat)
guard_y <- guard_mm%*%fixef(quadratic_model)

survspeed_mm <- model.matrix(~
                                I(speed_x^2) +
                                I(space_x^2) +
                                I(guard_x^2) +
                                I(survspeed_x^2) +
                                I(survspace_x^2) +
                                speed_x * space_x +
                                speed_x * guard_x +
                                space_x * guard_x +
                                speed_x * survspeed_x +
                                speed_x * survspace_x +
                                space_x * survspeed_x +
                                space_x * survspace_x +
                                guard_x * survspeed_x +
                                guard_x * survspace_x, survspeed_newdat)
survspeed_y <- survspeed_mm%*%fixef(quadratic_model)

survspace_mm <- model.matrix(~
                                I(speed_x^2) +
                                I(space_x^2) +
                                I(guard_x^2) +
                                I(survspeed_x^2) +
                                I(survspace_x^2) +
                                speed_x * space_x +
                                speed_x * guard_x +
                                space_x * guard_x +
                                speed_x * survspeed_x +
                                speed_x * survspace_x +
                                space_x * survspeed_x +
                                space_x * survspace_x +
                                guard_x * survspeed_x +
                                guard_x * survspace_x, survspace_newdat)
survspace_y <- survspace_mm%*%fixef(quadratic_model)

plotResiduals(simulationOutput, speed_y)  
plotResiduals(simulationOutput, space_y)
plotResiduals(simulationOutput, guard_y) 
plotResiduals(simulationOutput, survspeed_y)
plotResiduals(simulationOutput, survspace_y)

# Test model fit, dispersion and outliers
testDispersion(simulationOutput, plot = T) # tells me again that it's underdispersed
testOutliers(simulationOutput, plot = T)
testUniformity(simulationOutput, plot = T)
# =======================================================================
# =======================================================================




# =======================================================================
# 4. Build models where I take out each coefficient
# =======================================================================
# This step is to use BIC to assess parameter importance in the model

# no speed^2
system.time(no_speed2 <- glmer(cbind(hunting_success, 4 - hunting_success) ~
                                  # Quadratic terms
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
                                  # Predator trait covariances
                                  Zspeed : Zspace_covered_rate +
                                  Zspeed : Zprox_mid_guard +
                                  Zspace_covered_rate : Zprox_mid_guard +
                                  # Predator-prey trait covariances
                                  Zspeed : Zsurv_speed +
                                  Zspeed : Zsurv_space_covered_rate +
                                  Zspace_covered_rate : Zsurv_speed +
                                  Zspace_covered_rate : Zsurv_space_covered_rate +
                                  Zprox_mid_guard : Zsurv_speed +
                                  Zprox_mid_guard : Zsurv_space_covered_rate +
                                  (1 | map_name) +
                                  (1 | mirrors_id) +
                                  (1 | obs),
                               family = binomial,
                               data = data))

# no space^2
system.time(no_space2 <- glmer(cbind(hunting_success, 4 - hunting_success) ~
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

# no guard^2
system.time(no_guard2 <- glmer(cbind(hunting_success, 4 - hunting_success) ~
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

# no interaction speed*space^2
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

#  no interaction speed*guard^2
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

#  no interaction space*guard^2
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





# =======================================================================
# 5. Report summary and coefficients, save outputs
# =======================================================================
# save summary as a .txt file
s_quadratic_model <- summary(quadratic_model)
capture.output(s_quadratic_model, file = "03C_quadratic_model-summary.txt")
# =======================================================================
# =======================================================================





# =======================================================================
# 6. Assess model fit (R-squared)
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
R2_C <- (VarF + sum(as.numeric(VarCorr(quadratic_model)[c(2, 3)]))) / # Fixed effect variance + random effect variance
        (VarF + sum(as.numeric(VarCorr(quadratic_model))) + VarDS) # Total variance

# Save r-squared values into a table
r_squared <- as.data.table(cbind(R2_M, R2_C))
save(r_squared, file = "03C_r2-table.rda")
# =======================================================================
# =======================================================================









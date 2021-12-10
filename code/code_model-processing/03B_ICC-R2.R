# =======================================================================

#                   ICCs and R2 for the base models                     #

# =======================================================================





# =======================================================================
# 1. Load libraries and models
# =======================================================================


# Librairies ------------------------------------------------------------

library(data.table)
library(brms)
library(broom.helpers)



# Load models -----------------------------------------------------------

model1 <- readRDS("./outputs/models/03B_hunting_success_base-model1.rds")
model2 <- readRDS("./outputs/models/03B_hunting_success_base-model2.rds")


# =======================================================================
# =======================================================================





# =======================================================================
# 2. Assess model fit (R-squared)
# =======================================================================

# Calculation by hand 
# Based on the supplementary material 2 from Nakagawa & al. 2017


# Compute the model matrixes --------------------------------------------

# maybe I can extract the fitted values from the conditional effects

mm1 <- model_get_model_matrix(model1)
mm2 <- model_get_model_matrix(model2)



# Variance components ---------------------------------------------------

# 1. Fixed effects variance
# Compute variance in fitted values (Fixed effects variance)
fitted_tab1 <- data.table(mm1%*%fixef(model1))
fitted_tab2 <- data.table(mm2%*%fixef(model2))

var_fixef1 <- var(fitted_tab1$Estimate)
var_fixef2 <- var(fitted_tab2$Estimate)


# 2. Distribution-specific variance
# For a binomial model with OLRE
var_DS <- pi^2/3

# For beta binomial model
# (phi from the distribution)
#VarDS1 <- summary(base_model)$spec_pars[1]


# 3. Random effects variance
# Extract the random effect standard deviations
ranef1 <- data.table(
            as_draws_df(model1,
                        variable = c("^sd_"),
                        regex = TRUE))

ranef2 <- data.table(
            as_draws_df(model2,
                        variable = c("^sd_"),
                        regex = TRUE))

# Standard deviations to variances
ranef1[, c("map_var", "obs_var", "player_var") := 
        lapply(.SD, function(x) {x^2}),
          .SDcols = c(1:3)][, c(4:6) := NULL]

ranef2[, c("map_var", "obs_var", "player_var") := 
        lapply(.SD, function(x) {x^2}),
          .SDcols = c(1:3)][, c(4:6) := NULL]

# Variance in maps and players
var_ranef1 <- mean(ranef1$player_var) + mean(ranef1$map_var)
var_ranef2 <- mean(ranef2$player_var) + mean(ranef2$map_var)

# Observation-level variance
var_SE1 <- mean(ranef1$obs_var)
var_SE2 <- mean(ranef2$obs_var)


# 4. Total variance
# binomial model with OLRE
var_T1 <- var_fixef1 + var_ranef1 + var_SE1 + var_DS
var_T2 <- var_fixef2 + var_ranef2 + var_SE2 + var_DS

# beta-binomial model
#VarT1 <- VarF1 + VarR1 + VarDS1


# 5. Compute R-squared values
# Marginal R2
R2_marginal1 <- var_fixef1 /
                var_T1
R2_marginal2 <- var_fixef2 /
                var_T2

# Conditional R2
# (OLRE is excluded in the numerator to only account for random effects)
 # Fixed effect variance + random effect variance / total variance
R2_conditional1 <- (var_fixef1 + var_ranef1) /
                    var_T1
R2_conditional2 <- (var_fixef2 + var_ranef2) /
                    var_T2

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Compute ICCs and their 95% credibility intervals
# =======================================================================


# Compute total variance ------------------------------------------------            
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
icc_tab <- data.table(group = c("id", "map", "obs"),
                      mean = as.numeric(ran_var[, lapply(.SD, mean),
                                                  .SDcols = c(9:11)]),
                      rbind(coda::HPDinterval(as.mcmc(ran_var[,9]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,10]), 0.95),
                            coda::HPDinterval(as.mcmc(ran_var[,11]), 0.95)
                            )
                       )

icc_tab2 <- data.table(group = c("id", "map", "obs"),
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

r2_tab <- as.data.table(cbind(R2_M1, R2_C1, R2_M2, R2_C2))

# Round values for icc table
round_val <- function (x) {round(x, digits = 3)}
icc_tab[, c("mean", "lower", "upper") :=
                lapply(.SD, round_val), 
                .SDcols = c(2:4)][, model := "model1"]

icc_tab2[, c("mean", "lower", "upper") :=
                lapply(.SD, round_val), 
                .SDcols = c(2:4)][, model := "model2"]

# Bind both tables
icc_table <- rbind(icc_tab, icc_tab2)

capture.output(r2_tab, file = "./outputs/03B_r2-table.txt")
capture.output(icc_table, file = "./outputs/03B_icc-table.txt")

# =======================================================================
# =======================================================================
































# =======================================================================
# 2. Compute ICCs and their 95% credibility intervals
# =======================================================================


# Extract random effect standard deviations -----------------------------

ran_var1 <- data.table(
                as_draws_df(model1,
                            variable = c("^sd_", "sigma_"),
                            regex = TRUE))

ran_var2 <- data.table(
                as_draws_df(model2,
                            variable = c("^sd_", "sigma_"),
                            regex = TRUE))

ran_var1[, c(17:23) := NULL]
ran_var2[, c(17:23) := NULL]
ran_var3[, c(17:23) := NULL]
ran_var4[, c(17:23) := NULL]



# Compute variances for each random effect ------------------------------

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

ran_var4[, c("speedvar_char", "spacevar_char", "guardvar_char", "hookvar_char",
            "speedvar_map", "spacevar_map", "guardvar_map", "hookvar_map", 
            "speedvar_id", "spacevar_id", "guardvar_id", "hookvar_id",
            "speedvar_resid", "spacevar_resid", "guardvar_resid", "hookvar_resid") := 
          lapply(.SD, function(x) {x^2}),
            .SDcols = c(1:16)][, c(1:16) := NULL]



# Compute total variance ------------------------------------------------

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

ran_var4[, speedvar_total := rowSums(ran_var4[, c(1,5,9,13)])]
ran_var4[, spacevar_total := rowSums(ran_var4[, c(2,6,10,14)])]
ran_var4[, guardvar_total := rowSums(ran_var4[, c(3,7,11,15)])]
ran_var4[, hookvar_total := rowSums(ran_var4[, c(4,8,12,16)])]



# Calculate ICCs --------------------------------------------------------

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

ran_var4[, speedicc_id := speedvar_id / speedvar_total]
ran_var4[, spaceicc_id := spacevar_id / spacevar_total]
ran_var4[, guardicc_id := guardvar_id / guardvar_total]
ran_var4[, hookicc_id := hookvar_id / hookvar_total]


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

ran_var4[, speedicc_map := speedvar_map / speedvar_total]
ran_var4[, spaceicc_map := spacevar_map / spacevar_total]
ran_var4[, guardicc_map := guardvar_map / guardvar_total]
ran_var4[, hookicc_map := hookvar_map / hookvar_total]


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

ran_var4[, speedicc_char := speedvar_char / speedvar_total]
ran_var4[, spaceicc_char := spacevar_char / spacevar_total]
ran_var4[, guardicc_char := guardvar_char / guardvar_total]
ran_var4[, hookicc_char := hookvar_char / hookvar_total]


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

ran_var4[, speedicc_resid := speedvar_resid / speedvar_total]
ran_var4[, spaceicc_resid := spacevar_resid / spacevar_total]
ran_var4[, guardicc_resid := guardvar_resid / guardvar_total]
ran_var4[, hookicc_resid := hookvar_resid / hookvar_total]



# Create table with mean icc and credibility interval -------------------

lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}

icc_tab1 <- data.table(group = c("speedicc_id", "spaceicc_id",
                                 "guardicc_id", "hookicc_id",
                                "speedicc_map", "spaceicc_map",
                                "guardicc_map", "hookicc_map",
                                "speedicc_char", "spaceicc_char",
                                "guardicc_char", "hookicc_char",
                                "speedicc_resid", "spaceicc_resid",
                                "guardicc_resid", "hookicc_resid"),
                       mean = as.numeric(ran_var1[, lapply(.SD, mean),
                                                   .SDcols = c(21:36)]),
                       lower = as.numeric(ran_var1[, lapply(.SD, lower_interval),
                                                   .SDcols = c(21:36)]),
                       upper = as.numeric(ran_var1[, lapply(.SD, upper_interval),
                                                   .SDcols = c(21:36)])
                        )

icc_tab2 <- data.table(group = c("speedicc_id", "spaceicc_id",
                                 "guardicc_id", "hookicc_id",
                                "speedicc_map", "spaceicc_map",
                                "guardicc_map", "hookicc_map",
                                "speedicc_char", "spaceicc_char",
                                "guardicc_char", "hookicc_char",
                                "speedicc_resid", "spaceicc_resid",
                                "guardicc_resid", "hookicc_resid"),
                       mean = as.numeric(ran_var2[, lapply(.SD, mean),
                                                   .SDcols = c(21:36)]),
                       lower = as.numeric(ran_var2[, lapply(.SD, lower_interval),
                                                   .SDcols = c(21:36)]),
                       upper = as.numeric(ran_var2[, lapply(.SD, upper_interval),
                                                   .SDcols = c(21:36)])
                        )

icc_tab3 <- data.table(group = c("speedicc_id", "spaceicc_id",
                                 "guardicc_id", "hookicc_id",
                                "speedicc_map", "spaceicc_map",
                                "guardicc_map", "hookicc_map",
                                "speedicc_char", "spaceicc_char",
                                "guardicc_char", "hookicc_char",
                                "speedicc_resid", "spaceicc_resid",
                                "guardicc_resid", "hookicc_resid"),
                       mean = as.numeric(ran_var3[, lapply(.SD, mean),
                                                   .SDcols = c(21:36)]),
                       lower = as.numeric(ran_var3[, lapply(.SD, lower_interval),
                                                   .SDcols = c(21:36)]),
                       upper = as.numeric(ran_var3[, lapply(.SD, upper_interval),
                                                   .SDcols = c(21:36)])
                        )

icc_tab4 <- data.table(group = c("speedicc_id", "spaceicc_id",
                                 "guardicc_id", "hookicc_id",
                                "speedicc_map", "spaceicc_map",
                                "guardicc_map", "hookicc_map",
                                "speedicc_char", "spaceicc_char",
                                "guardicc_char", "hookicc_char",
                                "speedicc_resid", "spaceicc_resid",
                                "guardicc_resid", "hookicc_resid"),
                       mean = as.numeric(ran_var4[, lapply(.SD, mean),
                                                   .SDcols = c(21:36)]),
                       lower = as.numeric(ran_var4[, lapply(.SD, lower_interval),
                                                   .SDcols = c(21:36)]),
                       upper = as.numeric(ran_var4[, lapply(.SD, upper_interval),
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

icc_tab4[, ranef_variable := c(rep("id", 4),
                               rep("map", 4),
                               rep("character", 4),
                               rep("resid", 4))]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Save the tables as an r object
# =======================================================================

saveRDS(icc_tab1, file = "./outputs/R_objects/03A_icc-table1.rds")

saveRDS(icc_tab2, file = "./outputs/R_objects/03A_icc-table2.rds")

saveRDS(icc_tab3, file = "./outputs/R_objects/03A_icc-table3.rds")

saveRDS(icc_tab4, file = "./outputs/R_objects/03A_icc-table4.rds")

# =======================================================================
# =======================================================================
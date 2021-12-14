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



# Compute a table to add all variance components ------------------------

# Add fixef and ds var and calculate total variance
var_tab1 <- ranef1[, ":=" (fixef_var = var_fixef1, ds_var = var_DS)][
  , var_tot := rowSums(ranef1[, 4:8])][
    , c(1:3) := NULL]

var_tab2 <- ranef2[, ":=" (fixef_var = var_fixef2, ds_var = var_DS)][
  , var_tot := rowSums(ranef2[, 4:8])][
    , c(1:3) := NULL]



# Compute the R-squares -------------------------------------------------

var_tab1[, ":=" (r2_marginal = fixef_var / var_tot,
                 r2_conditional = (fixef_var + map_var + player_var) /
                                   var_tot)]

var_tab2[, ":=" (r2_marginal = fixef_var / var_tot,
                 r2_conditional = (fixef_var + map_var + player_var) / 
                                   var_tot)]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Compute ICCs and their 95% credibility intervals
# =======================================================================


# Calculate ICCs --------------------------------------------------------

ranef1[, c("icc_map", "icc_obs", "icc_player") := 
          lapply(.SD, function(x) x / var_tot),
            .SDcols = c(1:3)]

ranef2[, c("icc_map", "icc_obs", "icc_player") := 
          lapply(.SD, function(x) x / var_tot),
            .SDcols = c(1:3)]


# Create table with mean icc and credibility interval
icc_tab <- data.table(group = c("map", "obs", "player"),
                      mean = as.numeric(ranef1[, lapply(.SD, mean),
                                                  .SDcols = c(9:11)]),
                      rbind(coda::HPDinterval(as.mcmc(ranef1[, 9]), 0.95),
                            coda::HPDinterval(as.mcmc(ranef1[, 10]), 0.95),
                            coda::HPDinterval(as.mcmc(ranef1[, 11]), 0.95)
                            )
                       )

icc_tab2 <- data.table(group = c("map", "obs", "player"),
                      mean = as.numeric(ranef2[, lapply(.SD, mean),
                                                  .SDcols = c(9:11)]),
                      rbind(coda::HPDinterval(as.mcmc(ranef2[, 9]), 0.95),
                            coda::HPDinterval(as.mcmc(ranef2[, 10]), 0.95),
                            coda::HPDinterval(as.mcmc(ranef2[, 11]), 0.95)
                            )
                       )

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Save values in table
# =======================================================================


# R-square table --------------------------------------------------------

ci_marginal1    <- coda::HPDinterval(as.mcmc(var_tab1[, r2_marginal]), 0.95)
ci_conditional1 <- coda::HPDinterval(as.mcmc(var_tab1[, r2_conditional]), 0.95)
ci_marginal2    <- coda::HPDinterval(as.mcmc(var_tab2[, r2_marginal]), 0.95)
ci_conditional2 <- coda::HPDinterval(as.mcmc(var_tab2[, r2_conditional]), 0.95)

r2_tab <- data.table(model = cbind(c("model1", "model1", "model2", "model2")),
                     r2_type = cbind(c("r2_marginal",
                                       "r2_conditional",
                                       "r2_marginal",
                                       "r2_conditional")),
                     value = rbind(mean(var_tab1[, r2_marginal]),
                                   mean(var_tab1[, r2_conditional]),
                                   mean(var_tab2[, r2_marginal]),
                                   mean(var_tab2[, r2_conditional])),
                     rbind(ci_marginal1,
                           ci_conditional1,
                           ci_marginal2,
                           ci_conditional2))

setnames(r2_tab, old = c("model.V1", "r2_type.V1", "value.V1"),
                 new = c("model", "r2_type", "value"))



# Round values ----------------------------------------------------------

round_val <- function (x) {round(x, digits = 3)}

r2_tab[, c("value", "lower", "upper") :=
                lapply(.SD, round_val), 
                .SDcols = c(3:5)]

icc_tab[, c("mean", "lower", "upper") :=
                lapply(.SD, round_val), 
                .SDcols = c(2:4)][, model := "model1"]

icc_tab2[, c("mean", "lower", "upper") :=
                lapply(.SD, round_val), 
                .SDcols = c(2:4)][, model := "model2"]

# Bind the two icc tables
icc_table <- rbind(icc_tab, icc_tab2)



# Save the outputs ------------------------------------------------------

capture.output(r2_tab,
               file = "./outputs/model_diagnostics/03B_r2-table.txt")
capture.output(icc_table,
               file = "./outputs/model_diagnostics/03B_icc-table.txt")

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
# =======================================================================

#                 ICCs and R2 for the quadratic models                  #

# =======================================================================





# =======================================================================
# 1. Load libraries and models
# =======================================================================


# Librairies ------------------------------------------------------------

library(data.table)
library(brms)
library(broom.helpers)



# Load models -----------------------------------------------------------

model1 <- readRDS("./outputs/models/03C_hunting_success_quadratic-model1.rds")
model2 <- readRDS("./outputs/models/03C_hunting_success_quadratic-model2.rds")


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

ci_marginal1    <- coda::HPDinterval(as.mcmc(var_tab1[, r2_marginal]),
                                     0.95)
ci_conditional1 <- coda::HPDinterval(as.mcmc(var_tab1[, r2_conditional]),
                                     0.95)
ci_marginal2    <- coda::HPDinterval(as.mcmc(var_tab2[, r2_marginal]),
                                     0.95)
ci_conditional2 <- coda::HPDinterval(as.mcmc(var_tab2[, r2_conditional]),
                                     0.95)

r2_tab <- data.table(model = cbind(c("quadratic model1",
                                     "quadratic model1",
                                     "quadratic model2",
                                     "quadratic model2")),
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
                .SDcols = c(2:4)][, model := "quadratic model1"]

icc_tab2[, c("mean", "lower", "upper") :=
                lapply(.SD, round_val), 
                .SDcols = c(2:4)][, model := "quadratic model2"]

# Bind the two icc tables
icc_table <- rbind(icc_tab, icc_tab2)



# Save the outputs ------------------------------------------------------

saveRDS(r2_tab, file = "./outputs/R_objects/03C_r2-table.rds")
saveRDS(icc_table, file = "./outputs/R_objects/03C_icc-table.rds")

# =======================================================================
# =======================================================================
#########################################################################

#                   ICCs for the multivariate model                      #

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
load("./outputs/03A_multivariate-model.rda")
print(object.size(base_model), units = "MB")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Compute ICCs and their 95% credibility intervals
# =======================================================================

# Extract posterior draws of variance components
variances <- posterior_samples(mv_model, 
                               pars = c("sigma",
                                        "sd_mirrors_id__Intercept",
                                        "sd_map_name__Intercept",
                                        "sd_character_name__Intercept"),
                               as.matrix = TRUE)

id <- variances[, 1]^2 / (variances[, 1]^2 + 
                          variances[, 2]^2 +
                          variances[, 3]^2 +
                          variances[, 4]^2)

map <- variances[, 2]^2 / (variances[, 1]^2 + 
                           variances[, 2]^2 +
                           variances[, 3]^2 +
                           variances[, 4]^2)

avt <- variances[, 3]^2 / (variances[, 1]^2 + 
                           variances[, 2]^2 +
                           variances[, 3]^2 +
                           variances[, 4]^2)

res <- variances[, 4] / (variances[, 1]^2 + 
                         variances[, 2]^2 +
                         variances[, 3]^2 +
                         variances[, 4]^2)

icc_id <- mean(id);coda::HPDinterval(as.mcmc(id), 0.95)
icc_map <- mean(map);coda::HPDinterval(as.mcmc(map), 0.95)
icc_avt <- mean(avt);coda::HPDinterval(as.mcmc(map), 0.95)
icc_res <- mean(res);coda::HPDinterval(as.mcmc(obs), 0.95)

# =======================================================================
# =======================================================================

variances <- setDT(posterior_samples(base_model, 
                               pars = c("sigma",
                                        "sd_mirrors_id__Intercept",
                                        "sd_map_name__Intercept",
                                        "sd_character_name__Intercept")))
variances[, 1]^2


# =======================================================================
# 4. Save values in table
# =======================================================================

table1 <- as.data.table(cbind(R2_M1, R2_C1, 
                              icc_id, icc_map, icc_obs))

capture.output(table1, file = "./outputs/03B_icc_r2-table.txt")

# =======================================================================
# =======================================================================

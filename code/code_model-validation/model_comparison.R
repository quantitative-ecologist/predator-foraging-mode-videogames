# =======================================================================

#               LOO-CV table for hunting success models                 #

# =======================================================================





# =======================================================================
# 1. Load libraries and loo objects
# =======================================================================


# Librairies ------------------------------------------------------------

library(data.table)
library(brms)



# Load loo objects ------------------------------------------------------

loo1 <- readRDS("./outputs/R_objects/03B_loo_base-model1.rds")
loo2 <- readRDS("./outputs/R_objects/03B_loo_base-model2.rds")
loo3 <- readRDS("./outputs/R_objects/03c_loo_quadratic-model1.rds")
loo4 <- readRDS("./outputs/R_objects/03c_loo_quadratic-model2.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Compare the models with the loo table
# =======================================================================


# Compute the table -----------------------------------------------------

loo_table <- loo_compare(loo1, loo2, loo3, loo4)

loo_table <- data.table(print(loo_table, simplify = FALSE),
                        keep.rownames = TRUE)



# Arrange the table -----------------------------------------------------

# Change the first column name
setnames(loo_table, "rn", "model")

# Change the model names for proper names
loo_table[1, 1 := "quadratic model2"]
loo_table[2, 1 := "quadratic model1"]
loo_table[3, 1 := "base model2"]
loo_table[4, 1 := "base model1"]

# Keep the columns of interest
loo_table[, c(6:9) := NULL]

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Export the LOO-CV table
# =======================================================================

saveRDS(loo_table, file = "./outputs/R_objects/loo-cv-table.rds")

# =======================================================================
# =======================================================================
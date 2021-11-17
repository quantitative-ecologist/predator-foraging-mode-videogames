#########################################################################

#                   Create a table to produce Table I                   #

#########################################################################

# Code to produce the values reported in Table I
# Extract coefficients from all the hunting success models
# -----------------------------------------------------------------------





# =======================================================================
# 1. Load libraries and models
# =======================================================================

# Librairies
library(data.table)
library(brms)


# Load the base models
base_model1 <- readRDS("./outputs/03B_hunting_success_base-model1.rds")
load("./outputs/03B_hunting_success_base-model2.rda")
base_model2 <- base_model
rm(base_model)


# Load the quadratic models
quadratic_model1 <- readRDS("./outputs/03C_hunting_success_quadratic-model1.rds")
load("./outputs/03C_hunting_success_quadratic-model2.rda")
quadratic_model2 <- quadratic_model
rm(quadratic_model)
# =======================================================================
# =======================================================================





# =======================================================================
# 2. Extract the model coefficients
# =======================================================================

# Compute the tables
table1 <- data.table(fixef(base_model1), keep.rownames = TRUE)
table2 <- data.table(fixef(base_model2), keep.rownames = TRUE)
table3 <- data.table(fixef(quadratic_model1), keep.rownames = TRUE)
table4 <- data.table(fixef(quadratic_model2), keep.rownames = TRUE)

# Filter out rows and create a model variable
table1[, model := "model1"]
table2 <- table2[6:7][, model := "model2"]
table3 <- table3[c(2:5, 10:15)][, model := "model3"]
table4 <- table4[c(6:7, 20:27)][, model := "model4"]
# =======================================================================
# =======================================================================





# =======================================================================
# 3. Merge tables
# =======================================================================

# Combine the tables
table <- rbind(table1, table2, table3, table4)

# Round values to 3 digits
table[, c("Estimate", "Est.Error", "Q2.5", "Q97.5") :=
        lapply(.SD, function (x) round(x, digits = 3)), 
      .SDcols = c(2:5)]

# Remove error column and change name "rn" to "coefficient"
table[, Est.Error := NULL]
setnames(table, "rn", "coefficient")

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Save the table
# =======================================================================

saveRDS(table, file = "./outputs/05_hunting_success_coef-table.rds")

# =======================================================================
# =======================================================================


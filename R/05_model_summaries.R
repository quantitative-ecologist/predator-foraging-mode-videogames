##########################################################

#               Print all model summaries                #

##########################################################





# ========================================================
# Load librairies and models
# ========================================================
# Load libraries
library(data.table)

# Load the multivariate models
load("./outputs/03A_multivariate-model1.rda")
mv_model1 <- mv_model
rm(mv_model)

load("./outputs/03A_multivariate-model2.rda")
mv_model2 <- mv_model
rm(mv_model)

load("./outputs/03A_multivariate-model3.rda")

# Load the base hunting success models
base_model1 <- readRDS("./outputs/03B_hunting_success_base-model1.rds")

load("./outputs/03B_hunting_success_base-model2.rda")
base_model2 <- base_model
rm(base_model)

# Load the quadratic hunting success models
quadratic_model1 <- readRDS("./outputs/03C_hunting_success_quadratic-model1.rds")

load("./outputs/03C_hunting_success_quadratic-model2.rda")
quadratic_model2 <- quadratic_model
rm(quadratic_model)

# ========================================================
# ========================================================





# ========================================================
# Report summary and coefficients, save outputs
# ========================================================
# Save summaries as a .txt file
sum_mv_model1 <- summary(mv_model1)
sum_mv_model2 <- summary(mv_model2)

sum_base_model1 <- summary(base_model1)
sum_base_model2 <- summary(base_model2)

sum_quadratic_model1 <- summary(quadratic_model1)
sum_quadratic_model2 <- summary(quadratic_model2)

capture.output(mv_model1, mv_model2, 
               base_model1, base_model2,
               quadratic_model1, quadratic_model2,
               file = "./outputs/05_model_summaries.txt")
# ========================================================
# ========================================================
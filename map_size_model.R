

library(data.table)
library(brms)

data <- fread("./data/merged-data.csv")
map_dat <- fread("./data/clea_map-data.csv")

load("./outputs/03A_multivariate-model1.rda")



# merge map size

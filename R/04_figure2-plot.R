#########################################################################

#                     Behavioural correlations plot                     #

#########################################################################

# Code to produce Figure 2 in Fraser Franco et al. 2021.
# Powerpoint was used to produce the final adjustements

# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec
# -----------------------------------------------------------------------





# =======================================================================
# 1. Load libraries, and export dataset
# =======================================================================

# Import libraries
library(data.table)
library(brms)
library(corrplot)
library(export)

# Load data
load("./outputs/03A_multivariate-model.rda")
load("./outputs/03A_icc-table.RDS")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare the tables
# =======================================================================

# Extract correlation samples
correlations <- data.table(posterior_samples(mv_model)[,c(25:42, 51:56)])

# Create table with mean icc and credibility intervals
lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}

cor_tab <- data.table(group = c("speed~space_char", "speed~guard_char", "space~guard_char", 
                                "speed~hook_char", "space~hook_char", "guard~hook_char",
                                "speed~space_map", "speed~guard_map", "space~guard_map", 
                                "speed~hook_map", "space~hook_map", "guard~hook_map",
                                "speed~space_id", "speed~guard_id", "space~guard_id", 
                                "speed~hook_id", "space~hook_id", "guard~hook_id",
                                "speed~space_res", "speed~guard_res", "space~guard_res", 
                                "speed~hook_res", "space~hook_res", "guard~hook_res"),
                      mean = as.numeric(correlations[, lapply(.SD, mean),
                                                  .SDcols = c(1:24)]),
                      lower = as.numeric(correlations[, lapply(.SD, lower_interval),
                                                  .SDcols = c(1:24)]),
                      upper = as.numeric(correlations[, lapply(.SD, upper_interval),
                                                  .SDcols = c(1:24)])
                       )
cor_tab[, ranef_variable := c(rep("character", 6),
                              rep("map", 6),
                              rep("id", 6),
                              rep("resid", 6))]


# ---------------------------------------------------
# ID and residual correlation matrix
# ---------------------------------------------------
id_cor <- cor_tab[ranef_variable %in% c("id", "resid"), .(group, mean)]

# ICC values are on the diagonals
speed <- c(
      as.numeric(icc_tab[1,2]),
      as.numeric(id_cor[1,2]),
      as.numeric(id_cor[2,2]),
      as.numeric(id_cor[4,2])
)

space <- c(
      as.numeric(id_cor[7,2]),
      as.numeric(icc_tab[2,2]),
      as.numeric(id_cor[3,2]),
      as.numeric(id_cor[5,2])
)

ambush <- c(
      as.numeric(id_cor[8,2]),
      as.numeric(id_cor[9,2]),
      as.numeric(icc_tab[3,2]),
      as.numeric(id_cor[6,2])
)

time <- c(
      as.numeric(id_cor[10,2]),
      as.numeric(id_cor[11,2]),
      as.numeric(id_cor[12,2]),
      as.numeric(icc_tab[4,2])
)


id_cor_matrix <- cbind(speed, space, ambush, time)
rownames(id_cor_matrix) <- c("speed", "space", "time ambush", "time 1st cap.")
colnames(id_cor_matrix) <- c("speed", "space", "time ambush", "time 1st cap.")
# ---------------------------------------------------


# ---------------------------------------------------
# Maps correlation matrix
# ---------------------------------------------------
map_cor <- cor_tab[ranef_variable %in% "map", .(group, mean)]

# ICC values are on the diagonals
speed <- c(
      as.numeric(icc_tab[5,2]),
      as.numeric(map_cor[1,2]),
      as.numeric(map_cor[2,2]),
      as.numeric(map_cor[4,2])
)

space <- c(
      NA,
      as.numeric(icc_tab[6,2]),
      as.numeric(map_cor[3,2]),
      as.numeric(map_cor[5,2])
)

ambush <- c(
      NA,
      NA,
      as.numeric(icc_tab[7,2]),
      as.numeric(map_cor[6,2])
)

time <- c(
      NA,
      NA,
      NA,
      as.numeric(icc_tab[8,2])
)


map_cor_matrix <- cbind(speed, space, ambush, time)
rownames(map_cor_matrix) <- c("speed", "space", "time ambush", "time 1st cap.")
colnames(map_cor_matrix) <- c("speed", "space", "time ambush", "time 1st cap.")
# ---------------------------------------------------


# ---------------------------------------------------
# Avatars correlation matrix
# ---------------------------------------------------
char_cor <- cor_tab[ranef_variable %in% "character", .(group, mean)]

# ICC values are on the diagonals
speed <- c(
      as.numeric(icc_tab[9,2]),
      as.numeric(char_cor[1,2]),
      as.numeric(char_cor[2,2]),
      as.numeric(char_cor[4,2])
)

space <- c(
      NA,
      as.numeric(icc_tab[10,2]),
      as.numeric(char_cor[3,2]),
      as.numeric(char_cor[5,2])
)

ambush <- c(
      NA,
      NA,
      as.numeric(icc_tab[11,2]),
      as.numeric(char_cor[6,2])
)

time <- c(
      NA,
      NA,
      NA,
      as.numeric(icc_tab[12,2])
)


char_cor_matrix <- cbind(speed, space, ambush, time)
rownames(char_cor_matrix) <- c("speed", "space", "time ambush", "time 1st cap.")
colnames(char_cor_matrix) <- c("speed", "space", "time ambush", "time 1st cap.")
# ---------------------------------------------------
# =======================================================================
# =======================================================================





# =======================================================================
# 3. Create different correlation plots and save in figure 2
# =======================================================================

# --------------------------------------------------
# Version with colors
# --------------------------------------------------
corrplot(id_cor_matrix, type = "full", method = "ellipse", 
                        cl.pos = "r", cl.cex = .85, tl.pos = "lt", tl.col = "black", 
                        tl.cex = 1.1, tl.srt = 45, number.digits = 3,
                        addCoef.col = "black", 
                        col = RColorBrewer::brewer.pal(n = 10, name = "RdBu"), 
                        mar = c(0,0,0,0))
graph2ppt(file = "./outputs/04_figure2.pptx", 
          width = 10, height = 6)

corrplot(map_cor_matrix, type = "lower", method = "ellipse", 
                        cl.pos = "r", cl.cex = .85, tl.pos = "lt", tl.col = "black", 
                        tl.cex = 1.1, tl.srt = 45, number.digits = 3,
                        addCoef.col = "black", 
                        col = RColorBrewer::brewer.pal(n = 10, name = "RdBu"), 
                        mar = c(0,0,0,0))
graph2ppt(file = "./outputs/04_figure2.pptx", 
          width = 10, height = 6, append = TRUE)

corrplot(char_cor_matrix, type = "lower", method = "ellipse", 
                        cl.pos = "r", cl.cex = .85, tl.pos = "lt", tl.col = "black", 
                        tl.cex = 1.1, tl.srt = 45, number.digits = 3,
                        addCoef.col = "black", 
                        col = RColorBrewer::brewer.pal(n = 10, name = "RdBu"), 
                        mar = c(0,0,0,0))
graph2ppt(file = "./outputs/04_figure2.pptx", 
          width = 10, height = 6, append = TRUE)
# --------------------------------------------------
# --------------------------------------------------



# --------------------------------------------------
# Black and gray version
# --------------------------------------------------
corrplot(id_cor_matrix, type = "full", method = "ellipse", 
         cl.pos = "r", cl.cex = .85, tl.pos = "lt", tl.col = "black", 
         tl.cex = 1.1, tl.srt = 45, number.digits = 3,
         addCoef.col = "#7e7c7c", 
         col = c("black", "lightgray"), 
         mar = c(0,0,0,0))
graph2ppt(file = "./outputs/04_figure2.pptx", 
          width = 10, height = 6, append = TRUE)

corrplot(map_cor_matrix, type = "full", method = "ellipse", 
         cl.pos = "r", cl.cex = .85, tl.pos = "lt", tl.col = "black", 
         tl.cex = 1.1, tl.srt = 45, number.digits = 3,
         addCoef.col = "#7e7c7c", 
         col = c("black", "lightgray"), 
         mar = c(0,0,0,0))
graph2ppt(file = "./outputs/04_figure2.pptx", 
          width = 10, height = 6, append = TRUE)

# --------------------------------------------------
# --------------------------------------------------

# =======================================================================
# =======================================================================
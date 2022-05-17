
# =========================================================================

#           Principal component analysis on predator behaviour            #

# =========================================================================

# Code to produce the PCA and Figure S1

# -------------------------------------------------------------------------





# =========================================================================
# 1. Load the librairies and import the data
# =========================================================================

# Activate project environment --------------------------------------------

# Activates automatically with the .Rprofile file
#renv::activate()



# Librairies --------------------------------------------------------------

library(data.table)
library(missMDA)
library(ggcorrplot)
library(factoextra)
library(FactoMineR)
library(cowplot)
library(ggpubr)



# Load the data -----------------------------------------------------------

data <- fread("./data/FraserFrancoetal2022-data.csv")

# Divide variables by match duration
data [, ":=" (prop_closet_open = closet_open / game_duration,
              prop_hit_far_count = hit_far_count / game_duration,
              prop_pallet_destroyed = pallet_destroyed / game_duration,
              prop_damage_generator = DamageGenerator / game_duration,
              prop_prox_mid = prox_mid_PreyGuarding / game_duration,
              prop_hook_start_time = hook_start_time / game_duration)]

# Select the columns to be analyzed
# Here we use the proportion data to control for game duration
matrix <- data[, .(speed, space_covered_rate, prop_prox_mid,
                   prop_closet_open, prop_pallet_destroyed,
                   prop_damage_generator, prop_hook_start_time,
                   prop_hit_far_count
                   )]



# Transform --------------------------------------------------------------

# Transform the data even though it is not perfect
matrix[, ":=" (prop_prox_mid = sqrt(prop_prox_mid),
               prop_hit_far_count = sqrt(prop_hit_far_count),
               prop_closet_open = sqrt(prop_closet_open),
               prop_pallet_destroyed = sqrt(prop_pallet_destroyed),
               prop_damage_generator = sqrt(prop_damage_generator),
               prop_hook_start_time = sqrt(prop_hook_start_time))]



# Standardize the variables (Z-scores) ------------------------------------

# Create the function
standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / 
                              sd(x, na.rm = TRUE)}

# Apply the function and create new columns
# The function standardizes the variables by group :
# in this case, by level of experience

matrix[, c("travel speed", "rate of space covered", 
           "prey guarding",
           "closets opened",
           "pallets destroyed", "generators damaged",
           "time before 1st capture",
           "number of attacks") :=
       lapply(.SD, standardize), 
       .SDcols = c(1:8)]

names(matrix)

#matrix <- matrix[, c(8:14)]
matrix <- matrix[, c(9:16)]

# =========================================================================
# =========================================================================





# =========================================================================
# 2. Visualize correlation matrixes
# =========================================================================


# Compute correlation matrix ----------------------------------------------

cor_matrix <- cor(na.omit(matrix)) # Pearson r linear correlation
cor_matrix <- round(cor_matrix, 3) # Rounds the coefficients to 2 decimal points



# Correlation plot --------------------------------------------------------
cor_plot <- ggcorrplot(cor_matrix,
                       hc.order = T,
                       type = "lower",
                       lab = TRUE,
                       #p.mat = cor_pmat(cor_matrix),
                       digits = 3) +
      scale_fill_gradient2(name = "Pearson\ncorrelation\n",
                           low = "red", mid = "white", high = "blue",
                           midpoint = 0, limit = c(-1, 1))

# =========================================================================
# =========================================================================





# =========================================================================
# 3. Compute PCA using the FactoMineR package
# =========================================================================


# Impute missing data -----------------------------------------------------

nb <- estim_ncpPCA(matrix, scale = FALSE)
comp <- imputePCA(matrix, ncp = 3, scale = FALSE)



# FactoMineR method : singular value decomposition ------------------------

pca_fit <- PCA(comp$completeObs, graph = FALSE, scale.unit = FALSE)

# spectral decomposition (gives the same results)
#PCA_fullZ1 <- princomp(full_Zmatrix, cor = FALSE, scores = TRUE)

# Save the results
pc_results <- get_pca_var(pca_fit)

# =========================================================================
# =========================================================================





# =========================================================================
# 4. Scree plot
# =========================================================================

eigen <- get_eigenvalue(pca_fit)

scree_plot <- fviz_eig(pca_fit, addlabels = TRUE) +
  theme(axis.line = element_line(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain")) +
  ylab("Percentage of explained variance\n") +
  xlab("\nPC axes")

# =========================================================================
# =========================================================================





# =========================================================================
# 5. Contribution of variables to selected PCA axes
# =========================================================================


# Contribution of variables to PC1 ----------------------------------------

contrib_PC1 <- fviz_contrib(pca_fit,
                            choice = "var",
                            axes = 1,
                            top = 9) +
                  theme(axis.line = element_line(),
                        axis.title = element_text(size = 14,
                                                  face = "bold"),
                        axis.text.x = element_text(face = "bold",
                                                   color = "black",
                                                   size = 14,
                                                   angle = 45)) +
                    ylab("Contribution (%)\n")



# Contribution of variables to PC2 ----------------------------------------

contrib_PC2 <- fviz_contrib(pca_fit,
                            choice = "var",
                            axes = 2,
                            top = 9) +
                  theme(axis.line = element_line(),
                        axis.title = element_text(size = 14,
                                                  face = "bold"),
                        axis.text.x = element_text(face = "bold",
                                                   color = "black",
                                                   size = 14,
                                                   angle = 45)) +
                  ylab("Contribution (%)\n")



# Contribution of variables to PC3 ----------------------------------------

contrib_PC3 <- fviz_contrib(pca_fit,
                            choice = "var",
                            axes = 3,
                            top = 9) +
                  theme(axis.line = element_line(),
                        axis.title = element_text(size = 14,
                                                  face = "bold"),
                        axis.text.x = element_text(face = "bold",
                                                   color = "black",
                                                   size = 14,
                                                   angle = 45)) +
                  ylab("Contribution (%)\n")

# =========================================================================
# =========================================================================





# =========================================================================
# 6. Generate PCA biplots
# =========================================================================


# Principal components 1 and 2 --------------------------------------------

biplot12 <- fviz_pca_biplot(pca_fit,
                            col.var = "black",
                            col.ind = "dimgray", #571A44 #96579e
                            alpha.ind = 0.2,
                            arrowsize = 1,
                            geom.ind = "point",
                            pointshape = 16,
                            pointsize = 1,
                            labelsize = 6,
                            select.var = list(contrib = 9),
                            repel = TRUE) + # no text overlap
#  scale_x_continuous(breaks = seq(-6, 6, 2), limits = c(-6, 6)) +
#  scale_y_continuous(breaks = seq(-6, 6, 2), limits = c(-7.5, 7.5)) +
  background_grid(major = "none") +
  theme(panel.border = element_rect(fill = NA, size = 0.95),
        axis.text.x = element_text(face = "plain",
                                   size = 14,
                                   color = "black"),
        axis.text.y = element_text(face = "plain",
                                   size = 14,
                                   color = "black"),
        axis.ticks.length = unit(.15, "cm"),
        axis.ticks = element_line(size = 0.90),
        plot.margin = margin(0.1, 0.5, 0.2, 0.3, "cm"))

biplot12 <- ggpubr::ggpar(biplot12,
                          xlab = "\nPC1 (20.9% of explained variance)",
                          ylab = "PC2 (19.6% of explained variance)\n",
                          title = "",
                          font.x = c(15, "plain"),
                          font.y = c(15, "plain"))



# Principal components 1 and 3 --------------------------------------------

biplot23 <- fviz_pca_biplot(pca_fit,
                            col.var = "black",
                            axes = c(2,3),
                            col.ind = "dimgray", # firebrick3
                            alpha.ind = 0.2,
                            arrowsize = 1,
                            geom.ind = "point",
                            pointshape = 16,
                            pointsize = 1,
                            labelsize = 6,
                            select.var = list(contrib = 9),
                            repel = TRUE) + # no text overlap
  #scale_y_continuous(breaks = seq(-10, 10, 5), limits = c(-10, 10)) +
  #scale_x_continuous(breaks = seq(-10, 10, 5), limits = c(-10, 10)) +
  background_grid(major = "none") +
  theme(panel.border = element_rect(fill = NA, size = 0.95),
        axis.text.x = element_text(face = "plain", size = 14, color = "black"),
        axis.text.y = element_text(face = "plain", size = 14, color = "black"),
        axis.ticks.length = unit(.15, "cm"),
        axis.ticks = element_line(size = 0.90),
        plot.margin = margin(0.1, 0.5, 0.2, 0.5, "cm"))

biplot23 <- ggpubr::ggpar(biplot23,
                          xlab = "\nPC2 (21.0% of explained variance)",
                          ylab = "PC3 (15.3% of explained variance)\n",
                          title = "",
                          font.x = c(15, "plain"),
                          font.y = c(15, "plain"))

# =========================================================================
# =========================================================================





# =========================================================================
# 7. Calculate variables having highest loading (Table SI)
# =========================================================================

# Compute the tables ------------------------------------------------------

contrib_table <- data.table(format(round(pc_results$contrib, digits = 2),
                                    nsmall = 2),
                            keep.rownames = TRUE)

corr_table <- data.table(format(round(pc_results$cor, digits = 2),
                                nsmall = 2),
                         keep.rownames = TRUE)



# Save the tables ---------------------------------------------------------

saveRDS(contrib_table,
        file = "./outputs/R_objects/02_PCA_contrib-table.rds")

saveRDS(corr_table,
        file = "./outputs/R_objects/02_PCA_corr-table.rds")

# =========================================================================
# =========================================================================





# =========================================================================
# 8. Save plots
# =========================================================================

# Ran this code once
ggexport(plotlist = list(scree_plot,
                         contrib_PC1,
                         contrib_PC2,
                         contrib_PC3),
         nrow = 1, ncol = 1,
         filename = "./outputs/figures/data_exploration/02_PCA_diagnostics-plots.pdf")

ggexport(biplot12,
         filename = "./outputs/figures/suppmat_figures/02_figureS1.png",
         width = 2000,
         height = 1800,
         res = 300)

# as one figure with the two plots
#PCA_figure <- ggarrange(PCA_fullZ_biplot12,
#                        PCA_fullZ_biplot23,
#                        ncol = 2, nrow = 1)

# End of script ==========================================================
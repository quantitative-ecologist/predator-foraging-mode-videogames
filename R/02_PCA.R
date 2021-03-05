
####################################################################################

#                       Chapter 1. Principal component analysis                    #

####################################################################################





# =========================================================================
# 1. Set working directory and load libraries
# =========================================================================
# personal computer onedrive UQAM Montiglio lab
setwd("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/PhD_project/chapter1/outputs")

library(data.table)
library(ggcorrplot)
library(factoextra)
library(FactoMineR)
library(cowplot)
library(ggpubr)
# =========================================================================
# =========================================================================





# =========================================================================
# 2. Load data and create correlation matrixes for PCA
# =========================================================================
# Full centered data matrix
# Load original data
game_data <- fread("02_merged-data.csv")

# Select column to be analyzed
full_Zmatrix <- game_data[, c(65:68, 70, 71, 73:75)]

names(full_Zmatrix)
# Change variable names
setnames(full_Zmatrix, "Zspeed", "average speed")
setnames(full_Zmatrix, "Zspace_covered_rate", "rate of space covered")
setnames(full_Zmatrix, "Zprox_mid_guard", "prey guarding")
setnames(full_Zmatrix, "Zcloset_open", "closets opened")
setnames(full_Zmatrix, "Zhit_special_count", "special attacks")
setnames(full_Zmatrix, "Zhit_far_count", "normal attacks")
setnames(full_Zmatrix, "Zpallet_destroyed", "pallets destroyed")
setnames(full_Zmatrix, "Zchase_per_second", "number of chases")
setnames(full_Zmatrix, "Zdamage_generator", "generators damaged")

#setnames(full_Zmatrix, "Zhit_close_count", "attacks while carrying")
#setnames(full_Zmatrix, "Zhook_count", "prey hooked count")
#setnames(full_Zmatrix, "Zprey_pickedup", "prey pickedup")
# =========================================================================
# =========================================================================





# =========================================================================
# 3. Visualize correlation matrixes
# =========================================================================
# Correlation among variables (visualize as a correlation matrix or as a correlation plot)
(full_matrix_pearson <- cor(full_Zmatrix)) # Pearson r linear correlation
full_matrix_pearson <- round(full_matrix_pearson, 2) # Rounds the coefficients to 2 decimal points

full_p_val_cor <- cor_pmat(full_Zmatrix)
full_cor_plot <- ggcorrplot(full_matrix_pearson, hc.order = T, type = "lower", lab = TRUE, p.mat = cor_pmat(full_Zmatrix), digits = 1)
full_cor_plot <- ggpubr::ggpar(full_cor_plot,
                          title = "\nCorrelation plot of behavior variables\n",
                          subtitle = "Player mean values\n") +
  scale_fill_gradient2(name = "Pearson\ncorrelation\n",
                       low = "blue", mid = "white", high = "red",
                       midpoint = 0, limit = c(-1, 1))
# =========================================================================
# =========================================================================





# =========================================================================
# 4. Compute PCA for the Zmatrix using FactoMineR package
# =========================================================================
# FactoMineR method : singular value decomposition
PCA_fullZ <- PCA(full_Zmatrix, graph = FALSE, scale.unit = FALSE) 
# spectral decomposition (gives the same results)
#PCA_fullZ1 <- princomp(full_Zmatrix, cor = FALSE, scores = TRUE)
# Extract variable results
var_fullZ <- get_pca_var(PCA_fullZ)
# =========================================================================
# =========================================================================





# =========================================================================
# 5. Kaiser-Guttman criterion + scree plot
# =========================================================================
get_eigenvalue(PCA_fullZ)

scree_plot <- fviz_eig(PCA_fullZ, addlabels = TRUE, ylim = c(0, 30)) +
  theme(axis.line = element_line(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain")) +
  ylab("Percentage of explained variances\n") +
  xlab("\nPC axes")
# =========================================================================
# =========================================================================





# =========================================================================
# 6. Contribution of variables to selected PCA axes
# =========================================================================
# PCA for full dataset
# Contribution of variables to PC1
contrib_PC1 <- fviz_contrib(PCA_fullZ, choice = "var", axes = 1, top = 13) +
              theme(axis.line = element_line(),
                     axis.title = element_text(size = 14, face = "bold"),
                     axis.text.x = element_text(face = "bold", color = "black",
                                                size = 14, angle = 45)) +
                ylab("Contribution (%)\n")

# Contribution of variables to PC2
contrib_PC2 <- fviz_contrib(PCA_fullZ, choice = "var", axes = 2, top = 13) +
               theme(axis.line = element_line(),
                     axis.title = element_text(size = 14, face = "bold"),
                     axis.text.x = element_text(face = "bold", color = "black",
                                                size = 14, angle = 45)) +
               ylab("Contribution (%)\n")

# Contribution of variables to PC3
contrib_PC3 <- fviz_contrib(PCA_fullZ, choice = "var", axes = 3, top = 13) +
               theme(axis.line = element_line(),
                     axis.title = element_text(size = 14, face = "bold"),
                     axis.text.x = element_text(face = "bold", color = "black",
                                                size = 14, angle = 45)) +
               ylab("Contribution (%)\n")
# =========================================================================
# =========================================================================





# =========================================================================
# 7. Generate PCA biplots for each matrix
# =========================================================================
# col.var = "contrib",
# gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # (if I used cos2)
# gradient.cols = c("pink", "purple1", "purple4", "orchid4"), # (if I used cos2)
# col.ind = grp, # (cos2) to have a contribution gradient
# palette = c("blueviolet", "orchid", "slateblue2", "violetred"),

# PCA Biplot (individuals + variables)
PCA_fullZ_biplot12 <- fviz_pca_biplot(PCA_fullZ,
                                    col.var = "black",
                                    col.ind = "#96579e", #571A44
                                    alpha.ind = 0.2,
                                    arrowsize = 1,
                                    geom.ind = "point",
                                    pointshape = 16,
                                    pointsize = 1,
                                    labelsize = 6,
                                    select.var = list(contrib = 13),
                                    repel = TRUE) + # no text overlap
  scale_y_continuous(breaks = seq(-10, 15, 5), limits = c(-10, 16)) +
  scale_x_continuous(breaks = seq(-15, 15, 5), limits = c(-18, 15)) +
  background_grid(major = "none") +
  theme(panel.border = element_rect(fill = NA, size = 0.95),
        axis.text.x = element_text(face = "plain", size = 14, color = "black"),
        axis.text.y = element_text(face = "plain", size = 14, color = "black"),
        axis.ticks.length = unit(.15, "cm"),
        axis.ticks = element_line(size = 0.90),
        plot.margin = margin(0.1, 0.5, 0.2, 0.3, "cm"))

PCA_fullZ_biplot12 <- ggpubr::ggpar(PCA_fullZ_biplot12,
                                  xlab = "\nPC1 (20.9% of explained variance)",
                                  ylab = "PC2 (18.6% of explained variance)\n",
                                  title = "",
                                  font.x = c(15, "plain"),
                                  font.y = c(15, "plain"))

# ------------------------------------------------

PCA_fullZ_biplot23 <- fviz_pca_biplot(PCA_fullZ,
                                    col.var = "black",
                                    axes = c(2,3),
                                    col.ind = "#96579e", # firebrick3
                                    alpha.ind = 0.2,
                                    arrowsize = 1,
                                    geom.ind = "point",
                                    pointshape = 16,
                                    pointsize = 1,
                                    labelsize = 6,
                                    select.var = list(contrib = 13),
                                    repel = TRUE) + # no text overlap
  scale_y_continuous(breaks = seq(-10, 15, 5), limits = c(-10, 15)) +
  scale_x_continuous(breaks = seq(-15, 15, 5), limits = c(-15, 18)) +
  background_grid(major = "none") +
  theme(panel.border = element_rect(fill = NA, size = 0.95),
        axis.text.x = element_text(face = "plain", size = 14, color = "black"),
        axis.text.y = element_text(face = "plain", size = 14, color = "black"),
        axis.ticks.length = unit(.15, "cm"),
        axis.ticks = element_line(size = 0.90),
        plot.margin = margin(0.1, 0.5, 0.2, 0.5, "cm"))

PCA_fullZ_biplot23 <- ggpubr::ggpar(PCA_fullZ_biplot23,
                                  xlab = "\nPC2 (18.6% of explained variance)",
                                  ylab = "PC3 (13.3% of explained variance)\n",
                                  title = "",
                                  font.x = c(15, "plain"),
                                  font.y = c(15, "plain"))
# =========================================================================
# =========================================================================





# =========================================================================
# 8. Calculate variables having highest loading
# =========================================================================
contrib_table <- round(var_fullZ$contrib, digits = 2)
corr_table <- round(var_fullZ$cor, digits = 2)
# =========================================================================
# =========================================================================





# =========================================================================
# 9. Extract PCA scores and add them to the table
# =========================================================================
individuals <- get_pca_ind(PCA_fullZ)
individuals$coord
game_data <- cbind(game_data, individuals$coord)
# =========================================================================
# =========================================================================





# =========================================================================
# 10. Save table for selection analysis using PC scores
# =========================================================================
setnames(game_data, "Dim.1", "PC1")
setnames(game_data, "Dim.2", "PC2")
setnames(game_data, "Dim.3", "PC3")
setnames(game_data, "Dim.4", "PC4")
setnames(game_data, "Dim.5", "PC5")
fwrite(game_data[,.(mirrors_id, match_id, PC1, PC2, PC3, PC4, PC5)],
       "04_PCscores-data.csv", sep = ",")
# =========================================================================
# =========================================================================





# =========================================================================
# 11. Save plots in a PDF file or other png file****
# =========================================================================
ggexport(plotlist = list(scree_plot,
                         contrib_PC1, contrib_PC2,
                         contrib_PC3),
         nrow = 1, ncol = 1,
         filename = "04_PCA_diagnostics-plots.pdf") # as PDF file


# as one figure
PCA_figure <- ggarrange(PCA_fullZ_biplot12,
                        PCA_fullZ_biplot23,
                        ncol = 2, nrow = 1)

ggexport(PCA_figure, filename = "04_PCA_figure.tiff", 
         width = 4000, height = 1800, res = 300)


# End of script ==========================================================


# as two seperate figures
#ggsave("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/masters_project/chapter1/outputs/04_PCA_dim12-biplot.tiff",
#       plot = PCA_fullZ_biplot12, device = "tiff",
#       dpi = 400, height = 6, width = 6) # at home as a tiff file

#ggsave("/Users/maxim/UQAM/Montiglio, Pierre-Olivier - Maxime Fraser Franco/MFraserFranco(2019-06-11)/masters_project/chapter1/outputs/04_PCA_dim23-biplot.tiff",
#       plot = PCA_fullZ_biplot23, device = "tiff",
#       dpi = 400, height = 6, width = 6) # at home as a tiff file

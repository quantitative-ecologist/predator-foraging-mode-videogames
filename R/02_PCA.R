
# =========================================================================

#           Principal component analysis on predator behaviour            #

# =========================================================================

# Code to produce the PCA and Figure S1

# -------------------------------------------------------------------------





# =========================================================================
# 1. Load the librairies and import the data
# =========================================================================


# Librairies --------------------------------------------------------------
library(data.table)
library(ggcorrplot)
library(factoextra)
library(FactoMineR)
library(cowplot)
library(ggpubr)


# Load the data -----------------------------------------------------------
data <- fread("./data/merged-data2021.csv")

# Select column to be analyzed
full_Zmatrix <- data[, c(47:56)]

names(full_Zmatrix)
# Change variable names
setnames(full_Zmatrix, "Zspeed", "travel speed")
setnames(full_Zmatrix, "Zspace_covered_rate", "rate of space covered")
setnames(full_Zmatrix, "Zprox_mid_guard", "ambush time")
setnames(full_Zmatrix, "Zcloset_open", "closets opened")
setnames(full_Zmatrix, "Zhit_special_count", "special attacks")
setnames(full_Zmatrix, "Zhit_far_count", "normal attacks")
setnames(full_Zmatrix, "Zpallet_destroyed", "pallets destroyed")
setnames(full_Zmatrix, "Zdamage_generator", "generators damaged")
setnames(full_Zmatrix, "Zhook_start_time", "time before 1st capture")

# =========================================================================
# =========================================================================





# =========================================================================
# 2. Visualize correlation matrixes
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
# 3. Compute PCA for the Zmatrix using FactoMineR package
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
# 4. Kaiser-Guttman criterion + scree plot
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
# 5. Contribution of variables to selected PCA axes
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
# 6. Generate PCA biplots for each matrix
# =========================================================================
# col.var = "contrib",
# gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # (if I used cos2)
# gradient.cols = c("pink", "purple1", "purple4", "orchid4"), # (if I used cos2)
# col.ind = grp, # (cos2) to have a contribution gradient
# palette = c("blueviolet", "orchid", "slateblue2", "violetred"),

# PCA Biplot (individuals + variables)
PCA_fullZ_biplot12 <- fviz_pca_biplot(PCA_fullZ,
                                    col.var = "black",
                                    col.ind = "dimgray", #571A44 #96579e
                                    alpha.ind = 0.2,
                                    arrowsize = 1,
                                    geom.ind = "point",
                                    pointshape = 16,
                                    pointsize = 1,
                                    labelsize = 6,
                                    select.var = list(contrib = 13),
                                    repel = TRUE) + # no text overlap
  #scale_y_continuous(breaks = seq(-10, 15, 5), limits = c(-10, 16)) +
  #scale_x_continuous(breaks = seq(-15, 15, 5), limits = c(-18, 15)) +
  scale_y_continuous(breaks = seq(-10, 10, 5), limits = c(-10, 10)) +
  scale_x_continuous(breaks = seq(-10, 10, 5), limits = c(-10, 10)) +
  background_grid(major = "none") +
  theme(panel.border = element_rect(fill = NA, size = 0.95),
        axis.text.x = element_text(face = "plain", size = 14, color = "black"),
        axis.text.y = element_text(face = "plain", size = 14, color = "black"),
        axis.ticks.length = unit(.15, "cm"),
        axis.ticks = element_line(size = 0.90),
        plot.margin = margin(0.1, 0.5, 0.2, 0.3, "cm"))

PCA_fullZ_biplot12 <- ggpubr::ggpar(PCA_fullZ_biplot12,
                                  xlab = "\nPC1 (21.0% of explained variance)",
                                  ylab = "PC2 (15.3% of explained variance)\n",
                                  title = "",
                                  font.x = c(15, "plain"),
                                  font.y = c(15, "plain"))

# ------------------------------------------------

PCA_fullZ_biplot23 <- fviz_pca_biplot(PCA_fullZ,
                                    col.var = "black",
                                    axes = c(2,3),
                                    col.ind = "dimgray", # firebrick3
                                    alpha.ind = 0.2,
                                    arrowsize = 1,
                                    geom.ind = "point",
                                    pointshape = 16,
                                    pointsize = 1,
                                    labelsize = 6,
                                    select.var = list(contrib = 13),
                                    repel = TRUE) + # no text overlap
  #scale_y_continuous(breaks = seq(-10, 15, 5), limits = c(-10, 15)) +
  #scale_x_continuous(breaks = seq(-15, 15, 5), limits = c(-15, 18)) +
  scale_y_continuous(breaks = seq(-10, 10, 5), limits = c(-10, 10)) +
  scale_x_continuous(breaks = seq(-10, 10, 5), limits = c(-10, 10)) +
  background_grid(major = "none") +
  theme(panel.border = element_rect(fill = NA, size = 0.95),
        axis.text.x = element_text(face = "plain", size = 14, color = "black"),
        axis.text.y = element_text(face = "plain", size = 14, color = "black"),
        axis.ticks.length = unit(.15, "cm"),
        axis.ticks = element_line(size = 0.90),
        plot.margin = margin(0.1, 0.5, 0.2, 0.5, "cm"))

PCA_fullZ_biplot23 <- ggpubr::ggpar(PCA_fullZ_biplot23,
                                  xlab = "\nPC2 (15.3% of explained variance)",
                                  ylab = "PC3 (13.8% of explained variance)\n",
                                  title = "",
                                  font.x = c(15, "plain"),
                                  font.y = c(15, "plain"))
# =========================================================================
# =========================================================================





# =========================================================================
# 7. Calculate variables having highest loading (Table SI)
# =========================================================================
contrib_table <- data.table(format(round(var_fullZ$contrib, digits = 2), nsmall = 2), keep.rownames = TRUE)
corr_table <- data.table(format(round(var_fullZ$cor, digits = 2), nsmall = 2), keep.rownames = TRUE)
# =========================================================================
# =========================================================================





# =========================================================================
# 8. Save plots in a PDF file or other png file****
# =========================================================================

# Ran this code once

#ggexport(plotlist = list(scree_plot,
#                         contrib_PC1, contrib_PC2,
#                         contrib_PC3),
#         nrow = 1, ncol = 1,
#         filename = "./outputs/02_PCA_diagnostics-plots.pdf") # as PDF file

# ggexport(PCA_fullZ_biplot12, filename = "./outputs/02_figureS1.png",
#          width = 2000, height = 1800, res = 300)


# as one figure with the two plots
#PCA_figure <- ggarrange(PCA_fullZ_biplot12,
#                        PCA_fullZ_biplot23,
#                        ncol = 2, nrow = 1)

# ggexport(PCA_figure, filename = "./outputs/02_figureS1.tiff", 
#          width = 4000, height = 1800, res = 300)


# End of script ==========================================================
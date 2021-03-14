#########################################################################

#                     Behavioural correlations plot                     #

#########################################################################

# Code to produce Figure 1 in Fraser Franco et al. 2021.
# Powerpoint was used to produce the final adjustements

# Contact: maxime.fraser.franco@hotmail.com
# Département des Sciences Biologiques, UQAM, Montréal, Québec
# -----------------------------------------------------------------------





# =======================================================================
# 1. Set working directory, load libraries, and export dataset
# =======================================================================
# personal computer onedrive UQAM Montiglio lab
setwd("C:/Users/maxim/OneDrive/Documents/GitHub/Chapter2")

# Import libraries
library(data.table)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(MCMCglmm)

# Load data
load("05A_multivariate_ranef.rda")
load("05A_IDcorr_list.rda")
load("05A_ModMV1.rda")

# Character variables to factor variables
char_as_factor <- names(ranef_table)[sapply(ranef_table, is.character)] # extract columns that are characters
ranef_table[, (char_as_factor) := lapply(.SD, as.factor), .SDcols = char_as_factor] # columns as factors
# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare the tables
# =======================================================================

# Random effects table
# ------------------------------------------

# Create variables for the plot groupings
#ranef_table[, type := c(rep("Agreement", 9),
#                        rep("Adjusted", 9)
#                        )
#           ]

#ranef_table[, random_effect := c(rep("Player ID", 3),
#                                 rep("Environment", 3),
#                                 rep("Avatar", 3)
#                                 )
#            , by = type]

#ranef_table[, variable := rep(c("Speed",
#                                "Space",
#                                "Guard"),
#                              3)
#            , by = type]


# Create factor order
#ranef_table[, ranef_order := factor(random_effect, levels = c("Avatar", "Environment", "Player ID"))]
#ranef_table[, variable_order := factor(variable, levels = c("Speed", "Space", "Guard"))]
# ------------------------------------------



# 2.a table for correlation matrixes #1
# ------------------------------------------

# Individual level matrix ---
# Extract correlation tables from the list
among_corr <- round(IDcorr_list$among_corr, 3)
within_corr <- round(IDcorr_list$within_corr, 3)

# Assign within ID correlation values to the upper part of the among ID matrix
among_corr[1, 2:3] <- within_corr[1, 2:3]
among_corr[2, 3] <- within_corr[2, 3]

# Modify column and row names to match variables
colnames(among_corr) <- c("Speed", "Space", "Guard")
rownames(among_corr) <- c("Speed", "Space", "Guard")

among_corr[1, 1] <- NA
among_corr[2, 2] <- NA
among_corr[3, 3] <- NA


# Environmental level matrix ---
env_corr <- round(IDcorr_list$env_corr, 3)
# Modify column and row names to match variables
colnames(env_corr) <- c("Speed", "Space", "Guard")
rownames(env_corr) <- c("Speed", "Space", "Guard")

#env_corr[1, 1] <- as.numeric(ranef_table[4, 2])
#env_corr[2, 2] <- as.numeric(ranef_table[5, 2])
#env_corr[3, 3] <- as.numeric(ranef_table[6, 2])
env_corr[1, 1] <- NA
env_corr[2, 2] <- NA
env_corr[3, 3] <- NA
#env_corr[1, 2] <- NA
#env_corr[1, 3] <- NA
#env_corr[2, 3] <- NA


# ------------------------------------------



# 2.b table for correlation matrixes #2
# ------------------------------------------
corr_table <- as.data.table(among_corr[2:3])
corr_table <- rbind(corr_table, as.data.table(among_corr[3, 2]))
corr_table <- rbind(corr_table, as.data.table(among_corr[1, 2]))
corr_table <- rbind(corr_table, as.data.table(among_corr[1:2, 3]))

corr_table[, ID_level := as.factor(c("among_ID", "among_ID", "among_ID", "within_ID", "within_ID", "within_ID"))]
setnames(corr_table, "V1", "correlation")

# Confidence intervals for the correlations
among_int <- as.data.table(HPDinterval(posterior.cor(ModMV1$VCV[, 1:9]))) # among
among_int <- among_int[c(2, 3, 6),]
within_int <- as.data.table(HPDinterval(posterior.cor(ModMV1$VCV[, 28:36]))) # within
within_int <- within_int[c(2, 3, 6),]

# Bind tables together to have final table
int <- rbind(among_int, within_int)
corr_table <- cbind(corr_table, int)
corr_table[, group := as.factor(rep(c("Speed~Space", "Speed~Guard", "Space~Guard"), 2))]
# Order the factor
corr_table[, group_ordered := factor(group, levels = c("Speed~Space", "Speed~Guard", "Space~Guard"))]


# Bind tables together to have final table (french version)
int <- rbind(among_int, within_int)
corr_table <- cbind(corr_table, int)
corr_table[, group := as.factor(rep(c("Vitesse~Espace", "Vitesse~Garde", "Espace~Garde"), 2))]
# Order the factor
corr_table[, group_ordered := factor(group, levels = c("Vitesse~Espace", "Vitesse~Garde", "Espace~Garde"))]
# ------------------------------------------



# 2.c table for correlation matrixes #3 
# (including environmental correlations for plot #3)
# ------------------------------------------
env_table <- as.data.table(env_corr[2:3])
env_table <- rbind(env_table, as.data.table(env_corr[3,2]))
setnames(env_table, "V1", "correlation")
env_table[, ID_level := as.factor("among_env")]
among_envCI <- as.data.table(IDcorr_list$env_corrCI[c(2,3,6),]) # among env.

env_table <- cbind(env_table, among_envCI)
env_table[, group := as.factor(c("Vitesse~Espace", "Vitesse~Garde", "Espace~Garde"))]
env_table[, group_ordered := factor(group, levels = c("Vitesse~Espace", 
                                                      "Vitesse~Garde", 
                                                      "Espace~Garde"))]
# merge with the corr_table created above to have corr_table2
corr_table2 <- rbind(corr_table, env_table)
# ------------------------------------------
# =======================================================================
# =======================================================================





# =======================================================================
# 3. Prepare some plot options
# =======================================================================
# Colorblind friendly palettes
# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Dodge the position of dots and error bars
pd <- position_dodge(0.5)
# =======================================================================
# =======================================================================





# ======================================================================
# 4. Generate final figure
# ======================================================================

# Repeatabilities (ICC) plot
#rpt_plot <- ggplot() +
#              geom_pointrange(data = ranef_table[type == "Agreement"],
#                              mapping = aes(x = ranef_order,
#                                            y = rpt.V1,
#                                            ymin = lower,
#                                            ymax = upper,
#                                            shape = variable_order), 
#                              position = pd,
#                              size = 0.7) +
#                              geom_hline(yintercept = 0, linetype = "dashed") +
#              scale_y_continuous(breaks = seq(0, 0.5, 0.10),
#                                 limits = c(0, 0.4)) +
#              ylab("\nRepeatability (ICC) ± 95% CI") +
#              xlab("") +
#              scale_shape_manual(values = c(16, 17, 15), # legend shape and labels
#                                 labels = c("Speed",
#                                            "Space",
#                                            "Guard")) +
#              theme(axis.text.x = element_text(face = "plain", size = 14,
#                                        color = "black", vjust = -2),
#                    axis.text.y = element_text(face = "plain", size = 14,
#                                   color = "black"),
#                    axis.ticks.length = unit(.15, "cm"), # axis ticks lenght
#                    axis.ticks = element_line(size = 0.90, color = "black"), # axis ticks width
#                    axis.title = element_text(size = 14, face = "plain"), # axis titles size
#                    axis.line = element_line(size = 0.95),
#                    legend.position = "top",
#                    legend.title = element_blank(),
#                    legend.key = element_blank(),
#                    legend.text = element_text(size = 11),
#                    panel.grid.major.y = element_blank(),
#                    panel.background = element_blank()) + coord_flip()
                    #plot.margin = unit(c(2.5, 0.1, 0.5, 0.5), "lines")) + coord_flip()
# ---------------------------------------------------------------------

# ID correlations plot 1
IDcorr_plot <- ggcorrplot(among_corr[, 3:1], # [,3:1] switches diagonal direction
                          method = "square",
                          lab = TRUE,
                          lab_size = 5,
                          digits = 3,
                          outline.color = "black",
                          colors = c("#0072B2", "white", "#D55E00")) +
               xlab("") +
               ylab("") +
               theme(axis.text.x = element_text(face = "plain",
                                                size = 14,
                                                color = "black",
                                                hjust = 0.5, vjust = 85, angle = 0), 
                                                #hjust = 0.5, vjust = 142, angle = 0
                    axis.text.y = element_text(face = "plain",
                                               size = 14,
                                               color = "black"),
#axis.ticks.length = unit(.15, "cm"), # axis ticks lenght
#axis.ticks = element_line(size = 0.90, 
#                          color = "black"), # axis ticks width
#axis.line = element_line(size = 0.95),
                    axis.line = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.background = element_blank(),
                    legend.text = element_text(size = 12, face = "plain"),
                    legend.title = element_text(size = 12, face = "plain"))
                    #plot.margin = unit(c(2.5, 0.1, 0.5, 0.5), "lines"))

# Environmental correlations plot 1
ENVcorr_plot <- ggcorrplot(env_corr[, 3:1], # [,3:1] switches diagonal direction
                          method = "square",
                          lab = TRUE,
                          lab_size = 5,
                          digits = 3,
                          outline.color = "black",
                          colors = c("#0072B2", "white", "#D55E00")) +
               xlab("") +
               ylab("") +
               theme(axis.text.x = element_text(face = "plain",
                                                size = 14,
                                                color = "black",
                                                hjust = 0.5, vjust = 85, angle = 0), 
                                                #hjust = 0.5, vjust = 142, angle = 0
                    axis.text.y = element_text(face = "plain",
                                               size = 14,
                                               color = "black"),
#axis.ticks.length = unit(.15, "cm"), # axis ticks lenght
#axis.ticks = element_line(size = 0.90, 
#                          color = "black"), # axis ticks width
#axis.line = element_line(size = 0.95),
                    axis.line = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.background = element_blank(),
                    legend.text = element_text(size = 12, face = "plain"),
                    legend.title = element_text(size = 12, face = "plain"))
                    #plot.margin = unit(c(2.5, 0.1, 0.5, 0.5), "lines"))


# ---------------------------------------------------------------------

# ID Correlations plot 2
IDcorr_plot2 <- ggplot() +
              geom_pointrange(data = corr_table,
                              mapping = aes(x = group_ordered,
                                            y = correlation,
                                            ymin = lower,
                                            ymax = upper,
                                            color = ID_level),
                              position = pd,
                              size = 0.7, shape = 16) +
              geom_hline(yintercept = 0, linetype = "dashed") +
              scale_y_continuous(breaks = seq(-0.80, 0.20, 0.20),
                                 limits = c(-0.80, 0.20)) +
              scale_color_manual(values = cbp1[c(6, 7)],
                                 labels = c("Among ID", "Within ID")) +
              ylab("\nCorrelation ± 95% CI") +
              xlab("") +
               theme(axis.text.x = element_text(face = "plain",
                                                size = 14,
                                                color = "black",
                                                vjust = -2),
                    axis.text.y = element_text(face = "plain",
                                               size = 14,
                                               color = "black"),
                    axis.ticks.length = unit(.15, "cm"), # axis ticks lenght
                    axis.ticks = element_line(size = 0.90,
                                              color = "black"), # axis ticks width
                    axis.line = element_line(size = 0.95),
                    axis.title = element_text(size = 14, face = "plain"),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.background = element_blank(),
                    legend.title = element_blank(),
                    legend.position = "top",
                    legend.key = element_blank(),
                    legend.text = element_text(size = 11)) + coord_flip()
#                    plot.margin = unit(c(2.5, 0.5, 0.5, 0.6), "lines")) + coord_flip()
# ---------------------------------------------------------------------


# ID Correlations plot 3
IDcorr_plot3 <- ggplot() +
              geom_pointrange(data = corr_table2,
                              mapping = aes(x = group_ordered,
                                            y = correlation,
                                            ymin = lower,
                                            ymax = upper,
                                            color = ID_level),
                              position = pd,
                              size = 0.7, shape = 16) +
              geom_hline(yintercept = 0, linetype = "dashed") +
              #scale_y_continuous(breaks = seq(-0.80, 0.20, 0.20),
              #                   limits = c(-0.80, 0.20)) +
              scale_color_manual(values = cbp1[c(6, 7, 4)],
                                 labels = c("Inter-IND", "Intra-IND", "Inter-ENV")) +
              ylab("\nCorrélation ± 95% IC") +
              xlab("") +
               theme(axis.text.x = element_text(face = "plain",
                                                size = 14,
                                                color = "black",
                                                vjust = -2),
                    axis.text.y = element_text(face = "plain",
                                               size = 14,
                                               color = "black"),
                    axis.ticks.length = unit(.15, "cm"), # axis ticks lenght
                    axis.ticks = element_line(size = 0.90,
                                              color = "black"), # axis ticks width
                    axis.line = element_line(size = 0.95),
                    axis.title = element_text(size = 14, face = "plain"),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.background = element_blank(),
                    legend.title = element_blank(),
                    legend.position = "top",
                    legend.key = element_blank(),
                    legend.text = element_text(size = 11)) + coord_flip()
#                    plot.margin = unit(c(2.5, 0.5, 0.5, 0.6), "lines")) + coord_flip()
# ---------------------------------------------------------------------



# Combine both plots and annotate with labels
#ModMV1_figure <- ggarrange(rpt_plot, NULL, IDcorr_plot,
#                           labels = c("(a)", "", "(b)"),
#                           ncol = 3, nrow = 1, widths = c(1.45, 0.5, 1.5))
# Save and export figure
#ggexport(ModMV1_figure, filename = "06A_ModMV1-plot.tiff",
#         width = 4655, height = 1900, res = 450) # more res = bigger plot zoom
#

ModMV1.1_figure <- ggarrange(IDcorr_plot, NULL, ENVcorr_plot,
                           labels = c("(a)", "", "(b)"),
                           ncol = 3, nrow = 1, widths = c(1.45, 0.5, 1.45), 
                           common.legend = TRUE, legend = "right")
# Save and export figure
ggexport(ModMV1.1_figure, filename = "06A_ModMV1.1-plot.tiff",
         width = 4655, height = 1900, res = 450) # more res = bigger plot zoom
#

#ModMV1_figure2 <- ggarrange(rpt_plot, NULL, IDcorr_plot2,
#                           labels = c("(a)", "", "(b)"),
#                           ncol = 3, nrow = 1, widths = c(1.45, 0.5, 1.5))
#

# Save and export figure
#ggexport(ModMV1_figure2, filename = "06A_ModMV1_second-plot.tiff",
#         width = 4655, height = 1900, res = 450) # more res = bigger plot zoom

ggexport(IDcorr_plot3, filename = "06A_ModMV1_third_plot.tiff",
         width = 2500, height = 2500, res = 450)
# End of script --------------------------------------------------------


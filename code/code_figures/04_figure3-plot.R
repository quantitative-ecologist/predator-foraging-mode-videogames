# =======================================================================

#                Panel plot of hunting success analyses                 #

# =======================================================================

# Code to produce Figure 3
# Plot marginal effects of the base and quadratic hunting success models
# -----------------------------------------------------------------------





# =======================================================================
# 1. Load libraries and datasets
# =======================================================================


# Import libraries ------------------------------------------------------ 

library(data.table)
library(ggplot2)
library(ggpubr)



# Load predictions ------------------------------------------------------

base_tab <- readRDS("./outputs/R_objects/base-model_draws-table.rds")
quad_tab <- readRDS("./outputs/R_objects/quadratic-model_draws-table.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare plot customizations
# =======================================================================

# Visualise color palette to choose colors
#library(scales)
#library(viridis)
#show_col(viridis_pal()(20))


# Set custom theme for plots --------------------------------------------

custom_theme <- theme(# axis values size
                      axis.text.x = element_text(face = "plain", 
                                                 size = 15,
                                                 color = "black"),
                      axis.text.y = element_text(face = "plain", 
                                                 size = 15,
                                                 color = "black"),
                      # axis ticks lenght
                      axis.ticks.length = unit(.15, "cm"),
                      # axis ticks width
                      axis.ticks = element_line(size = 0.90, 
                                                color = "black"),
                      # axis titles size
                      axis.title = element_text(size = 15, 
                                                face = "plain"),
                      axis.line = element_line(size = 0.95),
                      #  plot.margin = unit(c(2, 1.2, 2, 2), "lines"),
                      legend.position = "none",
                      panel.grid = element_blank(),
                      panel.background = element_blank())



# Add two digits to all x axes ------------------------------------------

scaleFUN <- function(x) sprintf("%.1f", x)

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Fixed effects plots for the base model
# =======================================================================

# Here, the estimates need to be divided by 4 to have the probability


# Plot for predator speed -----------------------------------------------

speed <- ggplot(base_tab[x_variable == "speed"]) +
  geom_line(aes(x = Zspeed, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zspeed, y = lower__/4),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zspeed, y = upper__/4),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zspeed,
                  ymin = lower_pred_int/4,
                  ymax = upper_pred_int/4),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  #scale_x_continuous(breaks = seq(-8, 4, 4),
  #                   limits = c(-8, 4.8),
  #                   labels = scaleFUN) +
  scale_x_continuous(labels = scaleFUN) +
  xlab("\nSpeed") +
  ylab("") +
  custom_theme +
  theme(plot.margin = unit(c(2, 1.2, 2, 2), "lines"))



# Plot for predator space -----------------------------------------------

space <- ggplot(base_tab[x_variable == "space"]) +
  geom_line(aes(x = Zspace_covered_rate, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zspace_covered_rate, y = lower__/4),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zspace_covered_rate, y = upper__/4),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zspace_covered_rate,
                  ymin = lower_pred_int/4,
                  ymax = upper_pred_int/4),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  scale_x_continuous(labels = scaleFUN) +
  xlab("\nSpace") +
  ylab("") +
  custom_theme +
  theme(plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"))



# Plot for predator guard -----------------------------------------------

guard <- ggplot(base_tab[x_variable == "guard"]) +
  geom_line(aes(x = Zprox_mid_PreyGuarding, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zprox_mid_PreyGuarding, y = lower__/4),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zprox_mid_PreyGuarding, y = upper__/4),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zprox_mid_PreyGuarding, 
                  ymin = lower_pred_int/4,
                  ymax = upper_pred_int/4),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  #scale_x_continuous(breaks = seq(0, 7.5, 2.5),
  #                   limits = c(-1.2, 7.5)) +
  scale_x_continuous(labels = scaleFUN) +
  xlab("\nAmbush time") +
  ylab("") +
  custom_theme +
  theme(plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"))



# Plot for the time before 1st capture ----------------------------------

hook <- ggplot(base_tab[x_variable == "hook"]) +
  geom_line(aes(x = Zhook_start_time, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zhook_start_time, y = lower__/4),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zhook_start_time, y = upper__/4),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zhook_start_time, 
                  ymin = lower_pred_int/4,
                  ymax = upper_pred_int/4),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  #scale_x_continuous(breaks = seq(-1.5, 3, 1.5),
  #                   limits = c(-1.5, 4.5)) +
  scale_x_continuous(labels = scaleFUN) +
  xlab(expression(paste("Time for ",
                        1^st, 
                        "capture", 
                        sep = ""))) +
  ylab("") +
  custom_theme +
  theme(plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"))

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Fixed effects plots for the quadratic model
# =======================================================================


# Plot for predator speed^2 ---------------------------------------------

quad_speed <- ggplot(quad_tab[x_variable == "speed"]) +
  geom_line(aes(x = Zspeed, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zspeed, y = lower__/4),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zspeed, y = upper__/4),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zspeed,
                  ymin = lower_pred_int/4,
                  ymax = upper_pred_int/4),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  #scale_x_continuous(breaks = seq(-8, 4, 4),
  #                   limits = c(-8, 4.8), 
  #                   labels = scaleFUN) +
  scale_x_continuous(labels = scaleFUN) +
  xlab("\nSpeed") +
  ylab("") +
  custom_theme +
  theme(plot.margin = unit(c(2, 1.2, 2, 2), "lines"))



# Plot for predator space^2 ---------------------------------------------

quad_space <- ggplot(quad_tab[x_variable == "space"]) +
  geom_line(aes(x = Zspace_covered_rate, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zspace_covered_rate, y = lower__/4),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zspace_covered_rate, y = upper__/4),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zspace_covered_rate,
                  ymin = lower_pred_int/4,
                  ymax = upper_pred_int/4),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  scale_x_continuous(labels = scaleFUN) +
  xlab("\nSpace") +
  ylab("") +
  custom_theme +
  theme(plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"))



# Plot for predator guard^2 ---------------------------------------------

quad_guard <- ggplot(quad_tab[x_variable == "guard"]) +
  geom_line(aes(x = Zprox_mid_PreyGuarding, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zprox_mid_PreyGuarding, y = lower__/4),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zprox_mid_PreyGuarding, y = upper__/4),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zprox_mid_PreyGuarding,
                  ymin = lower_pred_int/4,
                  ymax = upper_pred_int/4),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  #scale_x_continuous(breaks = seq(0, 7.5, 2.5),
  #                   limits = c(-1.2, 7.5)) +
  scale_x_continuous(labels = scaleFUN) +
  xlab("\nAmbush time") +
  ylab("") +
  custom_theme +
  theme(plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"))



# Plot for predator hook^2 ----------------------------------------------

quad_hook <- ggplot(quad_tab[x_variable == "hook"]) +
  geom_line(aes(x = Zhook_start_time, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zhook_start_time, y = lower__/4),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zhook_start_time, y = upper__/4),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zhook_start_time, 
                  ymin = lower_pred_int/4,
                  ymax = upper_pred_int/4),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  #scale_x_continuous(breaks = seq(-1.5, 3, 1.5),
  #                   limits = c(-1.5, 4.5)) +
  scale_x_continuous(labels = scaleFUN) +
  xlab(expression(paste("Time for ",
                        1^st,
                        "capture",
                        sep = ""))) +
  ylab("") +
  custom_theme +
  theme(plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"))

# =======================================================================
# =======================================================================





# =======================================================================
# 5. Create the figure with 6 panels
# =======================================================================


# Create the 3 paneled figure -------------------------------------------

panel_plot <- ggarrange(speed,
                        space,
                        guard,
                        hook,
                        quad_speed,
                        quad_space,
                        quad_guard,
                        quad_hook,
                        ncol = 4, nrow = 2,
                        widths = c(2.8, 2.5, 2.5, 2.5),
                        heights = c(2.8, 2.8, 2.8, 2.8),
                        labels = c("(A)", "(B)", "(C)", "(D)",
                                   "(E)", "(F)", "(G)", "(H)"))
# Upper y label
panel_plot <- annotate_figure(panel_plot,
                              left = text_grob("Hunting success", 
                                               rot = 90,
                                               size = 15,
                                               #-1.23 with police size = 14
                                               hjust = -1.00,
                                               vjust = 0.5))
# lower y label
panel_plot <- annotate_figure(panel_plot,
                              left = text_grob("Hunting success", 
                                               rot = 90,
                                               size = 15,
                                               hjust = 1.7,
                                               vjust = 2.1))



# Export the figure -----------------------------------------------------

ggexport(panel_plot, filename = "./outputs/figures/manuscript_figures/04_figure3.png",
         width = 4500, height = 2500, res = 300)

# =======================================================================
# =======================================================================
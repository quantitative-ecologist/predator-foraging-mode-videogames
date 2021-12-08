# =======================================================================

#                Panel plot of hunting success analyses                 #

# =======================================================================

# Code to produce Figure 3
# Plot marginal effects of the base and quadratic hunting success models
# -----------------------------------------------------------------------





# =======================================================================
# 1. Load libraries, datasets, and models
# =======================================================================


# Import libraries ------------------------------------------------------ 

library(data.table)
library(brms)
library(ggplot2)
library(ggpubr)



# Import the data -------------------------------------------------------

data <- fread("./data/merged-data2021.csv",
              select = c("player_id", "match_id",
                         "hunting_success", "map_name", 
                         "game_duration", "speed",
                         "space_covered_rate",
                         "prox_mid_PreyGuarding",
                         "hook_start_time"),
                         stringsAsFactors = TRUE)



# Transform --------------------------------------------------------------

# Transform the data even though it is not perfect
data[, ":=" (prox_mid_PreyGuarding = log(prox_mid_PreyGuarding + 1),
             hook_start_time = log(hook_start_time + 1),
             game_duration = sqrt(game_duration))]



# Standardise the variables (Z-scores) ----------------------------------

standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / 
                              sd(x, na.rm = TRUE)}

data[, c("Zgame_duration", "Zspeed",
         "Zspace_covered_rate", "Zprox_mid_PreyGuarding",
         "Zhook_start_time") :=
                lapply(.SD, standardize), 
                .SDcols = c(5:9)]

# Add observation-level random effect
data$obs <- 1:nrow(data)



# Load both models ------------------------------------------------------

base_model <- readRDS("./outputs/models/03B_hunting_success_base-model1.rds")
quadratic_model <- readRDS("./outputs/models/03C_hunting_success_quadratic-model1.rds")



# Load predictions ------------------------------------------------------

speed_tab <- readRDS("./outputs/R_objects/base-model_speed-fe.rds")
space_tab <- readRDS("./outputs/R_objects/base-model_space-fe.rds")
guard_tab <- readRDS("./outputs/R_objects/base-model_guard-fe.rds")
hook_tab  <- readRDS("./outputs/R_objects/base-model_hook-fe.rds")

speed_tab1 <- readRDS("./outputs/R_objects/base-model_speed-re.rds")
space_tab1 <- readRDS("./outputs/R_objects/base-model_space-re.rds")
guard_tab1 <- readRDS("./outputs/R_objects/base-model_guard-re.rds")
hook_tab1  <- readRDS("./outputs/R_objects/base-model_hook-re.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Prepare the sample tables
# =======================================================================


# Extract the tables from the objects -----------------------------------

speed_tab <- speed_tab$Zspeed
space_tab <- speed_tab$Zspace_covered_rate
guard_tab <- speed_tab$Zprox_mid_PreyGuarding
hook_tab  <- speed_tab$Zhook_start_time



# Bind the two tables ---------------------------------------------------

# Here, I extract the prediction intervals from the second set of tables
speed_tab <- cbind(speed_tab$Zspeed, 
                   upper_pred_int = speed_tab1$Zspeed[, "upper__"],
                   lower_pred_int = speed_tab1$Zspeed[, "lower__"])

space_tab <- cbind(space_tab$Zspace_covered_rate, 
                   upper_pred_int = space_tab1$Zspace_covered_rate[, "upper__"],
                   lower_pred_int = space_tab1$Zspace_covered_rate[, "lower__"])

guard_tab <- cbind(guard_tab$Zprox_mid_PreyGuarding, 
                   upper_pred_int = guard_tab1$Zprox_mid_PreyGuarding[, "upper__"],
                   lower_pred_int = guard_tab1$Zprox_mid_PreyGuarding[, "lower__"])

hook_tab <- cbind(hook_tab$Zhook_start_time, 
                  upper_pred_int = hook_tab1$Zhook_start_time[, "upper__"],
                  lower_pred_int = hook_tab1$Zhook_start_time[, "lower__"])






# =======================================================================
# =======================================================================





# =======================================================================
# 3. Prepare plot customizations
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
# 4. Fixed effects plots for the base model
# =======================================================================

# Here, the estimates need to be divided by 4 to have the probability


# Plot for predator speed -----------------------------------------------

speed <- ggplot(speed_tab) +
  geom_line(aes(x = Zspeed, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zspeed, y = lower__),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zspeed, y = upper__),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zspeed,
                  ymin = lower_pred_int,
                  ymax = upper_pred_int),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  #scale_x_continuous(breaks = seq(-8, 4, 4),
  #                   limits = c(-8, 4.8),
  #                   labels = scaleFUN) +
  xlab("\nSpeed") +
  ylab("") +
  custom_theme + theme(plot.margin = unit(c(2, 1.2, 2, 2), "lines"))



# Plot for predator space -----------------------------------------------

space <- ggplot(space_tab) +
  geom_line(aes(x = Zspace_covered_rate, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zspace_covered_rate, y = lower__),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zspace_covered_rate, y = upper__),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zspace_covered_rate,
                  ymin = lower_pred_int,
                  ymax = upper_pred_int),
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

guard <- ggplot(guard_tab) +
  geom_line(aes(x = guard, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zprox_mid_PreyGuarding, y = lower__),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zprox_mid_PreyGuarding, y = upper__),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zprox_mid_PreyGuarding, 
                  ymin = lower_pred_int,
                  ymax = upper_pred_int),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  #scale_x_continuous(breaks = seq(0, 7.5, 2.5),
  #                   limits = c(-1.2, 7.5)) +
  xlab("\nAmbush time") +
  ylab("") +
  custom_theme +
  theme(plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"))



# Plot for the time before 1st capture ----------------------------------

hook <- ggplot(hook_tab) +
  geom_line(aes(x = Zhook_start_time, y = estimate__/4),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = Zhook_start_time, y = lower__),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(aes(x = Zhook_start_time, y = upper__),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(aes(x = Zhook_start_time, 
                  ymin = lower_pred_int,
                  ymax = upper_pred_int),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  #scale_x_continuous(breaks = seq(-1.5, 3, 1.5),
  #                   limits = c(-1.5, 4.5)) +
  xlab(expression(
    paste("Time for ", 1^st, "capture", sep = ""))) +
  ylab("") +
  custom_theme +
  theme(plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"))

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Fixed effects plots for the quadratic model
# =======================================================================


# -----------------------------------
# Predator average movement speed
# -----------------------------------
# Create new data
speed_dat <- data.table(speed      = seq(min(data$Zspeed),
                                         max(data$Zspeed),
                                         length.out = 100),
                        space      = mean(data$Zspace_covered_rate),
                        guard      = mean(data$Zprox_mid_guard),
                        hook       = mean(data$Zhook_start_time))
                       # surv_speed = mean(data$Zsurv_speed),
                       # surv_space = mean(data$Zsurv_space_covered_rate))
# Model matrix
speed_mm <- model.matrix(~ 
                           # Quadratic terms
                           I(speed^2) +
                           I(space^2) +
                           I(guard^2) +
                           I(hook^2) +
                          # I(surv_speed^2) +
                          # I(surv_space^2) +
                           # Linear terms
                           speed +
                           space +
                           guard +
                           hook +
                          # surv_speed +
                          # surv_space +
                           # Predator trait covariances
                           speed : space +
                           speed : guard +
                           space : guard +
                           speed : hook +
                           space : hook +
                           guard : hook,
                           # Predator-prey trait covariances
                          # speed : surv_speed +
                          # speed : surv_space +
                          # space : surv_speed +
                          # space : surv_space +
                          # guard : surv_speed +
                          # guard : surv_space +
                          # hook : surv_speed +
                          # hook : surv_space, 
                           speed_dat)
# Compute fitted values
speed_y <- speed_mm%*%fixef(quadratic_model)

# Confidence intervals
speed_pvar <- diag(speed_mm %*% tcrossprod(vcov(quadratic_model), speed_mm))
speed_tvar <- speed_pvar + 
  VarCorr(quadratic_model)$obs$sd[1]^2 + 
  VarCorr(quadratic_model)$mirrors_id$sd[1]^2 + 
  VarCorr(quadratic_model)$map_name$sd[1]^2

# Generate table
speed_newdat <- data.table(
  speed = speed_dat$speed,
  space = speed_dat$space,
  guard = speed_dat$guard,
  hook = speed_dat$hook,
#  surv_speed = speed_dat$surv_speed,
#  surv_space = speed_dat$surv_space,
  speed_y = plogis(speed_y),
  speed_plo = plogis(speed_y - 1.96 * sqrt(speed_pvar)),
  speed_phi = plogis(speed_y + 1.96 * sqrt(speed_pvar)),
  speed_tlo = plogis(speed_y - 1.96 * sqrt(speed_tvar)),
  speed_thi = plogis(speed_y + 1.96 * sqrt(speed_tvar))
)


# Plot for predator speed^2
quad_speed <- ggplot(speed_newdat) +
  #    geom_point(data = data,
  #                 aes(x = Zspeed, y = prop_captures),
  #                 shape = 16, 
  #                 alpha = 0.1, 
  #                 color = "black") +
  geom_line(aes(x = speed, y = speed_y.Estimate),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = speed, y = speed_plo.Estimate),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(data = speed_newdat,
            aes(x = speed, y = speed_phi.Estimate),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(data = speed_newdat,
              aes(x = speed,
                  ymin = speed_tlo.Estimate,
                  ymax = speed_thi.Estimate),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-8, 4, 4),
                     limits = c(-8, 4.8), 
                     labels = scaleFUN) +
  xlab("\nSpeed") +
  ylab("") +
  custom_theme + theme(plot.margin = unit(c(2, 1.2, 2, 2), "lines"))
# -----------------------------------


# -----------------------------------
# Predator rate of space covered
# -----------------------------------
# Create new data
space_dat <- data.table(speed      = mean(data$Zspeed),
                        space      = seq(min(data$Zspace_covered_rate), 
                                         5,
                                         length.out = 100),
                        guard      = mean(data$Zprox_mid_guard),
                        hook       = mean(data$Zhook_start_time))
                      #  surv_speed = mean(data$Zsurv_speed),            
                      #  surv_space = mean(data$Zsurv_space_covered_rate))
# Model matrix
space_mm <- model.matrix(~ 
                           # Quadratic terms
                           I(speed^2) +
                           I(space^2) +
                           I(guard^2) +
                           I(hook^2) +
                          # I(surv_speed^2) +
                          # I(surv_space^2) +
                           # Linear terms
                           speed +
                           space +
                           guard +
                           hook +
                          # surv_speed +
                          # surv_space +
                           # Predator trait covariances
                           speed : space +
                           speed : guard +
                           space : guard +
                           speed : hook +
                           space : hook +
                           guard : hook,
                           # Predator-prey trait covariances
                          # speed : surv_speed +
                          # speed : surv_space +
                          # space : surv_speed +
                          # space : surv_space +
                          # guard : surv_speed +
                          # guard : surv_space +
                          # hook : surv_speed +
                          # hook : surv_space,
                           space_dat)
# Compute fitted values
space_y <- space_mm%*%fixef(quadratic_model)

# Confidence intervals
space_pvar <- diag(space_mm %*% tcrossprod(vcov(quadratic_model), space_mm))
space_tvar <- space_pvar + 
  VarCorr(quadratic_model)$obs$sd[1]^2 + 
  VarCorr(quadratic_model)$mirrors_id$sd[1]^2 + 
  VarCorr(quadratic_model)$map_name$sd[1]^2

# Generate table
space_newdat <- data.table(
  speed = space_dat$speed,
  space = space_dat$space,
  guard = space_dat$guard,
  hook = space_dat$hook,
#  surv_speed = space_dat$surv_speed,
#  surv_space = space_dat$surv_space,
  space_y = plogis(space_y),
  space_plo = plogis(space_y - 1.96 * sqrt(space_pvar)),
  space_phi = plogis(space_y + 1.96 * sqrt(space_pvar)),
  space_tlo = plogis(space_y - 1.96 * sqrt(space_tvar)),
  space_thi = plogis(space_y + 1.96 * sqrt(space_tvar))
)


# Plot for predator space^2
quad_space <- ggplot(space_newdat) +
  #    geom_point(data = data[Zspace_covered_rate <= 5],
  #               aes(x = Zspace_covered_rate, y = prop_captures),
  #               shape = 16, 
  #               alpha = 0.1, 
  #               color = "black") +
  geom_line(aes(x = space, y = space_y.Estimate),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = space, y = space_plo.Estimate),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(data = space_newdat,
            aes(x = space, y = space_phi.Estimate),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(data = space_newdat,
              aes(x = space,
                  ymin = space_tlo.Estimate,
                  ymax = space_thi.Estimate),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  scale_x_continuous(labels = scaleFUN) +
  xlab("\nSpace") +
  ylab("") +
  custom_theme + theme(plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"))
# -----------------------------------


# -----------------------------------
# Predator proportion of time spent guarding
# -----------------------------------
# Create new data
guard_dat <- data.table(speed      = mean(data$Zspeed),             
                        space      = mean(data$Zspace_covered_rate),
                        guard      = seq(min(data$Zprox_mid_guard), 
                                         7,
                                         length.out = 100),
                        hook       = mean(data$Zhook_start_time))
                      #  surv_speed = mean(data$Zsurv_speed),
                      #  surv_space = mean(data$Zsurv_space_covered_rate))
# Model matrix
guard_mm <- model.matrix(~ 
                           # Quadratic terms
                           I(speed^2) +
                           I(space^2) +
                           I(guard^2) +
                           I(hook^2) +
                         #  I(surv_speed^2) +
                         #  I(surv_space^2) +
                           # Linear terms
                           speed +
                           space +
                           guard +
                           hook +
                         #  surv_speed +
                         #  surv_space +
                           # Predator trait covariances
                           speed : space +
                           speed : guard +
                           space : guard +
                           speed : hook +
                           space : hook +
                           guard : hook,
                           # Predator-prey trait covariances
                        #   speed : surv_speed +
                        #   speed : surv_space +
                        #   space : surv_speed +
                        #   space : surv_space +
                        #   guard : surv_speed +
                        #   guard : surv_space +
                        #   hook : surv_speed +
                        #   hook : surv_space, 
                          guard_dat)
# Compute fitted values
guard_y <- guard_mm%*%fixef(quadratic_model)

# Confidence intervals
guard_pvar <- diag(guard_mm %*% tcrossprod(vcov(quadratic_model), guard_mm))
guard_tvar <- guard_pvar + 
  VarCorr(quadratic_model)$obs$sd[1]^2 + 
  VarCorr(quadratic_model)$mirrors_id$sd[1]^2 + 
  VarCorr(quadratic_model)$map_name$sd[1]^2

# Generate table
guard_newdat <- data.table(
  speed = guard_dat$speed,
  space = guard_dat$space,
  guard = guard_dat$guard,
  hook = guard_dat$hook,
#  surv_speed = guard_dat$surv_speed,
#  surv_space = guard_dat$surv_space,
  guard_y = plogis(guard_y),
  guard_plo = plogis(guard_y - 1.96 * sqrt(guard_pvar)),
  guard_phi = plogis(guard_y + 1.96 * sqrt(guard_pvar)),
  guard_tlo = plogis(guard_y - 1.96 * sqrt(guard_tvar)),
  guard_thi = plogis(guard_y + 1.96 * sqrt(guard_tvar))
)


# Plot for predator guard^2
quad_guard <- ggplot(guard_newdat) +
  #   geom_point(data = data[Zprox_mid_guard <= 7],
  #              aes(x = Zprox_mid_guard, y = prop_captures),
  #              shape = 16, 
  #              alpha = 0.1, 
  #              color = "black") +
  geom_line(aes(x = guard, y = guard_y.Estimate),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = guard, y = guard_plo.Estimate),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(data = guard_newdat,
            aes(x = guard, y = guard_phi.Estimate),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(data = guard_newdat,
              aes(x = guard,
                  ymin = guard_tlo.Estimate,
                  ymax = guard_thi.Estimate),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 7.5, 2.5),
                     limits = c(-1.2, 7.5)) +
  xlab("\nAmbush time") +
  ylab("") +
  custom_theme + theme(plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"))
# -----------------------------------


# -----------------------------------
# Predator time before 1st capture
# -----------------------------------
# Create new data
hook_dat <- data.table(speed      = mean(data$Zspeed),             
                       space      = mean(data$Zspace_covered_rate),
                       guard      = mean(data$Zprox_mid_guard),
                       hook       = seq(min(data$Zhook_start_time),
                                        max(data$Zhook_start_time),
                                        length.out = 100))
                      # surv_speed = mean(data$Zsurv_speed),
                      # surv_space = mean(data$Zsurv_space_covered_rate))
# Model matrix
hook_mm <- model.matrix(~ 
                          # Quadratic terms
                          I(speed^2) +
                          I(space^2) +
                          I(guard^2) +
                          I(hook^2) +
                         # I(surv_speed^2) +
                         # I(surv_space^2) +
                          # Linear terms
                          speed +
                          space +
                          guard +
                          hook +
                         # surv_speed +
                         # surv_space +
                          # Predator trait covariances
                          speed : space +
                          speed : guard +
                          space : guard +
                          speed : hook +
                          space : hook +
                          guard : hook,
                          # Predator-prey trait covariances
                        #  speed : surv_speed +
                        #  speed : surv_space +
                        #  space : surv_speed +
                        #  space : surv_space +
                        #  guard : surv_speed +
                        #  guard : surv_space +
                        #  hook : surv_speed +
                        #  hook : surv_space, 
                          hook_dat)
# Compute fitted values
hook_y <- hook_mm%*%fixef(quadratic_model)

# Confidence intervals
hook_pvar <- diag(hook_mm %*% tcrossprod(vcov(quadratic_model), hook_mm))
hook_tvar <- hook_pvar + 
  VarCorr(quadratic_model)$obs$sd[1]^2 + 
  VarCorr(quadratic_model)$mirrors_id$sd[1]^2 + 
  VarCorr(quadratic_model)$map_name$sd[1]^2

# Generate table
hook_newdat <- data.table(
  speed = hook_dat$speed,
  space = hook_dat$space,
  guard = hook_dat$guard,
  hook = hook_dat$hook,
#  surv_speed = hook_dat$surv_speed,
#  surv_space = hook_dat$surv_space,
  hook_y = plogis(hook_y),
  hook_plo = plogis(hook_y - 1.96 * sqrt(hook_pvar)),
  hook_phi = plogis(hook_y + 1.96 * sqrt(hook_pvar)),
  hook_tlo = plogis(hook_y - 1.96 * sqrt(hook_tvar)),
  hook_thi = plogis(hook_y + 1.96 * sqrt(hook_tvar))
)

# Plot for predator guard
quad_hook <- ggplot(hook_newdat) +
  geom_line(aes(x = hook, y = hook_y.Estimate),
            size = 1.5,
            color = "darkgray") +
  geom_line(aes(x = hook, y = hook_plo.Estimate),
            linetype = "dashed",
            size = 1,
            color = "black") +
  geom_line(data = hook_newdat,
            aes(x = hook, y = hook_phi.Estimate),
            linetype = "dashed",
            size = 1, 
            color = "black") +
  geom_ribbon(data = hook_newdat,
              aes(x = hook, 
                  ymin = hook_tlo.Estimate,
                  ymax = hook_thi.Estimate),
              alpha = 0.2,
              fill = "darkgray") +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-1.5, 3, 1.5),
                     limits = c(-1.5, 4.5)) +
  xlab(expression(
    paste("Time for ", 1^st, "capture", sep=""))) +
  ylab("") +
  custom_theme + theme(plot.margin = unit(c(2, 1.2, 2, 0.5), "lines"))

# =======================================================================
# =======================================================================





# =======================================================================
# 5. Create the figure with 6 panels
# =======================================================================
# Create the 3 paneled figure
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
                                               hjust = -1.00, vjust = 0.5)) #-1.23 with police size = 14
# lower y label
panel_plot <- annotate_figure(panel_plot,
                              left = text_grob("Hunting success", 
                                               rot = 90,
                                               size = 15,
                                               hjust = 1.7, vjust = 2.1))

ggexport(panel_plot, filename = "./outputs/04_figure3.png",
         width = 4500, height = 2500, res = 300) # more res = bigger plot zoom
# =======================================================================
# =======================================================================
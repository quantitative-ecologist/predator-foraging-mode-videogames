# =======================================================================

#             Extract the effects of x on y for the quadratic           #
#                          hunting success model                        #

# =======================================================================




# =======================================================================
# 1. Prepare the script
# =======================================================================


# Detect the number of cores --------------------------------------------

options(mc.cores = parallel::detectCores())



# Load the packages -----------------------------------------------------

library(brms)
library(data.table)



# Import the model ------------------------------------------------------

quad_model <- readRDS("03C_hunting_success_quadratic-model2.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Extract the samples
# =======================================================================


# Run the sampling excluding the random effects -------------------------

speed_plot1 <- conditional_effects(quad_model, 
                                   effects = "Zspeed",
                                   method = "fitted",
                                   re_formula = NA, # exclude ranefs
                                   robust = FALSE, # plots the mean
                                   samples = 1000)

space_plot1 <- conditional_effects(quad_model, 
                                   effects = "Zspace_covered_rate",
                                   method = "fitted",
                                   re_formula = NA,
                                   robust = FALSE,
                                   samples = 1000)

guard_plot1 <- conditional_effects(quad_model, 
                                   effects = "Zprox_mid_PreyGuarding",
                                   method = "fitted",
                                   re_formula = NA,
                                   robust = FALSE,
                                   samples = 1000)

hook_plot1 <- conditional_effects(quad_model, 
                                   effects = "Zhook_start_time",
                                   method = "fitted",
                                   re_formula = NA,
                                   robust = FALSE,
                                   samples = 1000)

preyspeed_plot1 <- conditional_effects(quad_model, 
                                       effects = "Zprey_avg_speed",
                                       method = "fitted",
                                       re_formula = NA,
                                       robust = FALSE,
                                       samples = 1000)

preyspace_plot1 <- conditional_effects(quad_model, 
                                       effects = "Zprey_avg_space_covered_rate",
                                       method = "fitted",
                                       re_formula = NA,
                                       robust = FALSE,
                                       samples = 1000)

# Run the sampling including the random effects -------------------------

speed_plot2 <- conditional_effects(quad_model, 
                                   effects = "Zspeed",
                                   method = "fitted",
                                   re_formula = NULL, # condition on ranefs
                                   robust = FALSE, # plots the mean
                                   samples = 1000)

space_plot2 <- conditional_effects(quad_model, 
                                   effects = "Zspace_covered_rate",
                                   method = "fitted",
                                   re_formula = NULL,
                                   robust = FALSE,
                                   samples = 1000)

guard_plot2 <- conditional_effects(quad_model, 
                                   effects = "Zprox_mid_PreyGuarding",
                                   method = "fitted",
                                   re_formula = NULL,
                                   robust = FALSE,
                                   samples = 1000)

hook_plot2 <- conditional_effects(quad_model, 
                                  effects = "Zhook_start_time",
                                  method = "fitted",
                                  re_formula = NULL,
                                  robust = FALSE,
                                  samples = 1000)

preyspeed_plot2 <- conditional_effects(quad_model, 
                                       effects = "Zprey_avg_speed",
                                       method = "fitted",
                                       re_formula = NULL,
                                       robust = FALSE,
                                       samples = 1000)

preyspace_plot2 <- conditional_effects(quad_model, 
                                       effects = "Zprey_avg_space_covered_rate",
                                       method = "fitted",
                                       re_formula = NULL,
                                       robust = FALSE,
                                       samples = 1000)

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Prepare the sample tables
# =======================================================================


# Extract the tables from the objects -----------------------------------

speed_tab <- speed_plot1$Zspeed
space_tab <- space_plot1$Zspace_covered_rate
guard_tab <- guard_plot1$Zprox_mid_PreyGuarding
hook_tab  <- hook_plot1$Zhook_start_time
preyspeed_tab <- preyspeed_plot1$Zprey_avg_speed
preyspace_tab <- preyspace_plot1$Zprey_avg_space_covered_rate


# Bind the two tables ---------------------------------------------------

# Here, I extract the prediction intervals from the second set of tables
speed_tab <- as.data.table(cbind(speed_tab, 
                   upper_pred_int = speed_plot2$Zspeed[, "upper__"],
                   lower_pred_int = speed_plot2$Zspeed[, "lower__"]))

space_tab <- as.data.table(cbind(space_tab, 
                   upper_pred_int = space_plot2$Zspace_covered_rate[, "upper__"],
                   lower_pred_int = space_plot2$Zspace_covered_rate[, "lower__"]))

guard_tab <- as.data.table(cbind(guard_tab, 
                   upper_pred_int = guard_plot2$Zprox_mid_PreyGuarding[, "upper__"],
                   lower_pred_int = guard_plot2$Zprox_mid_PreyGuarding[, "lower__"]))

hook_tab <- as.data.table(cbind(hook_tab, 
                  upper_pred_int = hook_plot2$Zhook_start_time[, "upper__"],
                  lower_pred_int = hook_plot2$Zhook_start_time[, "lower__"]))

preyspeed_tab <- as.data.table(cbind(preyspeed_tab, 
                   upper_pred_int = preyspeed_plot2$Zprey_avg_speed[, "upper__"],
                   lower_pred_int = preyspeed_plot2$Zprey_avg_speed[, "lower__"]))

preyspace_tab <- as.data.table(cbind(preyspace_tab, 
                   upper_pred_int = preyspace_plot2$Zprey_avg_space_covered_rate[, "upper__"],
                   lower_pred_int = preyspace_plot2$Zprey_avg_space_covered_rate[, "lower__"]))


# Bind everything together to have 1 table ------------------------------

speed_tab[, x_variable := "speed"]
space_tab[, x_variable := "space"]
guard_tab[, x_variable := "guard"]
hook_tab [, x_variable := "hook"]
preyspeed_tab[, x_variable := "prey_speed"]
preyspace_tab[, x_variable := "prey_space"]

full_table <- rbind(speed_tab, space_tab,
                    guard_tab, hook_tab,
                    preyspeed_tab, preyspace_tab)

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Save the outputs
# =======================================================================

saveRDS(full_table,
        file = "03C_draws-table.rds")

# =======================================================================
# =======================================================================
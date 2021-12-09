# =======================================================================

#                Extract the effects of x on y for the base 
#                          hunting success model 

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

base_model <- readRDS("03B_hunting_success_base-model1.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Extract the samples
# =======================================================================


# Run the sampling excluding the random effects -------------------------

speed_plot1 <- conditional_effects(base_model, 
                                   effects = "Zspeed",
                                   method = "fitted",
                                   re_formula = NA, # exclude ranefs
                                   robust = FALSE, # plots the mean
                                   samples = 1000)

space_plot1 <- conditional_effects(base_model, 
                                   effects = "Zspace_covered_rate",
                                   method = "fitted",
                                   re_formula = NA,
                                   robust = FALSE,
                                   samples = 1000)

guard_plot1 <- conditional_effects(base_model, 
                                   effects = "Zprox_mid_PreyGuarding",
                                   method = "fitted",
                                   re_formula = NA,
                                   robust = FALSE,
                                   samples = 1000)

hook_plot1 <- conditional_effects(base_model, 
                                   effects = "Zhook_start_time",
                                   method = "fitted",
                                   re_formula = NA,
                                   robust = FALSE,
                                   samples = 1000)



# Run the sampling including the random effects -------------------------

speed_plot2 <- conditional_effects(base_model, 
                                   effects = "Zspeed",
                                   method = "fitted",
                                   re_formula = NULL, # condition on ranefs
                                   robust = FALSE, # plots the mean
                                   samples = 1000)

space_plot2 <- conditional_effects(base_model, 
                                   effects = "Zspace_covered_rate",
                                   method = "fitted",
                                   re_formula = NULL,
                                   robust = FALSE,
                                   samples = 1000)

guard_plot2 <- conditional_effects(base_model, 
                                   effects = "Zprox_mid_PreyGuarding",
                                   method = "fitted",
                                   re_formula = NULL,
                                   robust = FALSE,
                                   samples = 1000)

hook_plot2 <- conditional_effects(base_model, 
                                  effects = "Zhook_start_time",
                                  method = "fitted",
                                  re_formula = NULL,
                                  robust = FALSE,
                                  samples = 1000)

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Save the outputs
# =======================================================================


# 1st set of tables -----------------------------------------------------

saveRDS(speed_plot1, file = "base-model_speed-fe.rds")
saveRDS(space_plot1, file = "base-model_space-fe.rds")
saveRDS(guard_plot1, file = "base-model_guard-fe.rds")
saveRDS(hook_plot1, file = "base-model_hook-fe.rds")



# 2nd set of tables -----------------------------------------------------

saveRDS(speed_plot2, file = "base-model_speed-re.rds")
saveRDS(space_plot2, file = "base-model_space-re.rds")
saveRDS(guard_plot2, file = "base-model_guard-re.rds")
saveRDS(hook_plot2, file = "base-model_hook-re.rds")

# =======================================================================
# =======================================================================
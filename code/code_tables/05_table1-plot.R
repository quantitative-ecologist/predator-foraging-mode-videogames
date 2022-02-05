# ========================================================================

#                       Code to produce Table I                          #

# ========================================================================





# ========================================================================
# 1. Load librairies and the model
# ========================================================================


# Load libraries ---------------------------------------------------------

library(data.table)
library(flextable)
library(officer)
library(dplyr)
library(brms)



# Load the model ---------------------------------------------------------

model <- readRDS(here::here("outputs", "models",
                            "03C_hunting_success_quadratic-model2.rds"))

# Create a table of fixed effects
table <- data.table(fixef(model), keep.rownames = TRUE)
setnames(table, "rn", "coefficient")
table[, Est.Error := NULL]


# ========================================================================
# ========================================================================





# ========================================================================
# 2. Prepare the table
# ========================================================================


# Round values to 2 digits -----------------------------------------------

table[, c("Estimate", "Q2.5", "Q97.5") := 
        lapply(.SD, function (x) format(round(x, digits = 2), 
                                        nsmall = 2)),
      .SDcols = c(2:4)]



# Compute the CI column --------------------------------------------------

table[, "CI" := apply(cbind(Q2.5, Q97.5),
                      1, function(x) paste(sort(x), collapse = ", "))]

table[, "CI2" := paste("(", table$CI, sep = "")]
table[, "CI3" := paste(table$CI2, ")", sep = "")]
table[, c(3, 4, 5, 6) := NULL]
setnames(table, "CI3", "CI")



# Compute the estimate column --------------------------------------------

table[, Estimate := paste(table$Estimate, CI, sep = " ")][, CI := NULL]



# Change coefficient names -----------------------------------------------

# delete the game duration values
table <- table[coefficient != "Zgame_duration", ]

coefficient1 <- list(
  "intercept",
  "travel speed",
  "space covered",
  "time guarding",
  "time 1st capture",
  "prey travel speed",
  "prey space covered",
  "travel speed",
  "space covered",
  "time guarding",
  "time 1st capture",
  "prey travel speed",
  "prey space covered",
  "travel speed:\nspace covered",
  "travel speed:\ntime guarding",
  "travel speed:\ntime 1st capture",
  "space covered:\ntime guarding",
  "space covered:\ntime 1st capture",
  "time guarding:\ntime 1st capture",
  "travel speed:\nprey travel speed",
  "travel speed:\nprey space covered",
  "space covered:\nprey travel speed",
  "space covered:\nprey space covered",
  "time guarding:\nprey travel speed",
  "time guarding:\nprey space covered"
)

table[, coefficient := coefficient1]

# ========================================================================
# ========================================================================





# ========================================================================
# 3. Reshape the table
# ========================================================================


# Compute the coefficient names ------------------------------------------

# Trait names
newtab <- table[2:7, 1]

# Predator trait interactions names
newtab <- rbind(newtab, table[14:19, 1])

# Pred-Prey interactions names
newtab <- rbind(newtab, table[20:25, 1])



# Compute the coefficient estimates --------------------------------------

# Linear estimates
estimate1 <- c(table[8, 2], table[9, 2],
               table[10, 2], table[11, 2],
               table[12, 2], table[13, 2],
               rep("-", 12))

# Quadratic estimates
estimate2 <- c(table[2, 2], table[3, 2],
               table[4, 2], table[5, 2],
               table[6, 2], table[7, 2],
               rep("-", 12))

# Predator trait interactions estimates
estimate3 <- c(rep("-", 6), 
               table[14, 2], table[15, 2],
               table[16, 2], table[17, 2],
               table[18, 2], table[19, 2],
               rep("-", 6))

# Pred-prey interactions estimates 
estimate4 <- c(rep("-", 12), 
               table[20, 2], table[21, 2],
               table[22, 2], table[23, 2],
               table[24, 2], table[25, 2])



# Bind everything together -----------------------------------------------

# Bind estimates
newtab <- cbind(newtab,
                estimate1 = estimate1,
                estimate2 = estimate2,
                estimate3 = estimate3,
                estimate4 = estimate4)

# ========================================================================
# ========================================================================





# ========================================================================
# 4. Compute the table using flextable
# ========================================================================

# Custom header
my_header1 <- data.frame(
  col_keys = c("coefficient",
               "estimate1",
               "estimate2",
               "estimate3",
               "estimate4"),
  line1 = c("Predictor",
            "Linear (95% CI)",
            "Quadratic (95% CI)",
            "Predator trait interactions (95% CI)",
            "Predator-prey trait interactions (95% CI)"),
  stringsAsFactors = FALSE
)

# Custom theme
my_theme <- function(x, ...) {
  x <- colformat_double(x, big.mark = "'", decimal.mark = ",", digits = 1)
  x <- set_table_properties(x, layout = "fixed")
  x <- border_remove(x)
  std_border <- fp_border(width = 1, color = "black")
  x <- hline_top(x, part = "all", border = std_border)
  x <- hline_bottom(x, part = "all", border = std_border)
  autofit(x)
}

# Create the table
table1 <- newtab %>%
  select(coefficient, estimate1, estimate2,
         estimate3, estimate4) %>%
  flextable(col_keys = my_header1$col_keys) %>%
  set_header_df(mapping = my_header1, key = "col_keys") %>%
  my_theme %>%
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all", j = 1) %>% # left align first column
  width(j = c(1:5), width = 1.6) %>% # control table width
  height(height = .25) %>%
  hrule(rule = "exact")

save_as_image(table1, "./manuscript/table1.png",
              webshot = "webshot2")

# ========================================================================
# ========================================================================
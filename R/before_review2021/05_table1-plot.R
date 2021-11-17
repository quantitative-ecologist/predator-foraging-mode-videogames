##########################################################

#               Code to produce Table I                  #

##########################################################





# ========================================================
# Load librairies and import table
# ========================================================
# Load libraries
library(data.table)
library(flextable)
library(officer)
library(dplyr)

# Load the coefficient table
table <- readRDS(here::here("outputs", "05_hunting_success_coef-table.rds"))

# ========================================================
# ========================================================





# ========================================================
# Prepare the table
# ========================================================

# Round values to 2 digits
table[, c("Estimate", "Q2.5", "Q97.5") := 
        lapply(.SD, function (x) format(round(x, digits = 2), nsmall = 2)),
      .SDcols = c(2:4)]

# Compute the CI column
table[, "CI" := apply(cbind(Q2.5, Q97.5),
                      1, function(x) paste(sort(x), collapse = ", "))]

table[, "CI2" := paste("(", table$CI, sep = "")]
table[, "CI3" := paste(table$CI2, ")", sep = "")]
table[, c(3, 4, 6, 7) := NULL]
setnames(table, "CI3", "CI")

# Compute the estimate column
table[, Estimate := paste(table$Estimate, CI, sep = " ")][, CI := NULL]

# Reorder columns
setcolorder(table, neworder = c(3, 1, 2))

# Change coefficient names
coefficient1 <- list(
  "intercept",
  "travel speed",
  "space covered",
  "ambush time",
  "time 1st capture",
  "prey travel speed",
  "prey space covered",
  "travel speed",
  "space covered",
  "ambush time",
  "time 1st capture",
  "travel speed:space covered",
  "travel speed:ambush time",
  "travel speed:time 1st capture",
  "space covered:ambush time",
  "space covered:time 1st capture",
  "ambush time:time 1st capture",
  "prey travel speed",
  "prey space covered",
  "travel speed:prey travel speed",
  "travel speed:prey space covered",
  "space covered:prey travel speed",
  "space covered:prey space covered",
  "ambush time:prey travel speed",
  "ambush time:prey space covered",
  "time 1st capture:prey travel speed",
  "time 1st capture:prey space covered"
)

table[, coefficient := coefficient1]

# ========================================================
# ========================================================





# ========================================================
#               Reshape the table
# ========================================================

# Trait names
newtab <- table[2:7, 2]

# Predator trait interactions names
newtab <- rbind(newtab, table[12:17, 2])

# Pred-Prey interactions names
newtab <- rbind(newtab, table[20:27, 2])

# linear estimates
estimate1 <- c(table[2,3], table[3,3],
               table[4,3], table[5,3],
               table[6,3], table[7,3],
               rep("-", 14))

# Quadratic estimates
estimate2 <- c(table[8,3], table[9,3],
               table[10,3], table[11,3],
               table[18,3], table[19,3],
               rep("-", 14))

# Predator trait interactions estimates
estimate3 <- c(rep("-", 6), 
               table[12,3], table[13,3],
               table[14,3], table[15,3],
               table[16,3], table[17,3],
               rep("-", 8))

# Pred-prey interactions estimates 
estimate4 <- c(rep("-", 12), 
               table[20,3], table[21,3],
               table[22,3], table[23,3],
               table[24,3], table[25,3],
               table[26,3], table[27,3])

# Bind estimates
newtab <- cbind(newtab, estimate1 = estimate1,
                estimate2 = estimate2,
                estimate3 = estimate3,
                estimate4 = estimate4)

# ========================================================
# ========================================================





# ========================================================
# Compute the table using flextable
# ========================================================

# Custom header
my_header1 <- data.frame(
  col_keys = c("coefficient", "estimate1",
               "estimate2", "estimate3", "estimate4"),
  line1 = c("Predictor", "Linear (95% CI)",
            "Quadratic (95% CI)", "Predator trait interactions (95% CI)",
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
table1 <-newtab %>%
  select(coefficient, estimate1, estimate2,
         estimate3, estimate4) %>%
  flextable(col_keys = my_header1$col_keys) %>%
  set_header_df(mapping = my_header1, key = "col_keys") %>%
  my_theme %>%
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all", j = 1) %>% # left align first column
  width(j = c(1:5), width = 1.6) %>% # control table width
  height(height = .25) %>%
  hrule(rule = "exact")

save_as_image(table1, "./manuscript/table1.png")

# ========================================================
# ========================================================
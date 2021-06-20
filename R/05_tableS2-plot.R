
################################################################

#                   Code to produce Table SII                  #

################################################################





# ========================================================
# Load librairies and import table
# ========================================================

# Load libraries
library(data.table)
library(flextable)
library(officer)
library(dplyr)

# Source the files
icc_tab2 <- readRDS("./outputs/03A_icc-table2.rds")
icc_tab3 <- readRDS("./outputs/03A_icc-table3.rds")

# ========================================================
# ========================================================





# ========================================================
# Reorganize the table
# ========================================================

# Round values to 2 digits
icc_tab2[, c("mean", "lower", "upper") := 
        lapply(.SD, function (x) format(round(x, digits = 2), nsmall = 2)),
      .SDcols = c(2:4)]

icc_tab3[, c("mean", "lower", "upper") := 
        lapply(.SD, function (x) format(round(x, digits = 2), nsmall = 2)),
      .SDcols = c(2:4)]

# Compute the CI column
icc_tab2[, "CI" := apply(cbind(lower, upper),
                      1, function(x) paste(sort(x), collapse = ", "))]

icc_tab2[, "CI2" := paste("(", icc_tab2$CI, sep = "")]
icc_tab2[, "CI3" := paste(icc_tab2$CI2, ")", sep = "")]
icc_tab2[, c(3, 4, 6, 7) := NULL]
setnames(icc_tab2, "CI3", "CI")

icc_tab3[, "CI" := apply(cbind(lower, upper),
                      1, function(x) paste(sort(x), collapse = ", "))]

icc_tab3[, "CI2" := paste("(", icc_tab3$CI, sep = "")]
icc_tab3[, "CI3" := paste(icc_tab3$CI2, ")", sep = "")]
icc_tab3[, c(3, 4, 6, 7) := NULL]
setnames(icc_tab3, "CI3", "CI")


# Compute the estimate column
icc_tab2[, icc := paste(icc_tab2$mean, CI, sep = " ")][,mean := NULL][, CI := NULL]
icc_tab3[, icc := paste(icc_tab3$mean, CI, sep = " ")][,mean := NULL][, CI := NULL]

# Change response variable names (behaviors)
group1 <- c(
  "travel speed",
  "space covered",
  "ambush time",
  "time 1st capture",
  "travel speed",
  "space covered",
  "ambush time",
  "time 1st capture",
  "travel speed",
  "space covered",
  "ambush time",
  "time 1st capture",
  "travel speed",
  "space covered",
  "ambush time",
  "time 1st capture"
)

ranef_variable1 <- c(
  "player ID",
  "player ID",
  "player ID",
  "player ID",
  "game environment",
  "game environment",
  "game environment",
  "game environment",
  "avatar",
  "avatar",
  "avatar",
  "avatar",
  "residuals",
  "residuals",
  "residuals",
  "residuals"
)

icc_tab2[, group := as.factor(group1)][, ranef_variable := as.factor(ranef_variable1)]
icc_tab3[, group := as.factor(group1)][, ranef_variable := as.factor(ranef_variable1)]

# Reorder the table
icc_tab2 <- icc_tab2[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]
icc_tab3 <- icc_tab3[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]

# Remove repeated
icc_tab2[c(2:4,6:8,10:12,14:16), group := " "]
icc_tab3[c(2:4,6:8,10:12,14:16), group := " "]

# Combine tables to have two ICC columns
icc_tab2 <- cbind(icc_tab2, icc3 = icc_tab3$icc)

# ========================================================
# ========================================================



# ========================================================
# # Compute the table using flextable
# ========================================================

# Custom header
my_header <- data.frame(
  col_keys = c("group", "ranef_variable", "icc"),
  line1 = c("Behavior", "Random effect", "ICC (95% CI)"),
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
icc_table <- icc_tab2 %>%
  select(group, ranef_variable, icc) %>%
  flextable(col_keys = my_header$col_keys) %>%
  set_header_df(mapping = my_header, key = "col_keys") %>%
  my_theme() %>%
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  align(align = "left", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "body", j = 1) %>%
  align(align = "left", part = "body", j = 2) %>%
  width(j = c(1:3), width = 2) %>%
  height(height = .01) %>%
  hrule(rule = "exact")

save_as_image(icc_table2, "./manuscript/tableS2.png")

# ========================================================
# ========================================================
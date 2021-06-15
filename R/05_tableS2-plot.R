
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

# Source the PCA file
icc_tab <- readRDS("./outputs/03A_icc-table1.rds")

# ========================================================
# ========================================================





# ========================================================
# Reorganize the table
# ========================================================

# Round values to 2 digits
icc_tab[, c("mean", "lower", "upper") := 
        lapply(.SD, function (x) format(round(x, digits = 2), nsmall = 2)),
      .SDcols = c(2:4)]

# Compute the CI column
icc_tab[, "CI" := apply(cbind(lower, upper),
                      1, function(x) paste(sort(x), collapse = ", "))]

icc_tab[, "CI2" := paste("(", icc_tab$CI, sep = "")]
icc_tab[, "CI3" := paste(icc_tab$CI2, ")", sep = "")]
icc_tab[, c(3, 4, 6, 7) := NULL]
 setnames(icc_tab, "CI3", "CI")

# Compute the estimate column
icc_tab[, icc := paste(icc_tab$mean, CI, sep = " ")][,mean := NULL][, CI := NULL]

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

icc_tab[, group := as.factor(group1)][, ranef_variable := as.factor(ranef_variable1)]

# Reorder factor levels
group1 <-  as.factor(c("travel speed", "travel speed", 
                       "travel speed", "travel speed",
                       "space covered", "space covered", 
                       "space covered", "space covered",
                       "ambush time", "ambush time", 
                       "ambush time", "ambush time",
                       "time 1st capture", "time 1st capture", 
                       "time 1st capture", "time 1st capture"))
group2 <-  as.factor(c("travel speed",
                       "space covered",
                       "ambush time",
                       "time 1st capture"))

icc_tab$group <- factor(icc_tab$group, levels=unique(group1))

# ========================================================
# ========================================================



# ========================================================
# # Compute the table using flextable
# ========================================================

# Custom header
my_header <- data.frame(
  col_keys = c("Behavior", "Random effect", "ICC (95% CI)"),
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
icc_tab %>%
  select(group, ranef_variable, icc) %>%
  flextable(col_keys = my_header$col_keys) %>%
  set_header_df(mapping = my_header, key = "col_keys") %>%
  my_theme() %>%
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "body", j = 1) %>%
  #width(j = c(1:3), width = 1.3) %>%
  height(height = .3) %>%
  hrule(rule = "exact")

save_as_image(tableS1, "./manuscript/tableS1.png")

# ========================================================
# ========================================================
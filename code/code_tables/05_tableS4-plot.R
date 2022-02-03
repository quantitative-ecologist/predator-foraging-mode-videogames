
# ===========================================================================

#                          Code to produce Table SIV                        #

# ===========================================================================





# ===========================================================================
# 1. Load librairies and import table
# ===========================================================================

# Load libraries
library(data.table)
library(flextable)
library(officer)
library(dplyr)

# Source the files
loo_tab <- readRDS("./outputs/R_objects/loo-cv-table.rds")

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 3. Compute the table using flextable
# ===========================================================================

# Prepare the table parameters ----------------------------------------------

# Custom header
my_header <- data.frame(
  col_keys = c("model",
               "elpd_diff",
               "se_diff",
               "elpd_loo",
               "se_elpd_loo"),
  line1 = c("Model",
            "elpd difference",
            "standard error difference",
            "elpd loo value",
            "standard error"),
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



# Create the table ----------------------------------------------------------

loo_table <- loo_tab %>%
  select(model, elpd_diff, se_diff, elpd_loo, se_elpd_loo) %>%
  flextable(col_keys = my_header$col_keys) %>%
  set_header_df(mapping = my_header, key = "col_keys") %>%
  my_theme() %>%
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all", j = 1) %>%
  align(align = "left", part = "all", j = 2) %>%
  align(align = "center", part = "all", j = 3) %>%
  align(align = "center", part = "all", j = 4) %>%
  align(align = "center", part = "all", j = 5) %>%
  width(j = c(1:5), width = 1.5) %>%
  height(height = .01) %>%
  hrule(rule = "exact") #%>%
 # footnote(i = 1, j = 3:4,
 #           value = as_paragraph(
 #             c("Model without the effect of prey behavior",
 #               "Model including the game environment surface area")
 #           ),
 #           ref_symbols = c("*", "*"),
 #           part = "header")

# Save the table
save_as_image(loo_table,
              "./manuscript/tableS4.png",
              webshot = "webshot2")

# ===========================================================================
# ===========================================================================
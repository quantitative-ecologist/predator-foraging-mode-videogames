
# ==============================================================================

#                           Code to produce Table SI                           #

# ==============================================================================





# ==============================================================================
# 1. Load librairies and import the tables
# ==============================================================================

# Load libraries ---------------------------------------------------------------

library(data.table)
library(flextable)
library(officer)
library(dplyr)



# Import the tables ------------------------------------------------------------

contrib_tab <- readRDS("./outputs/R_objects/02_PCA_contrib-table.rds")
corr_tab <- readRDS("./outputs/R_objects/02_PCA_corr-table.rds")



# Arrange tables ---------------------------------------------------------------

# Compute seperate tables based on PCs
pc1_tab <- data.table("variable" = contrib_tab$rn,
                      "correlation_pc1" = corr_tab$Dim.1,
                      "variance_pc1" = contrib_tab$Dim.1)

pc2_tab <- data.table("variable" = contrib_tab$rn,
                      "correlation_pc2" = corr_tab$Dim.2,
                      "variance_pc2" = contrib_tab$Dim.2)

# ==============================================================================
# ==============================================================================





# ==============================================================================
# 2. Compute the table using flextable
# ==============================================================================

# Prepare the table parameters -------------------------------------------------

# Custom header
my_header <- data.frame(
  col_keys = c("variable", "correlation_pc1", "variance_pc1", 
               "correlation_pc2", "variance_pc2"),
  line1 = c("", rep("PC1 (% variance = 21.0)", 2), rep("PC2 (% variance = 15.3)", 2)),
  line2 = c(" ", "Correlation", "% variance", "Correlation", "% variance"),
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



# Create the table -------------------------------------------------------------

tableS1 <- cbind(pc1_tab, pc2_tab[,c(2:3)]) %>%
  select(variable, correlation_pc1, variance_pc1,
         correlation_pc2, variance_pc2) %>%
  flextable(col_keys = my_header$col_keys) %>%
  set_header_df(mapping = my_header, key = "col_keys") %>%
  my_theme() %>%
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "body", j = 1) %>%
  width(j = c(1:5), width = 1.3) %>%
  height(height = .3) %>%
  hrule(rule = "exact")

save_as_image(tableS1, 
              "./manuscript/tableS1.png",
              webshot = "webshot2")

# ==============================================================================
# ==============================================================================
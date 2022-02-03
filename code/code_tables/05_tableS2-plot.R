
# ===========================================================================

#                          Code to produce Table SII                        #

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
icc_tab1 <- readRDS("./outputs/R_objects/03A_icc-table1.rds")
icc_tab3 <- readRDS("./outputs/R_objects/03A_icc-table3.rds")
icc_tab4 <- readRDS("./outputs/R_objects/03A_icc-table4.rds")

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 2. Reorganize the table
# ===========================================================================

# Round values to 2 digits --------------------------------------------------

icc_tab1[, c("mean", "lower", "upper") := 
        lapply(.SD, function (x) format(round(x, digits = 2), nsmall = 2)),
      .SDcols = c(2:4)]

icc_tab3[, c("mean", "lower", "upper") := 
        lapply(.SD, function (x) format(round(x, digits = 2), nsmall = 2)),
      .SDcols = c(2:4)]

icc_tab4[, c("mean", "lower", "upper") := 
        lapply(.SD, function (x) format(round(x, digits = 2), nsmall = 2)),
      .SDcols = c(2:4)]



# Compute the CI column -----------------------------------------------------

# tab1
icc_tab1[, "CI" := apply(cbind(lower, upper),
                         1,
                         function(x) paste(sort(x), collapse = ", ")
                    )
]

icc_tab1[, "CI2" := paste("(", icc_tab1$CI, sep = "")]
icc_tab1[, "CI3" := paste(icc_tab1$CI2, ")", sep = "")]
icc_tab1[, c(3, 4, 6, 7) := NULL]
setnames(icc_tab1, "CI3", "CI")


# tab3
icc_tab3[, "CI" := apply(cbind(lower, upper),
                      1,
                      function(x) paste(sort(x), collapse = ", ")
                    )
]

icc_tab3[, "CI2" := paste("(", icc_tab3$CI, sep = "")]
icc_tab3[, "CI3" := paste(icc_tab3$CI2, ")", sep = "")]
icc_tab3[, c(3, 4, 6, 7) := NULL]
setnames(icc_tab3, "CI3", "CI")


# tab4
icc_tab4[, "CI" := apply(cbind(lower, upper),
                         1,
                         function(x) paste(sort(x), collapse = ", ")
                    )
]

icc_tab4[, "CI2" := paste("(", icc_tab4$CI, sep = "")]
icc_tab4[, "CI3" := paste(icc_tab4$CI2, ")", sep = "")]
icc_tab4[, c(3, 4, 6, 7) := NULL]
setnames(icc_tab4, "CI3", "CI")



# Compute the estimate column -----------------------------------------------

icc_tab1[, icc := paste(icc_tab1$mean, CI, sep = " ")][
  , mean := NULL][
    , CI := NULL]

icc_tab3[, icc := paste(icc_tab3$mean, CI, sep = " ")][
  , mean := NULL][
    , CI := NULL]

icc_tab4[, icc := paste(icc_tab4$mean, CI, sep = " ")][
  , mean := NULL][
    , CI := NULL]

# Change response variable names (behaviors)
group1 <- c(
  "travel speed",
  "space covered",
  "guard time",
  "time 1st capture",
  "travel speed",
  "space covered",
  "guard time",
  "time 1st capture",
  "travel speed",
  "space covered",
  "guard time",
  "time 1st capture",
  "travel speed",
  "space covered",
  "guard time",
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

icc_tab1[, group := as.factor(group1)][
  , ranef_variable := as.factor(ranef_variable1)]

icc_tab3[, group := as.factor(group1)][
  , ranef_variable := as.factor(ranef_variable1)]

icc_tab4[, group := as.factor(group1)][
  , ranef_variable := as.factor(ranef_variable1)]



# Final adjustments ---------------------------------------------------------

# Reorder the table
icc_tab1 <- icc_tab1[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]
icc_tab3 <- icc_tab3[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]
icc_tab4 <- icc_tab4[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]

# Remove repeated
icc_tab1[c(2:4,6:8,10:12,14:16), group := " "]
icc_tab3[c(2:4,6:8,10:12,14:16), group := " "]
icc_tab4[c(2:4,6:8,10:12,14:16), group := " "]

# Combine tables to have two ICC columns
icc_tab1 <- cbind(icc_tab1,
                  icc3 = icc_tab3$icc,
                  icc4 = icc_tab4$icc)

# ===========================================================================
# ===========================================================================





# ===========================================================================
# 3. Compute the table using flextable
# ===========================================================================

# Prepare the table parameters ----------------------------------------------

# Custom header
my_header <- data.frame(
  col_keys = c("group",
               "ranef_variable",
               "icc",
               "icc3",
               "icc4"),
  line1 = c("Behavior",
            "Random effect",
            "ICC (95% CI) - No prey",
            "ICC (95% CI) - Novices",
            "ICC (95% CI) - Experienced"),
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

icc_table <- icc_tab1 %>%
  select(group, ranef_variable, icc, icc3, icc4) %>%
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
  width(j = c(1:5), width = 1.8) %>%
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
save_as_image(icc_table,
              "./manuscript/tableS2.png",
              webshot = "webshot2")

# ===========================================================================
# ===========================================================================
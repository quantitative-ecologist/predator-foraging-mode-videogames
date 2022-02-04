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
library(brms)


# Import model objects --------------------------------------------------

model1 <- readRDS("./outputs/models/03A_multivariate-model1.rds")
model3 <- readRDS("./outputs/models/03A_multivariate-model-novice.rds")
model4 <- readRDS("./outputs/models/03A_multivariate-model-experienced.rds")

# ===========================================================================
# ===========================================================================





# =======================================================================
# 2. Prepare correlation tables
# =======================================================================


# Extract correlation samples -------------------------------------------

correlations1 <- data.table(
      as_draws_df(model1, 
                  variable = c("^cor_", "^rescor_"),
                  regex = TRUE))

correlations3 <- data.table(
      as_draws_df(model3, 
                  variable = c("^cor_", "^rescor_"),
                  regex = TRUE))

correlations4 <- data.table(
      as_draws_df(model4, 
                  variable = c("^cor_", "^rescor_"),
                  regex = TRUE))

# Filter to keep only individual level correlations
correlations1 <- correlations1[, c(13:24)]
correlations3 <- correlations3[, c(13:24)]
correlations4 <- correlations4[, c(13:24)]



# Create table with mean icc and credibility intervals ------------------

lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}




# Create the correlation table ------------------------------------------

cor_tab1 <- data.table(group = c("speed~space_id", "speed~guard_id",
                                 "space~guard_id", "speed~hook_id",
                                 "space~hook_id", "guard~hook_id",
                                 "speed~space_res", "speed~guard_res",
                                 "space~guard_res", "speed~hook_res",
                                 "space~hook_res", "guard~hook_res"),
                       mean = as.numeric(correlations1[, lapply(.SD, mean),
                                                   .SDcols = c(1:12)]),
                       lower = as.numeric(correlations1[, lapply(.SD, lower_interval),
                                                   .SDcols = c(1:12)]),
                       upper = as.numeric(correlations1[, lapply(.SD, upper_interval),
                                                   .SDcols = c(1:12)])
                        )

cor_tab3 <- data.table(group = c("speed~space_id", "speed~guard_id",
                                 "space~guard_id", "speed~hook_id",
                                 "space~hook_id", "guard~hook_id",
                                 "speed~space_res", "speed~guard_res",
                                 "space~guard_res", "speed~hook_res",
                                 "space~hook_res", "guard~hook_res"),
                       mean = as.numeric(correlations3[, lapply(.SD, mean),
                                                   .SDcols = c(1:12)]),
                       lower = as.numeric(correlations3[, lapply(.SD, lower_interval),
                                                   .SDcols = c(1:12)]),
                       upper = as.numeric(correlations3[, lapply(.SD, upper_interval),
                                                   .SDcols = c(1:12)])
                        )

cor_tab4 <- data.table(group = c("speed~space_id", "speed~guard_id",
                                 "space~guard_id", "speed~hook_id",
                                 "space~hook_id", "guard~hook_id",
                                 "speed~space_res", "speed~guard_res",
                                 "space~guard_res", "speed~hook_res",
                                 "space~hook_res", "guard~hook_res"),
                       mean = as.numeric(correlations4[, lapply(.SD, mean),
                                                   .SDcols = c(1:12)]),
                       lower = as.numeric(correlations4[, lapply(.SD, lower_interval),
                                                   .SDcols = c(1:12)]),
                       upper = as.numeric(correlations4[, lapply(.SD, upper_interval),
                                                   .SDcols = c(1:12)])
                        )

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Make adjustments to the table
# =======================================================================

# Round the column values -----------------------------------------------
cor_tab1[, c("mean", "lower", "upper") := 
        lapply(.SD, function (x) format(round(x, digits = 2), nsmall = 2)),
      .SDcols = c(2:4)]

cor_tab3[, c("mean", "lower", "upper") := 
        lapply(.SD, function (x) format(round(x, digits = 2), nsmall = 2)),
      .SDcols = c(2:4)]

cor_tab4[, c("mean", "lower", "upper") := 
        lapply(.SD, function (x) format(round(x, digits = 2), nsmall = 2)),
      .SDcols = c(2:4)]



# merge everything into 1 corr column -----------------------------------

# paste the values of lower and upper as 1 column
cor_tab1[, "CI" := apply(cbind(lower, upper),
                         1,
                         function(x) paste(sort(x), collapse = ",")
                    )
]

cor_tab3[, "CI" := apply(cbind(lower, upper),
                         1,
                         function(x) paste(sort(x), collapse = ",")
                    )
]

cor_tab4[, "CI" := apply(cbind(lower, upper),
                         1,
                         function(x) paste(sort(x), collapse = ",")
                    )
]


# Paste "("  and ")" in new columns
cor_tab1[, "CI2" := paste("(", cor_tab1$CI, sep = "")]
cor_tab1[, "CI3" := paste(cor_tab1$CI2, ")", sep = "")]
cor_tab1[, c(3, 4, 5, 6) := NULL]
setnames(cor_tab1, "CI3", "CI")

cor_tab3[, "CI2" := paste("(", cor_tab3$CI, sep = "")]
cor_tab3[, "CI3" := paste(cor_tab3$CI2, ")", sep = "")]
cor_tab3[, c(3, 4, 5, 6) := NULL]
setnames(cor_tab3, "CI3", "CI")

cor_tab4[, "CI2" := paste("(", cor_tab4$CI, sep = "")]
cor_tab4[, "CI3" := paste(cor_tab4$CI2, ")", sep = "")]
cor_tab4[, c(3, 4, 5, 6) := NULL]
setnames(cor_tab4, "CI3", "CI")


# Create the final column
cor_tab1[, corr := paste(cor_tab1$mean, CI, sep = " ")][
  , mean := NULL][
    , CI := NULL]

cor_tab3[, corr := paste(cor_tab3$mean, CI, sep = " ")][
  , mean := NULL][
    , CI := NULL]

cor_tab4[, corr := paste(cor_tab4$mean, CI, sep = " ")][
  , mean := NULL][
    , CI := NULL]

# =======================================================================
# =======================================================================





# =======================================================================
# 4. Prepare correlation matrices
# =======================================================================

# correlation matrix no prey --------------------------------------------

speed <- c(
           "-",
           cor_tab1[1,2],
           cor_tab1[2,2],
           cor_tab1[4,2]
)

space <- c(
           cor_tab1[7,2],
           "-",
           cor_tab1[3,2],
           cor_tab1[5,2]
)

guard <- c(
      cor_tab1[8,2],
      cor_tab1[9,2],
      "-",
      cor_tab1[6,2]
)

time <- c(
      cor_tab1[10,2],
      cor_tab1[11,2],
      cor_tab1[12,2],
      "-"
)


id_cor_matrix1 <- cbind(speed, space, guard, time)

rownames(id_cor_matrix1) <- c("speed", "space",
                              "prey guarding", "time 1st cap.")
colnames(id_cor_matrix1) <- c("speed", "space",
                              "prey guarding", "time 1st cap.")



# correlation matrix novice ---------------------------------------------

speed <- c(
           "-",
           cor_tab3[1,2],
           cor_tab3[2,2],
           cor_tab3[4,2]
)

space <- c(
           cor_tab3[7,2],
           "-",
           cor_tab3[3,2],
           cor_tab3[5,2]
)

guard <- c(
      cor_tab3[8,2],
      cor_tab3[9,2],
      "-",
      cor_tab3[6,2]
)

time <- c(
      cor_tab3[10,2],
      cor_tab3[11,2],
      cor_tab3[12,2],
      "-"
)


id_cor_matrix3 <- cbind(speed, space, guard, time)

rownames(id_cor_matrix3) <- c("speed", "space",
                              "prey guarding", "time 1st cap.")
colnames(id_cor_matrix3) <- c("speed", "space",
                              "prey guarding", "time 1st cap.")



# correlation matrix experienced ----------------------------------------

speed <- c(
           "-",
           cor_tab4[1,2],
           cor_tab4[2,2],
           cor_tab4[4,2]
)

space <- c(
           cor_tab4[7,2],
           "-",
           cor_tab4[3,2],
           cor_tab4[5,2]
)

guard <- c(
      cor_tab4[8,2],
      cor_tab4[9,2],
      "-",
      cor_tab4[6,2]
)

time <- c(
      cor_tab4[10,2],
      cor_tab4[11,2],
      cor_tab4[12,2],
      "-"
)


id_cor_matrix4 <- cbind(speed, space, guard, time)

rownames(id_cor_matrix4) <- c("speed", "space",
                              "prey guarding", "time 1st cap.")
colnames(id_cor_matrix4) <- c("speed", "space",
                              "prey guarding", "time 1st cap.")



 # Bind all matrices ----------------------------------------------------

tab1 <- data.table(id_cor_matrix1,
                   Model = "without prey",
                   keep.rownames = TRUE)

tab3 <- data.table(id_cor_matrix3,
                   Model = "novice players",
                   keep.rownames = TRUE)

tab4 <- data.table(id_cor_matrix3,
                   Model = "experienced players",
                   keep.rownames = TRUE)

tab <- data.frame(rbind(tab1, tab3, tab4))

# =======================================================================
# =======================================================================





# ===========================================================================
# 5. Compute the table using flextable
# ===========================================================================

# Prepare the table parameters ----------------------------------------------

# Custom header
my_header <- data.frame(
  col_keys = c("rn",
               "speed",
               "space",
               "prey.guarding",
               "time.1st.cap."),
  line1 = c("",
            "speed",
            "space",
            "prey guarding",
            "time 1st cap."),       
  stringsAsFactors = FALSE
)

# Custom theme
my_theme <- function(x, ...) {
  x <- colformat_double(x, big.mark = " ", decimal.mark = ".", digits = 1)
  x <- set_table_properties(x, layout = "fixed")
  x <- border_remove(x)
  std_border <- fp_border(width = 1, color = "black")
  x <- hline_top(x, part = "all", border = std_border)
  x <- hline_bottom(x, part = "all", border = std_border)
  autofit(x)
}



# Create the table ----------------------------------------------------------

tab <- as_grouped_data(tab,
                       groups = "Model") %>%
           
           as_flextable(col_keys = my_header$col_keys) %>%
           
           set_header_df(mapping = my_header,
                         key = "col_keys") %>%
           
           my_theme() %>%

           align(align = "center",
                 part = "body",
                 j = "rn",
                 i = c(1, 6, 11)) %>%
           
            align(align = "center",
                  part = "all",
                  j = c(2, 3, 4, 5)) %>%

           bold(i = ~ !is.na(Model),
                bold = TRUE) %>%

           width(j = c(1:5),
                 width = 1.2) %>%
           height(height = .01) %>%
           hrule(rule = "exact") %>%
          
           footnote(i = 1, j = 1,
              part = "header",
              value = as_paragraph(
                  "* The among-individual correlations are on the lower off-diagonal and the residual within-individual correlations on the upper off-diagonal"),
              ref_symbols = " ") %>%
           
           fontsize(size = 10, part = "all") %>%
           font(fontname = "Times New Roman", part = "all")

# Save the table
save_as_image(table,
              "./manuscript/tableS3.png",
              webshot = "webshot2")

# ===========================================================================
# ===========================================================================
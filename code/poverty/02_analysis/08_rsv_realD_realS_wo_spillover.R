# =============================================================================
# Estimate RSV: Real Treatment Effects & Real Sample Definitions
# =============================================================================

rm(list = ls())


## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(parallel)
  library(argparse)
})
options(readr.show_col_types = F)
source("code/poverty/utils/rsv_fun.R")

## Parse Command-Line Arguments
parser <- ArgumentParser(description = "Estimate RSV: Real Treatment Effects & Real Sample Definitions w/o Spillover")
parser$add_argument("-c", "--cores", type = "integer", default = 245)
parser$add_argument("-b", "--B", type = "integer", default = 1000)
parser$add_argument("-y", "--Y_vars", type = "character", nargs = "+", default = c("Ycons", "Ylowinc", "Ymidinc"))
args <- parser$parse_args()

Y_vars <- c(args$Y_vars)
cores  <- min(detectCores() - 1, args$cores)
B <- args$B
cat(sprintf(
  "[INFO] Y_vars = %s; B = %d; cores = %d;\n", paste(Y_vars, collapse = ","), B, cores
))

## Parallelization Strategy
cores_1 <- min(cores, length(Y_vars))
cores_2 <- min(B, max(1, floor((cores - cores_1) / cores_1)))
cat(sprintf("[INFO] Parallelizing %d outcomes × %d cores/boot\n", cores_1, cores_2))

## Load data and define samples
# we always assume R/X is not missing and is observed
# We also assume that Y is not missing and is observed
data <- read_csv("data/poverty/processed/poverty_data.csv") %>%
  filter(spillover_20km == FALSE) %>%
  mutate(
    Se = !is.na(D), # wave %in% c("Treatment", "Control", "Buffer") # Se = all experimental villages (treated and untreated)
    So = wave %in% c("Holdout", "Buffer"), # So = Observational villages + "buffer" untreated experimental villages 
    Ycons = if_else(So == TRUE, Ycons, NA), # Outcome Y is observed only for S = o
    Ylowinc = if_else(So == TRUE, Ylowinc, NA),
    Ymidinc = if_else(So == TRUE, Ymidinc, NA)
  )
  
## RSV Estimation Function (per outcome)
run_one <- function(Y_var) {
  d <- data %>%
    mutate(Y = !!sym(Y_var)) 

  # Features
  X <- d %>%
    select(
      starts_with("viirs_annual_"),
      starts_with("feature_")
    )

  # Fit RSV
  set.seed(42)
  out <- rsv_fun(
    X_train = X, D_train = d$D, Y_train = d$Y, Se_train = d$Se, So_train = d$So,
    X_test  = X, D_test  = d$D, Y_test  = d$Y, Se_test  = d$Se, So_test  = d$So,
    classwt = c(10, 1), ntree = 100, eps = 1e-2,
    se.boot = TRUE, clusters_test = d$clusters, B = B, cores = cores_2
  )

  # Export Results
  output_path <- sprintf("data/poverty/interim/rsv_realD_realS_%s_wo_spillover.rds", Y_var)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, output_path)

  # Return Summary
  sprintf("[RSV] wo spillover realD realS %s; coef = %.3f (%.3f); denominator = %.3f (%.3f) → saved to %s\n", Y_var, out$coef, out$se, out$denominator, out$denominator_se, output_path)
}


## Parallel Estimation Across Outcomes
results <- mclapply(Y_vars, run_one, mc.cores = cores_1)


## Print Summary
for (i in seq_along(results))
  cat(results[[i]])

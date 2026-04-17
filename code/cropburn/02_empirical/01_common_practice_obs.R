# =============================================================================
# Crop Burning — Common-Practice Estimator (Observational Sample)
#
# Estimates the treatment effect of D on each RS predictor (R_max, R_bal,
# R_prob) using OLS on the experimental subsample. This is the "common
# practice" benchmark: regress the remote sensing outcome on treatment,
# ignoring the observational sample entirely.
#
# Input:  data/clean/cropburn/obs_data.csv
# Output: data/interim/cropburn/fit_common_practice_obs.Rds
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(parallel)
  library(fixest)
  library(readr)
})
source("code/utils/common_practice.R")


# -----------------------------------------------------------------------------
# 1. Load data and extract inputs
# -----------------------------------------------------------------------------

df <- read_csv("data/clean/cropburn/obs_data.csv", show_col_types = FALSE)
D        <- df$D
clusters <- df$village_id

# -----------------------------------------------------------------------------
# 2. Estimate common-practice TE for each RS predictor
#
# Loops over the three RS predictors and fits R ~ D on the experimental
# subsample, with cluster bootstrap SEs.
# -----------------------------------------------------------------------------

seed        <- 42L
B_se        <- 5000
num.threads <- 7

fits <- list()
for (R_var in c("R_max", "R_bal", "R_prob")) {
  R    <- df[[R_var]]
  S_e  <- !is.na(D) & !is.na(R)
  coef <- fit_common_practice(D, R, S_e)
  fits[[R_var]] <- add_se_common_practice(coef, D, R, S_e, 
                                          clusters    = clusters,
                                          B           = B_se,
                                          seed        = seed,
                                          num.threads = num.threads)
  cat(sprintf("Common-practice estimate (%s ~ D): %.3f (SE: %.3f)\n",
              R_var, fits[[R_var]]$coefficients, fits[[R_var]]$se))
}


# -----------------------------------------------------------------------------
# 3. Save
# -----------------------------------------------------------------------------

out_path <- "data/interim/cropburn/fit_common_practice_obs.Rds"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(fits, out_path)
cat("Saved to", out_path, "\n")

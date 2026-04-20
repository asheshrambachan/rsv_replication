# =============================================================================
# Crop Burning — Summarize Common-Practice Estimates
#
# Extracts coefficients and bootstrap SEs from the fitted common-practice
# objects for both the observational and validation samples and writes a
# single flat CSV consumed by 03_tables/01_tab_common_practice_obs.R and
# 03_tables/02_tab_estimators_jtest.R.
#
# Input:
#   data/interim/cropburn/fit_common_practice_obs.Rds
#   data/interim/cropburn/fit_common_practice_val.Rds
#
# Output:
#   data/clean/cropburn/empirical_common_practice.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages(library(dplyr))

R_vars <- c("R_max", "R_bal", "R_prob")

# -----------------------------------------------------------------------------
# 1. Load and flatten both samples
# -----------------------------------------------------------------------------

results <- bind_rows(lapply(c("obs", "val"), function(sample) {
  fits <- readRDS(sprintf("data/interim/cropburn/fit_common_practice_%s.Rds", sample))
  bind_rows(lapply(R_vars, function(rv) {
    data.frame(
      sample = sample,
      R_var  = rv,
      coef   = as.numeric(fits[[rv]]$coefficients),
      se     = as.numeric(fits[[rv]]$se)
    )
  }))
}))

# -----------------------------------------------------------------------------
# 2. Save
# -----------------------------------------------------------------------------

out_path <- "data/clean/cropburn/empirical_common_practice.csv"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write.csv(results, out_path, row.names = FALSE)
cat("Saved:", out_path, "\n")
cat(sprintf("  %d rows, %d columns\n", nrow(results), ncol(results)))

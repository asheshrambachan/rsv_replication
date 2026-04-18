# =============================================================================
# Crop Burning — Summarize RSV Empirical Results
#
# Extracts RSV and naive estimates, bootstrap SEs, and J-test statistics from
# the fitted RSV objects for both samples. Also pulls the R_max common-practice
# estimate (used in 03_tables/02_tab_estimators_jtest.R) from the CP fits and
# joins it in.
#
# Input:
#   data/interim/cropburn/fit_rsv_obs.Rds
#   data/interim/cropburn/fit_rsv_val.Rds
#   data/interim/cropburn/fit_common_practice_obs.Rds
#   data/interim/cropburn/fit_common_practice_val.Rds
#
# Output:
#   data/clean/cropburn/empirical_results.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages(library(dplyr))
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# -----------------------------------------------------------------------------
# 1. Load fits and compute J-test for both samples
# -----------------------------------------------------------------------------

results <- bind_rows(lapply(c("obs", "val"), function(sample) {
  fit_rsv <- readRDS(sprintf("data/interim/cropburn/fit_rsv_%s.Rds", sample))
  fit_cp  <- readRDS(sprintf("data/interim/cropburn/fit_common_practice_%s.Rds", sample))
  jt      <- jtest(fit_rsv)

  data.frame(
    sample     = sample,
    rsv_coef   = as.numeric(fit_rsv$coefficients),
    rsv_se     = as.numeric(fit_rsv$se),
    naive_coef = as.numeric(fit_rsv$coefficients_naive),
    naive_se   = as.numeric(fit_rsv$se_naive),
    cp_coef    = as.numeric(fit_cp[["R_max"]]$coefficients),
    cp_se      = as.numeric(fit_cp[["R_max"]]$se),
    J          = jt$J,
    p_value    = jt$p_value
  )
}))

# -----------------------------------------------------------------------------
# 2. Save
# -----------------------------------------------------------------------------

out_path <- "data/clean/cropburn/empirical_results.csv"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write.csv(results, out_path, row.names = FALSE)
cat("Saved:", out_path, "\n")
cat(sprintf("  %d rows, %d columns\n", nrow(results), ncol(results)))

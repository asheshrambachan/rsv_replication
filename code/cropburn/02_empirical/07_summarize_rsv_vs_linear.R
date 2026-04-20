# =============================================================================
# Crop Burning — Summarize RSV vs Common-Practice (Observational Sample)
#
# Merges the RSV and common-practice bootstrap draws on their shared seeds to
# produce paired SEs. Storing the aligned SEs here keeps the table script free
# of raw bootstrap objects.
#
# Input:
#   data/interim/cropburn/fit_rsv_obs.Rds
#   data/interim/cropburn/fit_common_practice_obs.Rds
#
# Output:
#   data/clean/cropburn/empirical_rsv_vs_linear.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages(library(dplyr))

# -----------------------------------------------------------------------------
# 1. Load fits
# -----------------------------------------------------------------------------

fit_rsv    <- readRDS("data/interim/cropburn/fit_rsv_obs.Rds")
fit_linear <- readRDS("data/interim/cropburn/fit_common_practice_obs.Rds")[["R_max"]]

# -----------------------------------------------------------------------------
# 2. Align bootstrap draws by seed to compute paired SEs
#
# Both estimators share the same cluster bootstrap seeds, so merging on seed_b
# pairs each RSV draw with the corresponding common-practice draw.
# -----------------------------------------------------------------------------

rsv_draws <- data.frame(
  seed_b  = fit_rsv$bootstrap$seed_draws,
  tau_rsv = fit_rsv$bootstrap$tau_draws
)
lin_draws <- data.frame(
  seed_b   = fit_linear$bootstrap$seed_draws,
  coef_lin = fit_linear$bootstrap$coef_draws
)
draws <- merge(rsv_draws, lin_draws, by = "seed_b")

# -----------------------------------------------------------------------------
# 3. Flatten to one row per estimator
# -----------------------------------------------------------------------------

results <- data.frame(
  estimator = c("rsv", "common_practice"),
  coef      = c(as.numeric(fit_rsv$coefficients), as.numeric(fit_linear$coefficients)),
  se        = c(sd(draws$tau_rsv, na.rm = TRUE), sd(draws$coef_lin, na.rm = TRUE))
)

# -----------------------------------------------------------------------------
# 4. Save
# -----------------------------------------------------------------------------

out_path <- "data/clean/cropburn/empirical_rsv_vs_linear.csv"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write.csv(results, out_path, row.names = FALSE)
cat("Saved:", out_path, "\n")
cat(sprintf("  %d rows, %d columns\n", nrow(results), ncol(results)))

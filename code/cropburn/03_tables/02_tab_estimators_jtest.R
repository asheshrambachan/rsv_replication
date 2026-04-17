# =============================================================================
# Crop Burning — Table: Estimator Comparison and J-Test
#
# Produces a LaTeX table comparing three treatment effect estimators —
# RSV (optimal representation), naive (simple representation), and common
# practice — for both the validation and observational samples side by side.
# Also reports the J-test statistic and p-value for overidentifying
# restrictions, which tests whether the RSV and naive estimates are consistent.
#
# Inputs:
#   data/interim/cropburn/fit_rsv_val.Rds
#   data/interim/cropburn/fit_rsv_obs.Rds
#   data/interim/cropburn/fit_common_practice_val.Rds
#   data/interim/cropburn/fit_common_practice_obs.Rds
#
# Output: tables/cropburn/tab_estimators_jtest.tex
# =============================================================================

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

level <- 0.90
fmt4  <- function(x) formatC(round(x, 4), format = "f", digits = 4)

# -----------------------------------------------------------------------------
# 1. Load fits and extract estimates
# -----------------------------------------------------------------------------

# Validation sample
fit_val       <- readRDS("data/interim/cropburn/fit_rsv_val.Rds")
est_rsv_val   <- as.numeric(fit_val$coefficients)
est_naive_val <- as.numeric(fit_val$coefficients_naive)
se_rsv_val    <- as.numeric(fit_val$se)
se_naive_val  <- as.numeric(fit_val$se_naive)
jt_val        <- jtest(fit_val)

cp_val     <- readRDS("data/interim/cropburn/fit_common_practice_val.Rds")[["R_max"]]
est_cp_val <- as.numeric(cp_val$coefficients)
se_cp_val  <- as.numeric(cp_val$se)

# Observational sample
fit_obs       <- readRDS("data/interim/cropburn/fit_rsv_obs.Rds")
est_rsv_obs   <- as.numeric(fit_obs$coefficients)
est_naive_obs <- as.numeric(fit_obs$coefficients_naive)
se_rsv_obs    <- as.numeric(fit_obs$se)
se_naive_obs  <- as.numeric(fit_obs$se_naive)
jt_obs        <- jtest(fit_obs)

cp_obs     <- readRDS("data/interim/cropburn/fit_common_practice_obs.Rds")[["R_max"]]
est_cp_obs <- as.numeric(cp_obs$coefficients)
se_cp_obs  <- as.numeric(cp_obs$se)

# -----------------------------------------------------------------------------
# 2. Build LaTeX table
# -----------------------------------------------------------------------------

tex <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{l | cc}",
  "\\toprule",
  " & Validation & Observational \\\\",
  "\\midrule",
  sprintf("  Optimal representation &  %s  &  %s  \\\\", fmt4(est_rsv_val),   fmt4(est_rsv_obs)),
  sprintf("                         & (%s) & (%s) \\\\", fmt4(se_rsv_val),    fmt4(se_rsv_obs)),
  sprintf("  Simple representation  &  %s  &  %s  \\\\", fmt4(est_naive_val), fmt4(est_naive_obs)),
  sprintf("                         & (%s) & (%s) \\\\", fmt4(se_naive_val),  fmt4(se_naive_obs)),
  sprintf("  Common practice        &  %s  &  %s  \\\\", fmt4(est_cp_val),    fmt4(est_cp_obs)),
  sprintf("                         & (%s) & (%s) \\\\", fmt4(se_cp_val),     fmt4(se_cp_obs)),
  "\\midrule",
  sprintf("  $J$-statistic & %s & %s \\\\", fmt4(jt_val$J),       fmt4(jt_obs$J)),
  sprintf("  $p$-value     & %s & %s \\\\", fmt4(jt_val$p_value), fmt4(jt_obs$p_value)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{threeparttable}"
)

out_path <- "tables/cropburn/tab_estimators_jtest.tex"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, out_path)
cat("Saved to", out_path, "\n")

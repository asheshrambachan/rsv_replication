# =============================================================================
# Crop Burning — Table: RSV vs Common-Practice (Observational Sample)
#
# Produces a LaTeX table comparing the RSV treatment effect estimate against
# the common-practice estimate (R_max ~ D) on the observational sample.
# Bootstrap SEs are aligned by seed so that paired draws can be used to
# compute the SE of the difference between the two estimators.
#
# Inputs:
#   data/interim/cropburn/fit_rsv_obs.Rds
#   data/interim/cropburn/fit_common_practice_obs.Rds
#
# Output: tables/cropburn/tab_rsv_vs_linear_obs.tex
# =============================================================================

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

level <- 0.90
fmt4  <- function(x) formatC(round(x, 4), format = "f", digits = 4)

# -----------------------------------------------------------------------------
# 1. Load fits
# -----------------------------------------------------------------------------

fit_rsv    <- readRDS("data/interim/cropburn/fit_rsv_obs.Rds")
fit_linear <- readRDS("data/interim/cropburn/fit_common_practice_obs.Rds")[["R_max"]]

# -----------------------------------------------------------------------------
# 2. Align bootstrap draws by seed to compute paired SEs
#
# Both estimators use the same cluster bootstrap seeds, so merging on seed_b
# pairs each RSV draw with the corresponding common-practice draw. SEs are
# computed from this aligned distribution.
# -----------------------------------------------------------------------------

rsv_draws <- data.frame(
  seed_b  = fit_rsv$bootstrap$seed_draws,
  tau_rsv = fit_rsv$bootstrap$tau_draws
)
lin_draws <- data.frame(
  seed_b  = fit_linear$bootstrap$seed_draws,
  coef_lin = fit_linear$bootstrap$coef_draws
)
draws <- merge(rsv_draws, lin_draws, by = "seed_b")

se_rsv <- sd(draws$tau_rsv,  na.rm = TRUE)
se_lin <- sd(draws$coef_lin, na.rm = TRUE)

est_rsv <- as.numeric(fit_rsv$coefficients)
est_lin <- as.numeric(fit_linear$coefficients)

# -----------------------------------------------------------------------------
# 3. Build LaTeX table
# -----------------------------------------------------------------------------

tex <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{l | c c}",
  " \\toprule",
  " & Common practice & Our method \\\\",
  " \\midrule",
  sprintf(" Treatment effect & %s & %s \\\\", fmt4(est_lin), fmt4(est_rsv)),
  sprintf("                  & (%s) & (%s) \\\\", fmt4(se_lin), fmt4(se_rsv)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{threeparttable}"
)

out_path <- "tables/cropburn/tab_rsv_vs_linear_obs.tex"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, out_path)
cat("Saved to", out_path, "\n")

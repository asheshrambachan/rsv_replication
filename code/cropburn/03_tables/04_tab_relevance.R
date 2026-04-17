# =============================================================================
# Crop Burning — Table: Relevance of the RS Predictor
#
# Produces a LaTeX table reporting the relevance of the remote sensing
# predictor for both the validation and observational samples side by side.
# Relevance measures how informative R is for treatment effect estimation;
# a higher value indicates a stronger predictor.
#
# Inputs:
#   data/interim/cropburn/fit_rsv_val.Rds
#   data/interim/cropburn/fit_rsv_obs.Rds
#
# Output: tables/cropburn/tab_relevance.tex
# =============================================================================

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

level <- 0.90
fmt4  <- function(x) formatC(round(x, 4), format = "f", digits = 4)

# -----------------------------------------------------------------------------
# 1. Load fits and compute relevance
# -----------------------------------------------------------------------------

fit_val <- readRDS("data/interim/cropburn/fit_rsv_val.Rds")
rel_val <- relevance(fit_val, level = level)
r_val   <- rel_val[rel_val$estimator == "rsv", ]

fit_obs <- readRDS("data/interim/cropburn/fit_rsv_obs.Rds")
rel_obs <- relevance(fit_obs, level = level)
r_obs   <- rel_obs[rel_obs$estimator == "rsv", ]

# -----------------------------------------------------------------------------
# 2. Build LaTeX table
# -----------------------------------------------------------------------------

tex <- c(
  "\\begin{tabular}{l | cc}",
  "\\toprule",
  " & Validation & Observational \\\\",
  "\\midrule",
  sprintf("  Relevance & %s & %s \\\\", fmt4(r_val$estimate), fmt4(r_obs$estimate)),
  sprintf("            & (%s) & (%s) \\\\", fmt4(r_val$se), fmt4(r_obs$se)),
  sprintf("  %.0f\\%% CI  & [%s, %s] & [%s, %s] \\\\",
          100 * level,
          fmt4(r_val$ci_lower), fmt4(r_val$ci_upper),
          fmt4(r_obs$ci_lower), fmt4(r_obs$ci_upper)),
  "\\bottomrule",
  "\\end{tabular}"
)

out_path <- "tables/cropburn/tab_relevance.tex"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, out_path)
cat("Saved to", out_path, "\n")

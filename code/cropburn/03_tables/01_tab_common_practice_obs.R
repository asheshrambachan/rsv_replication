# =============================================================================
# Crop Burning — Table: Common-Practice Estimates (Observational Sample)
#
# Produces a LaTeX table reporting the common-practice treatment effect
# estimates for each of the three RS predictors (R_max, R_bal, R_prob),
# with bootstrap standard errors. This is the benchmark estimator that
# regresses R on D using only the experimental subsample.
#
# Input:  data/interim/cropburn/fit_common_practice_obs.Rds
# Output: tables/cropburn/tab_common_practice_obs.tex
# =============================================================================

fmt3 <- function(x) formatC(round(x, 3), format = "f", digits = 3)

fits <- readRDS("data/interim/cropburn/fit_common_practice_obs.Rds")

tex <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{l | c c c}",
  "\\toprule",
  "Outcome: & \\shortstack{Maximum \\\\ Accuracy} & \\shortstack{Balanced \\\\ Accuracy} & \\shortstack{Random Forest \\\\ Predictions} \\\\",
  " & (1) & (2) & (3) \\\\",
  "\\midrule",
  sprintf(
    "  Estimate & %s & %s & %s \\\\",
    fmt3(fits[["R_max"]]$coefficients),
    fmt3(fits[["R_bal"]]$coefficients),
    fmt3(fits[["R_prob"]]$coefficients)
  ),
  sprintf(
    "           & (%s) & (%s) & (%s) \\\\",
    fmt3(fits[["R_max"]]$se),
    fmt3(fits[["R_bal"]]$se),
    fmt3(fits[["R_prob"]]$se)
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{threeparttable}"
)

out_path <- "tables/cropburn/tab_common_practice_obs.tex"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, out_path)
cat("Saved to", out_path, "\n")

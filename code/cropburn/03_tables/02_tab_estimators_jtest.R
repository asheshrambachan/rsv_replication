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
#   data/clean/cropburn/empirical_results.csv  (from 02_empirical/06_summarize_results.R)
#
# Output: tables/cropburn/tab_estimators_jtest.tex
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

fmt4 <- function(x) formatC(round(x, 4), format = "f", digits = 4)

res <- read.csv("data/clean/cropburn/empirical_results.csv")
val <- res[res$sample == "val", ]
obs <- res[res$sample == "obs", ]

tex <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{l | cc}",
  "\\toprule",
  " & Validation & Observational \\\\",
  "\\midrule",
  sprintf("  Optimal representation &  %s  &  %s  \\\\", fmt4(val$rsv_coef),   fmt4(obs$rsv_coef)),
  sprintf("                         & (%s) & (%s) \\\\", fmt4(val$rsv_se),     fmt4(obs$rsv_se)),
  sprintf("  Simple representation  &  %s  &  %s  \\\\", fmt4(val$naive_coef), fmt4(obs$naive_coef)),
  sprintf("                         & (%s) & (%s) \\\\", fmt4(val$naive_se),   fmt4(obs$naive_se)),
  sprintf("  Common practice        &  %s  &  %s  \\\\", fmt4(val$cp_coef),    fmt4(obs$cp_coef)),
  sprintf("                         & (%s) & (%s) \\\\", fmt4(val$cp_se),      fmt4(obs$cp_se)),
  "\\midrule",
  sprintf("  $J$-statistic & %s & %s \\\\", fmt4(val$J),       fmt4(obs$J)),
  sprintf("  $p$-value     & %s & %s \\\\", fmt4(val$p_value), fmt4(obs$p_value)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{threeparttable}"
)

out_path <- "tables/cropburn/tab_estimators_jtest.tex"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, out_path)
cat("Saved to", out_path, "\n")

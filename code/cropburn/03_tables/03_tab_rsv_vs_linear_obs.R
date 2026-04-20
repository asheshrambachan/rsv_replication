# =============================================================================
# Crop Burning — Table: RSV vs Common-Practice (Observational Sample)
#
# Produces a LaTeX table comparing the RSV treatment effect estimate against
# the common-practice estimate (R_max ~ D) on the observational sample.
# Paired bootstrap SEs (aligned by seed) are pre-computed in
# 02_empirical/07_summarize_rsv_vs_linear.R.
#
# Input:  data/clean/cropburn/empirical_rsv_vs_linear.csv
# Output: tables/cropburn/tab_rsv_vs_linear_obs.tex
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

fmt4 <- function(x) formatC(round(x, 4), format = "f", digits = 4)

res <- read.csv("data/clean/cropburn/empirical_rsv_vs_linear.csv")
rsv <- res[res$estimator == "rsv", ]
lin <- res[res$estimator == "common_practice", ]

tex <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{l | c c}",
  " \\toprule",
  " & Common practice & Our method \\\\",
  " \\midrule",
  sprintf(" Treatment effect & %s & %s \\\\", fmt4(lin$coef), fmt4(rsv$coef)),
  sprintf("                  & (%s) & (%s) \\\\", fmt4(lin$se), fmt4(rsv$se)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{threeparttable}"
)

out_path <- "tables/cropburn/tab_rsv_vs_linear_obs.tex"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, out_path)
cat("Saved to", out_path, "\n")

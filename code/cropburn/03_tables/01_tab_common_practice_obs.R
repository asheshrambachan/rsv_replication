# =============================================================================
# Crop Burning — Table: Common-Practice Estimates (Observational Sample)
#
# Produces a LaTeX table reporting the common-practice treatment effect
# estimates for each of the three RS predictors (R_max, R_bal, R_prob),
# with bootstrap standard errors. This is the benchmark estimator that
# regresses R on D using only the experimental subsample.
#
# Input:  data/clean/cropburn/empirical_common_practice.csv  (from 02_empirical/05_summarize_common_practice.R)
# Output: tables/cropburn/tab_common_practice_obs.tex
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

fmt3 <- function(x) formatC(round(x, 3), format = "f", digits = 3)

cp  <- read.csv("data/clean/cropburn/empirical_common_practice.csv")
obs <- cp[cp$sample == "obs", ]

tex <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{l | c c c}",
  "\\toprule",
  " & \\shortstack{Maximum \\\\ Accuracy} & \\shortstack{Balanced \\\\ Accuracy} & \\shortstack{Random Forest \\\\ Predictions} \\\\",
  "\\midrule",
  sprintf(
    "  Estimate & %s & %s & %s \\\\",
    fmt3(obs[obs$R_var == "R_max", "coef"]),
    fmt3(obs[obs$R_var == "R_bal", "coef"]),
    fmt3(obs[obs$R_var == "R_prob", "coef"])
  ),
  sprintf(
    "           & (%s) & (%s) & (%s) \\\\",
    fmt3(obs[obs$R_var == "R_max", "se"]),
    fmt3(obs[obs$R_var == "R_bal", "se"]),
    fmt3(obs[obs$R_var == "R_prob", "se"])
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{threeparttable}"
)

out_path <- "tables/cropburn/tab_common_practice_obs.tex"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, out_path)
cat("Saved to", out_path, "\n")

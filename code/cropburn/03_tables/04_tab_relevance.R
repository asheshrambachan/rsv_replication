# =============================================================================
# Crop Burning — Table: Relevance of the RS Predictor
#
# Produces a LaTeX table reporting the relevance of the remote sensing
# predictor for both the validation and observational samples side by side.
# Relevance measures how informative R is for treatment effect estimation;
# a higher value indicates a stronger predictor.
#
# Input:  data/clean/cropburn/empirical_relevance.csv  (from 02_empirical/08_summarize_relevance.R)
# Output: tables/cropburn/tab_relevance.tex
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

level <- 0.90
fmt4  <- function(x) formatC(round(x, 4), format = "f", digits = 4)

rel <- read.csv("data/clean/cropburn/empirical_relevance.csv")
val <- rel[rel$sample == "val", ]
obs <- rel[rel$sample == "obs", ]

tex <- c(
  "\\begin{tabular}{l | cc}",
  "\\toprule",
  " & Validation & Observational \\\\",
  "\\midrule",
  sprintf("  Relevance & %s & %s \\\\", fmt4(val$estimate), fmt4(obs$estimate)),
  sprintf("            & (%s) & (%s) \\\\", fmt4(val$se), fmt4(obs$se)),
  sprintf("  %.0f\\%% CI  & [%s, %s] & [%s, %s] \\\\",
          100 * level,
          fmt4(val$ci_lower), fmt4(val$ci_upper),
          fmt4(obs$ci_lower), fmt4(obs$ci_upper)),
  "\\bottomrule",
  "\\end{tabular}"
)

out_path <- "tables/cropburn/tab_relevance.tex"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, out_path)
cat("Saved to", out_path, "\n")

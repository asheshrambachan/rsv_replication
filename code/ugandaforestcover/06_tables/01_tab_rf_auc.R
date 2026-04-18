# =============================================================================
# Uganda Forest Cover — RF Predictor Performance Table
#
# Reports AUC for the random forest predictor on the training and simulation
# splits, one row per split.
#
# Input:
#   data/clean/ugandaforestcover/rf_auc_Ybin.csv
#
# Output:
#   tables/ugandaforestcover/tab_rf_auc.tex
# =============================================================================

library(data.table)

outcome <- "Ybin"

auc <- fread(file.path("data/clean/ugandaforestcover", paste0("rf_auc_", outcome, ".csv")))

split_labels <- c(train = "Training", sim = "Simulation (held-out)")

# ------------------------------------------------------------------------------
# Build LaTeX table
# ------------------------------------------------------------------------------
lines <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{lr}",
  "\\toprule",
  "Split & AUC \\\\",
  "\\midrule"
)

for (s in c("train", "sim")) {
  r   <- auc[auc$split == s, ]
  lbl <- split_labels[s]
  lines <- c(lines, sprintf("%s & $%.3f$ \\\\", lbl, r$AUC))
}

lines <- c(
  lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\small",
  paste0(
    "\\item \\textit{Notes:} Area under the ROC curve (AUC) for the random forest ",
    "predictor of $Y_{\\text{bin}}$ trained on MOSAIKS satellite features. ",
    "The training split was used to fit the model; the simulation split was ",
    "held out entirely and never seen during training."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}"
)

latex_table <- paste(lines, collapse = "\n")

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
tables_dir <- "tables/ugandaforestcover"
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

out_path <- file.path(tables_dir, "tab_rf_auc.tex")
writeLines(latex_table, out_path)
cat("Saved:", out_path, "\n")

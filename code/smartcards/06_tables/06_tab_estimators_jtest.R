# ==============================================================================
# Table: Efficient vs Simple Representations, with J-test
#
# Compares the efficient RSV estimate (H(R) weights) to the simple plug-in
# estimate (P(Y=1|R,S=o) weights), and reports the J-test statistic for the
# overidentifying restriction that the two representations agree.
#
# Reads:  data/clean/smartcards/empirical_results.csv  (from 04_empirical/02_summarize.R)
# Saves:  tables/smartcards/empirical/{sample}/tab_estimators_jtest.tex
# ==============================================================================

suppressPackageStartupMessages(library(dplyr))

Y_vars     <- c("Ycons", "Ylowinc", "Ymidinc")

all_results <- read.csv("data/clean/smartcards/empirical_results.csv")
col_labels <- c(Ycons = "Consumption", Ylowinc = "Low income", Ymidinc = "Middle income")

fmt4 <- function(x) formatC(round(x, 4), format = "f", digits = 4)

ncols      <- length(Y_vars)
col_header <- paste0(
  "Representations: & ",
  paste(sprintf("\\multicolumn{1}{c}{%s}", col_labels[Y_vars]), collapse = " & "),
  " \\\\"
)
col_nums <- paste0(
  "& ",
  paste(sprintf("(%d)", seq_along(Y_vars)), collapse = " & "),
  " \\\\"
)

# Produce table for both the main sample and the no-spillover robustness check
for (sample in c("full", "nospillover")) {
  res <- all_results[all_results$sample == sample, ]

  est_row <- function(field_coef, field_se, row_label) {
    coefs <- sapply(Y_vars, function(y) fmt4(as.numeric(res[res$Y_var == y, field_coef])))
    ses   <- sapply(Y_vars, function(y) fmt4(as.numeric(res[res$Y_var == y, field_se])))
    c(
      sprintf("  %s & %s \\\\", row_label, paste(coefs, collapse = " & ")),
      sprintf("           & %s \\\\", paste(sprintf("(%s)", ses), collapse = " & "))
    )
  }

  jstat_row <- paste0(
    "  $J$-statistic & ",
    paste(sapply(Y_vars, function(y) fmt4(res[res$Y_var == y, "J"])), collapse = " & "),
    " \\\\"
  )
  pval_row <- paste0(
    "  $p$-value     & ",
    paste(sapply(Y_vars, function(y) fmt4(res[res$Y_var == y, "p_value"])), collapse = " & "),
    " \\\\"
  )

  tex <- c(
    "\\begin{threeparttable}",
    sprintf("\\begin{tabular}{l | %s}", paste(rep("c", ncols), collapse = "")),
    "\\toprule",
    col_header,
    col_nums,
    "\\midrule",
    est_row("rsv_coef",    "rsv_se",    "Efficient"),
    "\\addlinespace[0.3em]",
    est_row("simple_coef", "simple_se", "Simple"),
    "\\midrule",
    jstat_row,
    pval_row,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{threeparttable}"
  )

  out_dir <- file.path("tables/smartcards/empirical", sample)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, "tab_estimators_jtest.tex")
  writeLines(tex, out_path)
  cat(sprintf("Saved: %s\n", out_path))
}

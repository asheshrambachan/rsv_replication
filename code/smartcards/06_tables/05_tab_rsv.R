# ==============================================================================
# Table: RSV vs benchmark, with difference
#
# Columns: Consumption, Low income, Middle income
# Rows:    RSV Estimate, Benchmark Estimate, Difference between Estimates
#
# Reads:  data/clean/smartcards/empirical_results.csv  (from 04_empirical/02_summarize.R)
# Saves:  tables/smartcards/empirical/{sample}/tab_rsv.tex
# ==============================================================================

Y_vars     <- c("Ycons", "Ylowinc", "Ymidinc")
col_labels <- c(
  Ycons   = "Consumption",
  Ylowinc = "Low income",
  Ymidinc = "Middle income"
)

fmt4 <- function(x) formatC(round(x, 4), format = "f", digits = 4)

ncols <- length(Y_vars)

col_header <- paste0(
  "& ",
  paste(sprintf("\\multicolumn{1}{c}{%s}", col_labels[Y_vars]), collapse = " & "),
  " \\\\"
)

col_nums <- paste0(
  "& ",
  paste(sprintf("(%d)", seq_along(Y_vars)), collapse = " & "),
  " \\\\"
)

# Produce table for both the main sample and the no-spillover robustness check
all_results <- read.csv("data/clean/smartcards/empirical_results.csv")

for (sample in c("full", "nospillover")) {
  res <- all_results[all_results$sample == sample, ]

  pull <- function(y, col) {
    as.numeric(res[res$Y_var == y, col])
  }

  est_row <- function(coef_col, se_col, row_label, add_midrule = FALSE) {
    coefs <- sapply(Y_vars, function(y) fmt4(pull(y, coef_col)))
    ses   <- sapply(Y_vars, function(y) fmt4(pull(y, se_col)))

    out <- c(
      sprintf("  %s & %s \\\\", row_label, paste(coefs, collapse = " & ")),
      sprintf("           & %s \\\\", paste(sprintf("(%s)", ses), collapse = " & "))
    )

    if (add_midrule) {
      out <- c(out, "\\midrule")
    }

    out
  }

  tex <- c(
    "\\begin{threeparttable}",
    sprintf("\\begin{tabular}{l | %s}", paste(rep("c", ncols), collapse = "")),
    "\\toprule",
    col_header,
    col_nums,
    "\\midrule",
    est_row("rsv_coef",   "rsv_se",   "RSV Estimate"),
    "\\midrule",
    "\\addlinespace[0.3em]",
    est_row("bench_coef", "bench_se", "Benchmark Estimate"),
    "\\midrule",
    est_row("diff_coef",  "diff_se",  "Difference between Estimates"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{threeparttable}"
  )

  out_dir <- file.path("tables/smartcards/empirical", sample)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  out_path <- file.path(out_dir, "tab_rsv.tex")
  writeLines(tex, out_path)

  cat(sprintf("Saved: %s\n", out_path))
}

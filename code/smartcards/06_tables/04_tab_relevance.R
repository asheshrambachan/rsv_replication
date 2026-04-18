# ==============================================================================
# Table: RSV relevance E_n[H(R) Delta^o]
#
# Columns: Consumption, Low income, Middle income
# Rows:    Relevance (estimate, SE, 90% CI)
#
# Reads:  data/clean/smartcards/empirical_relevance.csv  (from 04_empirical/04_summarize_relevance.R)
# Saves:  tables/smartcards/empirical/{sample}/tab_relevance.tex
# ==============================================================================

suppressPackageStartupMessages(library(dplyr))

Y_vars     <- c("Ycons", "Ylowinc", "Ymidinc")
col_labels <- c(Ycons = "Consumption", Ylowinc = "Low income", Ymidinc = "Middle income")

rel_data <- read.csv("data/clean/smartcards/empirical_relevance.csv")

fmt4 <- function(x) formatC(round(x, 4), format = "f", digits = 4)

ncols      <- length(Y_vars)
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
for (sample in c("full", "nospillover")) {

  # Formats estimate, SE, and CI rows for one estimator
  rel_row <- function(estimator_name, row_label) {
    rels <- lapply(Y_vars, function(y)
      rel_data %>% filter(.data$sample == .env$sample, Y_var == y, estimator == estimator_name)
    )
    ests <- sapply(rels, function(r) fmt4(r$estimate))
    ses  <- sapply(rels, function(r) fmt4(r$se))
    cis  <- sapply(rels, function(r) sprintf("[%s, %s]", fmt4(r$ci_lower), fmt4(r$ci_upper)))
    c(
      sprintf("  %s & %s \\\\", row_label, paste(ests, collapse = " & ")),
      sprintf("           & %s \\\\", paste(sprintf("(%s)", ses), collapse = " & ")),
      sprintf("           & %s \\\\", paste(cis, collapse = " & "))
    )
  }

  tex <- c(
    "\\begin{threeparttable}",
    sprintf("\\begin{tabular}{l | %s}", paste(rep("c", ncols), collapse = "")),
    "\\toprule",
    col_header,
    col_nums,
    "\\midrule",
    rel_row("rsv", "Relevance"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{threeparttable}"
  )

  out_dir <- file.path("tables/smartcards/empirical", sample)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, "tab_relevance.tex")
  writeLines(tex, out_path)
  cat(sprintf("Saved: %s\n", out_path))
}

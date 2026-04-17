suppressPackageStartupMessages({
  library(dplyr)
})

# ==============================================================================
# Assumption 3(ii) — No Direct Effect: KS table
#
# Tabulates the KS test results from 02_assumption_tests/01_no_direct_effect.R.
# Reports F(R_PC1 | S=e, D=1, Y=y) vs F(R_PC1 | S=e, D=0, Y=y) for each
# outcome and level of Y. Failure to reject supports D ⊥ R | S=e, Y.
#
# Input:  data/clean/smartcard/assumption_no_direct_effect.csv
# Output: tables/smartcard/tab_no_direct_effect.tex
# ==============================================================================

ks <- read.csv("data/clean/smartcard/assumption_no_direct_effect.csv")

Y_vars     <- c("Ycons", "Ylowinc", "Ymidinc")
col_labels <- c(Ycons = "Consumption", Ylowinc = "Low income", Ymidinc = "Middle income")

fmt_p <- function(p) {
  if (is.na(p))  return("--")
  if (p < 0.001) return("$<0.001$")
  sprintf("$%.3f$", p)
}

# Formats one block of table rows (one per outcome) for a given Y stratum
section_rows <- function(df) {
  rows <- character(0)
  for (Y_var in Y_vars) {
    r <- df %>% filter(outcome == Y_var)
    if (nrow(r) == 0) {
      rows <- c(rows, sprintf("\\quad %s & -- & -- & -- & -- \\\\", col_labels[Y_var]))
    } else {
      rows <- c(rows, sprintf(
        "\\quad %s & $%d$ & $%d$ & $%.3f$ & %s \\\\",
        col_labels[Y_var], r$n_d1, r$n_d0, r$ks_stat, fmt_p(r$ks_pval)
      ))
    }
  }
  rows
}

# ------------------------------------------------------------------------------
# Build LaTeX table
# ------------------------------------------------------------------------------
lines <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "Outcome & $n_1$ & $n_2$ & KS & $p$-value \\\\",
  "\\midrule",
  "\\multicolumn{5}{l}{$S=e,\\, Y=0$}\\\\",
  section_rows(ks %>% filter(Y == 0)),
  "\\addlinespace[0.6em]",
  "\\multicolumn{5}{l}{$S=e,\\, Y=1$}\\\\",
  section_rows(ks %>% filter(Y == 1)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\small",
  "\\item Notes: Each row reports the Kolmogorov--Smirnov (KS) statistic and corresponding $p$-value comparing the distribution of $R$ between treated ($D=1$) and untreated ($D=0$) units within the experimental sample ($S=e$). The sample sizes of $R \\mid D=1, S=e, Y$ and $R \\mid D=0, S=e, Y$ are denoted by $n_1$ and $n_2$, respectively.",
  "\\end{tablenotes}",
  "\\end{threeparttable}"
)

latex_table <- paste(lines, collapse = "\n")

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
tables_dir <- "tables/smartcard"
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
out_path <- file.path(tables_dir, "tab_no_direct_effect.tex")
writeLines(latex_table, out_path)
cat("Saved:", out_path, "\n")

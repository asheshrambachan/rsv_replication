suppressPackageStartupMessages({
  library(dplyr)
})


# ==============================================================================
# Assumption 2(i) -- Stability: KS table (Stilde overlap design)
#
# Tabulates the KS test results from 02_assumption_tests/03_stability_Stilde.R.
# Reports F(R_PC1 | Stilde=e, D=0, Y=y) vs F(R_PC1 | Stilde=o, D=0, Y=y) for
# each outcome and level of Y using the overlap sample definition Stilde.
# Buffer Mandals (2011) appear in both Stilde=e and Stilde=o. Failure to
# reject supports R ⊥ Stilde | D=0, Y.
#
# See 02_tab_stability_S.R for the analogous table using the non-overlapping
# definition S, where Buffer Mandals appear only in S=e.
#
# Input:  data/clean/smartcards/assumption_stability_Stilde.csv
# Output: tables/smartcards/tab_stability_Stilde.tex
# ==============================================================================

ks <- read.csv("data/clean/smartcards/assumption_stability_Stilde.csv")

Y_vars     <- c("Ycons", "Ylowinc", "Ymidinc")
col_labels <- c(Ycons = "Consumption", Ylowinc = "Low income", Ymidinc = "Middle income")

fmt_p <- function(p) {
  if (is.na(p))  return("--")
  if (p < 0.001) return("$<0.001$")
  sprintf("$%.3f$", p)
}

# Formats one block of table rows (one per outcome) for a given (D, Y) stratum
section_rows <- function(df) {
  rows <- character(0)
  for (Y_var in Y_vars) {
    r <- df %>% filter(outcome == Y_var)
    if (nrow(r) == 0) {
      rows <- c(rows, sprintf("\\quad %s & -- & -- & -- & -- \\\\", col_labels[Y_var]))
    } else {
      rows <- c(rows, sprintf(
        "\\quad %s & $%d$ & $%d$ & $%.3f$ & %s \\\\",
        col_labels[Y_var], r$n_exp, r$n_obs, r$ks_stat, fmt_p(r$ks_pval)
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
  "\\multicolumn{5}{l}{$D=0,\\, Y=0$}\\\\",
  section_rows(ks %>% filter(Y == 0)),
  "\\addlinespace[0.6em]",
  "\\multicolumn{5}{l}{$D=0,\\, Y=1$}\\\\",
  section_rows(ks %>% filter(Y == 1)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\small",
  "\\item Notes: Each row reports the Kolmogorov--Smirnov (KS) statistic and corresponding $p$-value comparing the distribution of $R$ between the experimental ($S=e$) and observational ($S=o$) units within the untreated sample ($D=0$). The sample sizes of $R \\mid S=e, D, Y$ and $R \\mid S=o, D, Y$ are denoted by $n_1$ and $n_2$, respectively.",
  "\\end{tablenotes}",
  "\\end{threeparttable}"
)

latex_table <- paste(lines, collapse = "\n")

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
tables_dir <- "tables/smartcards"
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
out_path <- file.path(tables_dir, "tab_stability_Stilde.tex")
writeLines(latex_table, out_path)
cat("Saved:", out_path, "\n")

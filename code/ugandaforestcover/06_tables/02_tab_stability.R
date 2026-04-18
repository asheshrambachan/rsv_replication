library(data.table)
library(dplyr)

# ==============================================================================
# KS test results table: F(R | Y=y, S_e) vs F(R | Y=y, S_o)
# Layout: grouped by Y class, rows = distance bands, columns = n1, n2, KS, p
# ==============================================================================

OUTCOME <- "Ybin"

ks <- read.csv("data/clean/ugandaforestcover/assumption_stability.csv")

band_order   <- c("0 - 2km",  "0 - 5km",  "0 - 10km")
band_display <- c("0--2km",   "0--5km",   "0--10km")
bands        <- band_order[band_order %in% unique(ks$S_o)]
band_labels  <- setNames(band_display, band_order)[bands]

y_classes <- sort(unique(ks$Y))

fmt_pval <- function(p) {
  if (is.na(p))  return("--")
  if (p < 0.001) return("$<0.001$")
  sprintf("$%.3f$", p)
}

# ------------------------------------------------------------------------------
# Build LaTeX table
# ------------------------------------------------------------------------------
lines <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "Observational Sample & $n_1$ & $n_2$ & KS & $p$-value \\\\",
  "\\midrule"
)

for (i in seq_along(y_classes)) {
  y <- y_classes[i]

  # Group header
  lines <- c(lines, sprintf("\\multicolumn{5}{l}{$D=0,\\, Y=%d$}\\\\", y))

  # One row per distance band
  for (b in bands) {
    r <- ks %>% filter(S_o == b, Y == y)
    if (nrow(r) == 0) {
      lines <- c(lines, sprintf("\\quad %s & -- & -- & -- & -- \\\\", band_labels[b]))
    } else {
      lines <- c(lines, sprintf(
        "\\quad %s & $%d$ & $%d$ & $%.3f$ & %s \\\\",
        band_labels[b], r$n_exp, r$n_obs, r$ks_stat, fmt_pval(r$ks_pval)
      ))
    }
  }

  # Space between groups (not after last)
  if (i < length(y_classes))
    lines <- c(lines, "\\addlinespace[0.6em]")
}

lines <- c(
  lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\small",
  paste0(
    "\\item \\textit{Notes:} Each row reports the Kolmogorov--Smirnov (KS) statistic ",
    "and corresponding $p$-value comparing the distribution of $R$ between the ",
    "experimental ($S=e$) and observational ($S=o$) units within the untreated sample ($D=0$). ",
    "The sample sizes of $R \\mid S=e, D, Y$ and $R \\mid S=o, D, Y$ are denoted by ",
    "$n_1$ and $n_2$, respectively."
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

out_path <- file.path(tables_dir, "tab_stability.tex")
writeLines(latex_table, out_path)
cat("Saved:", out_path, "\n")

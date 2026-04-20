# =============================================================================
# Smartcard — Summarize Representation Weights
#
# Extracts the per-observation efficient (H(R)) and simple (P(Y=1|R,S=o))
# representation weights from the fitted RSV objects produced by 01_empirical.R,
# and writes a single CSV consumed by 05_figures/06_representations.R.
#
# Input:
#   data/interim/smartcards/empirical/{full,nospillover}/{Y_var}.Rds
#
# Output:
#   data/clean/smartcards/empirical_representations.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
})

Y_vars <- c("Ycons", "Ylowinc", "Ymidinc")

# -----------------------------------------------------------------------------
# 1. Extract Weights for Both Samples
# -----------------------------------------------------------------------------

results <- lapply(c("full", "nospillover"), function(sample) {
  lapply(Y_vars, function(Y_var) {
    fit <- readRDS(
      file.path("data/interim/smartcards/empirical", sample, paste0(Y_var, ".Rds"))
    )[["fit"]]

    data.frame(
      sample    = sample,
      Y_var     = Y_var,
      efficient = unname(unlist(fit$weights)),
      simple    = unname(unlist(fit$weights_naive))
    )
  }) %>% bind_rows()
}) %>%
  bind_rows()

# -----------------------------------------------------------------------------
# 2. Save
# -----------------------------------------------------------------------------

out_path <- "data/clean/smartcards/empirical_representations.csv"
write.csv(results, out_path, row.names = FALSE)
cat("Saved:", out_path, "\n")
cat(sprintf("  %d rows, %d columns\n", nrow(results), ncol(results)))

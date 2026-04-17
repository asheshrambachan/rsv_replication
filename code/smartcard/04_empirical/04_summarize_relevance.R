# =============================================================================
# Smartcard — Summarize Relevance Results
#
# Extracts relevance estimates from the fitted RSV objects produced by
# 01_empirical.R for both samples, and writes a CSV consumed by
# 06_tables/04_tab_relevance.R.
#
# Input:
#   data/interim/smartcard/empirical/{full,nospillover}/{Y_var}.Rds
#
# Output:
#   data/clean/smartcard/empirical_relevance.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
})
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

Y_vars <- c("Ycons", "Ylowinc", "Ymidinc")
level  <- 0.90   # 90% CIs to match the paper's reporting convention

# -----------------------------------------------------------------------------
# 1. Load and Summarize Relevance
# -----------------------------------------------------------------------------

relevance_results <- lapply(c("full", "nospillover"), function(sample) {
  lapply(Y_vars, function(Y_var) {
    fit <- readRDS(
      file.path("data/interim/smartcard/empirical", sample, paste0(Y_var, ".Rds"))
    )[["fit"]]
    relevance(fit, level = level) %>%
      mutate(sample = sample, Y_var = Y_var)
  }) %>% bind_rows()
}) %>%
  bind_rows() %>%
  select(sample, Y_var, estimator, estimate, se, ci_lower, ci_upper)

out_path <- "data/clean/smartcard/empirical_relevance.csv"
write.csv(relevance_results, out_path, row.names = FALSE)
cat("Saved:", out_path, "\n")

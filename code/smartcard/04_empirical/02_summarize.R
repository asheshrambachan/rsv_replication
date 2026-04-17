# =============================================================================
# Smartcard — Summarize Empirical Results
#
# Extracts the flat result data frames from the per-outcome .Rds files
# produced by 01_empirical.R for both samples, appends the J-test statistic
# for each outcome, and writes a single CSV consumed by 05_figures/05_te.R,
# 06_tables/05_tab_rsv.R, and 06_tables/06_tab_estimators_jtest.R.
#
# Input:
#   data/interim/smartcard/empirical/{full,nospillover}/{Y_var}.Rds
#
# Output:
#   data/clean/smartcard/empirical_results.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
})
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

Y_vars <- c("Ycons", "Ylowinc", "Ymidinc")

# -----------------------------------------------------------------------------
# 1. Load Results and J-test for Both Samples
# -----------------------------------------------------------------------------

results <- lapply(c("full", "nospillover"), function(sample) {
  all_files <- list.files(
    file.path("data/interim/smartcard/empirical", sample),
    pattern    = "\\.Rds$",
    full.names = TRUE
  )

  res <- bind_rows(lapply(all_files, function(f) readRDS(f)[["result"]])) %>%
    mutate(sample = sample)

  jt <- bind_rows(lapply(Y_vars, function(Y_var) {
    fit <- readRDS(
      file.path("data/interim/smartcard/empirical", sample, paste0(Y_var, ".Rds"))
    )[["fit"]]
    jtest_out <- jtest(fit)
    data.frame(Y_var = Y_var, J = jtest_out$J, p_value = jtest_out$p_value)
  }))

  left_join(res, jt, by = "Y_var")
}) %>%
  bind_rows()

# -----------------------------------------------------------------------------
# 2. Save
# -----------------------------------------------------------------------------

out_path <- "data/clean/smartcard/empirical_results.csv"
write.csv(results, out_path, row.names = FALSE)
cat("Saved:", out_path, "\n")
cat(sprintf("  %d rows, %d columns\n", nrow(results), ncol(results)))

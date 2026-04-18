# =============================================================================
# Crop Burning — Summarize RSV Relevance
#
# Computes the relevance of the RS predictor for both the observational and
# validation samples and writes a flat CSV consumed by
# 03_tables/04_tab_relevance.R.
#
# Input:
#   data/interim/cropburn/fit_rsv_obs.Rds
#   data/interim/cropburn/fit_rsv_val.Rds
#
# Output:
#   data/clean/cropburn/empirical_relevance.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages(library(dplyr))
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

level <- 0.90

# -----------------------------------------------------------------------------
# 1. Compute relevance for both samples
# -----------------------------------------------------------------------------

results <- bind_rows(lapply(c("obs", "val"), function(sample) {
  fit <- readRDS(sprintf("data/interim/cropburn/fit_rsv_%s.Rds", sample))
  rel <- relevance(fit, level = level)
  rel %>%
    filter(estimator == "rsv") %>%
    mutate(sample = sample) %>%
    select(sample, estimate, se, ci_lower, ci_upper)
}))

# -----------------------------------------------------------------------------
# 2. Save
# -----------------------------------------------------------------------------

out_path <- "data/clean/cropburn/empirical_relevance.csv"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write.csv(results, out_path, row.names = FALSE)
cat("Saved:", out_path, "\n")
cat(sprintf("  %d rows, %d columns\n", nrow(results), ncol(results)))

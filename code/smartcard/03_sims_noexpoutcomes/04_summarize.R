# =============================================================================
# Smartcard — Summarize No-Exp-Outcomes Simulation Results
#
# Loads the raw per-replicate .Rds files produced by 02_calibrate.R and
# 03_rsv.R, computes summary statistics (bias, coverage, RMSE) across
# replicates for each parameter configuration, and writes a single flat CSV
# consumed by 05_figures/04_sims_noexpoutcomes.R.
#
# Input:
#   data/interim/smartcard/binary_noexpoutcomes/{calibrate,rsv}/*.Rds
#
# Output:
#   data/clean/smartcard/sims_noexpoutcomes.csv
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})
source("code/utils/summarize_sim_results.R")

cat("=== Summarize Simulation Results — No Experimental Outcomes ===\n\n")

alpha   <- 0.05
out_dir <- "data/interim/smartcard/binary_noexpoutcomes"
by_cols <- c("outcome", "tau", "n_e", "n_o")

# -----------------------------------------------------------------------------
# 1. Load, Summarize, and Label Each Estimator
# -----------------------------------------------------------------------------

calibrate <- list.files(file.path(out_dir, "calibrate"), pattern = "\\.Rds$", full.names = TRUE) %>%
  lapply(readRDS) %>%
  rbindlist(fill = TRUE) %>%
  summarize_sim_results(by_cols = by_cols, alpha = alpha) %>%
  as.data.frame() %>%
  mutate(estimator = "Common practice")

rsv <- list.files(file.path(out_dir, "rsv"), pattern = "\\.Rds$", full.names = TRUE) %>%
  lapply(readRDS) %>%
  rbindlist(fill = TRUE) %>%
  summarize_sim_results(by_cols = by_cols, alpha = alpha) %>%
  as.data.frame() %>%
  mutate(estimator = "RSV")

# -----------------------------------------------------------------------------
# 2. Combine and Save
# -----------------------------------------------------------------------------

all_results <- bind_rows(calibrate, rsv)

out_path <- "data/clean/smartcard/sims_noexpoutcomes.csv"
write.csv(all_results, out_path, row.names = FALSE)
cat("Saved:", out_path, "\n")
cat(sprintf("  %d rows, %d columns\n", nrow(all_results), ncol(all_results)))

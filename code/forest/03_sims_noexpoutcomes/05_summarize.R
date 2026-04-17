# =============================================================================
# Forest — Summarize No-Exp-Outcomes Simulation Results
#
# Loads the raw per-replicate .Rds files produced by 02_plugin.R, 03_calibrate.R,
# and 04_rsv.R, computes summary statistics (bias, coverage, RMSE) across
# replicates for each parameter configuration, and writes a single flat CSV
# consumed by 05_figures/03_sims_noexpoutcomes.R.
#
# Input:
#   data/interim/forest/binary_noexpoutcomes/{plugin,calibrate,rsv}/*.Rds
#
# Output:
#   data/clean/forest/sims_noexpoutcomes.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})
source("code/utils/summarize_sim_results.R")

alpha   <- 0.05
out_dir <- file.path("data/interim/forest", "binary_noexpoutcomes")
by_cols <- c("outcome", "tau", "n_e", "n_o", "alpha_o_id")

# -----------------------------------------------------------------------------
# 1. Load, Summarize, and Label Each Estimator
# -----------------------------------------------------------------------------

plugin <- list.files(file.path(out_dir, "plugin"), full.names = TRUE) %>%
  lapply(readRDS) %>%
  rbindlist(fill = TRUE) %>%
  summarize_sim_results(by_cols = by_cols, alpha = alpha) %>%
  as.data.frame() %>%
  mutate(estimator = "PTE")

calibrate <- list.files(file.path(out_dir, "calibrate"), full.names = TRUE) %>%
  lapply(readRDS) %>%
  rbindlist(fill = TRUE) %>%
  summarize_sim_results(by_cols = by_cols, alpha = alpha) %>%
  as.data.frame() %>%
  mutate(estimator = "Common practice")

rsv <- list.files(file.path(out_dir, "rsv"), full.names = TRUE) %>%
  lapply(readRDS) %>%
  rbindlist(fill = TRUE) %>%
  summarize_sim_results(by_cols = by_cols, alpha = alpha) %>%
  as.data.frame() %>%
  mutate(estimator = "RSV")

# -----------------------------------------------------------------------------
# 2. Combine and Save
# -----------------------------------------------------------------------------

all_results <- bind_rows(plugin, calibrate, rsv)

out_path <- "data/clean/forest/sims_noexpoutcomes.csv"
write.csv(all_results, out_path, row.names = FALSE)
cat("Saved:", out_path, "\n")
cat(sprintf("  %d rows, %d columns\n", nrow(all_results), ncol(all_results)))

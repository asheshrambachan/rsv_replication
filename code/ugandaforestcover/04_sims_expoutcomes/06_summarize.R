# =============================================================================
# Forest — Summarize Experimental-Outcomes Simulation Results
#
# Loads the raw per-replicate .Rds files produced by 02_benchmark.R,
# 03_ppio.R, 04_ppiv.R, and 05_rsv.R, computes summary statistics (bias,
# coverage, RMSE) across replicates for each parameter configuration, and
# writes a single flat CSV consumed by 05_figures/04_sims_expoutcomes.R.
#
# Input:
#   data/interim/ugandaforestcover/sims_expoutcomes_{outcome}/{benchmark,ppio,ppiv,rsv}/*.Rds
#
# Output:
#   data/clean/ugandaforestcover/sims_expoutcomes.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})
source("code/utils/summarize_sim_results.R")

outcome <- "Ybin"
alpha   <- 0.05
out_dir <- file.path("data/interim/ugandaforestcover", paste0("sims_expoutcomes_", outcome))
by_cols <- c("outcome", "tau", "n_e", "n_o", "n_eo", "alpha_o_id")

# -----------------------------------------------------------------------------
# 1. Load, Summarize, and Label Each Estimator
# -----------------------------------------------------------------------------

benchmark <- list.files(file.path(out_dir, "benchmark"), full.names = TRUE) %>%
  lapply(readRDS) %>%
  rbindlist(fill = TRUE) %>%
  summarize_sim_results(by_cols = by_cols, alpha = alpha) %>%
  as.data.frame() %>%
  mutate(estimator = "Benchmark")

ppio <- list.files(file.path(out_dir, "ppio"), full.names = TRUE) %>%
  lapply(readRDS) %>%
  rbindlist(fill = TRUE) %>%
  summarize_sim_results(by_cols = by_cols, alpha = alpha) %>%
  as.data.frame() %>%
  mutate(estimator = "PPIO")

ppiv <- list.files(file.path(out_dir, "ppiv"), full.names = TRUE) %>%
  lapply(readRDS) %>%
  rbindlist(fill = TRUE) %>%
  summarize_sim_results(by_cols = by_cols, alpha = alpha) %>%
  as.data.frame() %>%
  mutate(estimator = "PPIV")

rsv <- list.files(file.path(out_dir, "rsv"), full.names = TRUE) %>%
  lapply(readRDS) %>%
  rbindlist(fill = TRUE) %>%
  summarize_sim_results(by_cols = by_cols, alpha = alpha) %>%
  as.data.frame() %>%
  mutate(estimator = "RSV")

# -----------------------------------------------------------------------------
# 2. Combine and Save
# -----------------------------------------------------------------------------

all_results <- bind_rows(benchmark, ppio, ppiv, rsv)

out_path <- "data/clean/ugandaforestcover/sims_expoutcomes.csv"
write.csv(all_results, out_path, row.names = FALSE)
cat("Saved:", out_path, "\n")
cat(sprintf("  %d rows, %d columns\n", nrow(all_results), ncol(all_results)))

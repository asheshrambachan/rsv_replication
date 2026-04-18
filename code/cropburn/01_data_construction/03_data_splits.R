# =============================================================================
# Crop Burning — Define Analytical Samples
#
# Produces two analysis-ready datasets from the cleaned plot-level data:
#
#   obs_data.csv     — Observational study sample
#     Plots are assigned to either the experimental sample (has D, no Y) or
#     the observational sample (has Y, no D). D and Y are mutually exclusive
#     by construction: spot-check plots contribute outcomes only; all other
#     plots contribute treatment variation only.
#
#   val_data.csv     — Validation study sample
#     Spot-check plots have both D and Y observed, enabling the RSV
#     validation exercise. Non-spot-check plots have D only (experimental).
#
# Both datasets apply the same base filter: plots must have all three
# remote sensing predictors (R_max, R_bal, R_prob) non-missing.
#
# Input:
#   data/clean/cropburn/data.csv
#
# Outputs:
#   data/clean/cropburn/data_obs.csv
#   data/clean/cropburn/data_val.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})


# -----------------------------------------------------------------------------
# 1. Load cleaned plot-level data
# -----------------------------------------------------------------------------

plots <- read_csv(
  "data/clean/cropburn/data.csv",
  show_col_types = FALSE
)


# -----------------------------------------------------------------------------
# 2. Observational study sample
#
# In this sample, the experimental and observational subsamples are disjoint:
#   - Spot-check plots (is_sc == 1): Y is observed, D is set to NA.
#     These contribute to the observational sample (S = "o").
#   - All other plots: D is observed, Y is set to NA.
#     These contribute to the experimental sample (S = "e").
#
# S = "e,o" (plots with both D and Y) cannot arise by construction here.
# Plots with S = NA are dropped (they have neither D nor Y).
# -----------------------------------------------------------------------------

obs_data <- plots %>%
  mutate(
    Y = if_else(is_sc == 1,        Y, NA_real_),   # Y only for spot-check plots
    D = if_else(is_sc == 1, NA_real_,        D),   # D only for non-spot-check plots
    S = case_when(
      !is.na(D) & !is.na(Y) ~ "e,o",
      !is.na(D)              ~ "e",
      !is.na(Y)              ~ "o"
    )
  ) %>%
  filter(!is.na(S))

write_csv(obs_data, "data/clean/cropburn/data_obs.csv")

cat("\nObservational sample — data/clean/cropburn/data_obs.csv\n")
cat(sprintf("  Total plots: %d\n", nrow(obs_data)))
obs_data %>%
  count(S) %>%
  mutate(pct = round(100 * n / sum(n), 1),
         line = sprintf("  S = %-4s  %5d plots (%s%%)\n", S, n, pct)) %>%
  pull(line) %>%
  cat(sep = "")


# -----------------------------------------------------------------------------
# 3. Validation study sample
#
# Here D is retained for all plots, including spot-check plots. Spot-check
# plots therefore have both D and Y observed (S = "e,o"), which is the
# configuration required to run RSV validation: we can compare the RSV
# estimate (using only D and R from the experimental sample) against the
# direct ATE estimate from plots where both D and Y are available.
# -----------------------------------------------------------------------------

val_data <- plots %>%
  mutate(
    Y = if_else(is_sc == 1, Y, NA_real_),   # Y only for spot-check plots
    S = case_when(
      !is.na(D) & !is.na(Y) ~ "e,o",
      !is.na(D)              ~ "e",
      !is.na(Y)              ~ "o"
    )
  ) %>%
  filter(!is.na(S))

write_csv(val_data, "data/clean/cropburn/data_val.csv")

cat("\nValidation sample — data/clean/cropburn/data_val.csv\n")
cat(sprintf("  Total plots: %d\n", nrow(val_data)))
val_data %>%
  count(S) %>%
  mutate(pct = round(100 * n / sum(n), 1),
         line = sprintf("  S = %-4s  %5d plots (%s%%)\n", S, n, pct)) %>%
  pull(line) %>%
  cat(sep = "")

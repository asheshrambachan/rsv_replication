# =============================================================================
# Smartcard — Merge, Spillover Indicators, and PCA
#
# Merges the three interim datasets into a single village-level analysis file,
# then computes two additional columns:
#
#   spillover_20km — TRUE for villages within 20 km of a village with the
#     opposite treatment status (potential spillover contamination). Computed
#     via minimum vertex cover on the bipartite conflict graph (see
#     code/utils/spillover.R).
#
#   R_PC1 / R_PC1_scaled — first principal component of the combined
#     luminosity and satellite feature matrix, used as the remote sensing
#     predictor R. Constant columns are dropped before PCA to avoid
#     zero-variance scaling issues.
#
# Inputs:
#   data/interim/smartcard/base_data.Rds
#   data/interim/smartcard/viirs.Rds
#   data/interim/smartcard/satellite_features.Rds
#   data/interim/smartcard/centroid_coords.Rds
#
# Output: data/clean/smartcard/smartcard_data.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(haven)
  library(sf)
  library(igraph)
})
source("code/utils/spillover.R")

# -----------------------------------------------------------------------------
# 1. Load Interim Data
# -----------------------------------------------------------------------------

base_data          <- readRDS("data/interim/smartcard/base_data.Rds")
viirs_annual       <- readRDS("data/interim/smartcard/viirs.Rds")
satellite_features <- readRDS("data/interim/smartcard/satellite_features.Rds")
centroid_coords    <- readRDS("data/interim/smartcard/centroid_coords.Rds")

# -----------------------------------------------------------------------------
# 2. Merge and Compute Spillover Indicators
#
# centroid_coords contains only villages that passed the 1 km MOSAIKS filter,
# so the inner_join also restricts the sample to those villages. Centroid
# columns are dropped after the spillover computation via the final select.
# -----------------------------------------------------------------------------

smartcard_data <- base_data %>%
  inner_join(centroid_coords, by = "shrid2") %>%
  mutate(spillover_20km = spillover_affected_bkm(D, centroid_lat, centroid_lon,
                                                  b_km = 20)) %>%
  inner_join(viirs_annual,       by = "shrid2") %>%
  inner_join(satellite_features, by = "shrid2") %>%
  select(shrid2, spillover_20km, tot_p, tot_f,
         wave, D, Ycons, Ylowinc, Ymidinc, clusters,
         starts_with("luminosity_"), starts_with("satellite_"))

# Strip residual Stata attributes introduced by haven
smartcard_data <- zap_labels(smartcard_data)
smartcard_data <- zap_formats(smartcard_data)
for (i in seq_along(smartcard_data)) {
  attr(smartcard_data[[i]], "label")        <- NULL
  attr(smartcard_data[[i]], "format.stata") <- NULL
}

cat(sprintf("Merged dataset: %d villages\n", nrow(smartcard_data)))

# -----------------------------------------------------------------------------
# 3. Compute PCA Remote Sensing Predictor
#
# PC1 of the combined luminosity + satellite feature matrix serves as the
# remote sensing predictor R. Constant columns are dropped first (they
# contribute nothing to variance and break scale. = TRUE in prcomp).
# R_PC1_scaled is standardised to mean 0, SD 1 for comparability across runs.
# -----------------------------------------------------------------------------

features_mat <- smartcard_data %>%
  select(starts_with("luminosity_"), starts_with("satellite_")) %>%
  select(where(~ n_distinct(.) > 1))   # drop constant columns

cat(sprintf("Running PCA on %d rows x %d features\n",
            nrow(features_mat), ncol(features_mat)))

pca_result <- prcomp(features_mat, center = TRUE, scale. = TRUE, rank. = 1)
R_PC1      <- pca_result$x[, "PC1"]

smartcard_data <- smartcard_data %>%
  mutate(
    R_PC1        = as.numeric(R_PC1),
    R_PC1_scaled = as.numeric(scale(R_PC1))
  )

# -----------------------------------------------------------------------------
# 4. Save
# -----------------------------------------------------------------------------

out_path <- "data/clean/smartcard/smartcard_data.csv"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_csv(smartcard_data, out_path)
cat("Saved to", out_path, "\n")

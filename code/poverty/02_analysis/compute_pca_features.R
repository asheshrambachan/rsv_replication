# =============================================================================
# Title: Compute PCA Representation of Remote Sensing Features
# Purpose: Applies PCA to remote sensing + VIIRS features, generates PC1 scores
# Output: data/poverty/processed/pca.csv
# =============================================================================

rm(list = ls())

## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
options(readr.show_col_types = F)


# -----------------------------------------------------------------------------
# 1. Select Feature Columns (remove duplicates & constant columns)
# -----------------------------------------------------------------------------
## Load data
data <- read_csv("data/poverty/processed/poverty_data.csv")

features <- data %>%
  select(
    shrid2,
    starts_with("viirs_annual_"),
    starts_with("feature_")
  ) %>%
  distinct()  # Drop duplicate SHRIDs (e.g., urban/rural repeats)

feature_ids <- features$shrid2

features_mat <- features %>%
  select(-shrid2) %>%
  select(where(~ n_distinct(.) > 1))  # Drop constant columns

cat(sprintf("Performing PCA on %d rows * %d columns\n", nrow(features_mat), ncol(features_mat)))

# -----------------------------------------------------------------------------
# 2. Run PCA and Extract PC1
# -----------------------------------------------------------------------------
pca_results <- prcomp(features_mat, center = TRUE, scale. = TRUE)
R_PC1 <- -pca_results$x[, "PC1"]  # Flip sign to match original study

## Combine PCA Output with Metadata
pca_scores <- data.frame(
  shrid2 = feature_ids,
  R_PC1 = R_PC1,
  R_PC1_scaled = scale(R_PC1)
)

pca_full <- data %>%
  select(shrid2, Ycons, Ylowinc, Ymidinc, wave, D) %>%
  left_join(pca_scores, by = "shrid2")


# -----------------------------------------------------------------------------
# 3. Export Results
# -----------------------------------------------------------------------------
output_path <- "data/poverty/processed/pca_data.csv"
write_csv(pca_full, output_path)
cat(sprintf("Saved PCA results to: %s\n", output_path))
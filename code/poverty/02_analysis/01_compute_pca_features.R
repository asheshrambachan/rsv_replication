rm(list = ls())

# === Load libraries ===
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
options(readr.show_col_types = F)

# === Load Data ===
data <- read_csv("data/processed/poverty/data.csv")

# === Perform PCA on feature columns ===
pca_results <- data %>%
  select(
    starts_with("viirs_annual_"),
    starts_with("feature_")
    ) %>%
  select(where(~ n_distinct(.) > 1)) %>% # Remove constant columns
  prcomp(., center = T, scale. = T)

# === Combine PCA output with key columns ===
data_with_pca <- data %>% 
  select(shrid2, lat, lon, Ycons, Ylow, Ymid, wave, D) %>%
  mutate(
    R_PC1 = pca_results$x[, "PC1"],
    R_PC1_scaled = scale(pca_results$x[, "PC1"])
  )

# === Save Output ===
path <- "data/processed/poverty/pca.csv"
write.csv(data_with_pca, path, row.names = F)
cat(sprintf("Saved PCA results to: %s\n", path))
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
features <- data %>%
  select(
    "shrid2",
    starts_with("viirs_annual_"),
    starts_with("feature_")
  ) %>%
  distinct() # to keep only 1 copy of the shrids that appear as urban and rural

print(nrow(features))

features_id <- features$shrid2

features <- features %>% 
  select(-shrid2) %>%
  select(where(~ n_distinct(.) > 1)) # Remove constant columns

pca_results <- prcomp(features, center = T, scale. = T)
R_PC1 <- pca_results$x[, "PC1"]

# === Combine PCA output with shrid2 id and other metadata columns ===
data_pca <- data.frame(
  shrid2 = features_id,
  R_PC1 = R_PC1,
  R_PC1_scaled = scale(R_PC1)
  )

data_pca_metadata <- data %>% 
  select(shrid2, lat, lon, Ycons, Ylowinc, Ymidinc, wave, D) %>%
  left_join(data_pca, by = "shrid2")

# === Save Output ===
path <- "data/processed/poverty/pca.csv"
write.csv(data_pca_metadata, path, row.names = F)
cat(sprintf("Saved PCA results to: %s\n", path))
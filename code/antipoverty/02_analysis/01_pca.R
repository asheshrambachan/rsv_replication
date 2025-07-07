library(dplyr)

rm(list=ls())

data_wo_features <- read.csv("data/clean/antipoverty/data_wo_features.csv") %>%
  filter(
    tot_p >= 100, # Identify and remove small shrid based on population size i.e filter out all shrids with pop size of 100 or lower
    !(is.na(y_05k) | is.na(y_10k) | is.na(y_cons)) # Remove rows with missing values in target variable
  ) %>% 
  select(shrid2, lat, lon, y_cons, y_05k, y_10k, wave, D)
features_parts <- list.files("data/clean/antipoverty/features", full.names = T, pattern = ".csv") 
features <- do.call(rbind, lapply(features_parts, read.csv)) 

data <- data_wo_features %>%
  left_join(features, by=c("lat", "lon"))

# Perform PCA
pca_results <- data %>%
  select(starts_with("feature_")) %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  select(where(~ n_distinct(.) > 1)) %>% # remove constant features across all obs
  prcomp(., center = TRUE, scale. = TRUE)

data_wo_features$R_PC1 <- pca_results$x[, "PC1"]
data_wo_features$R_PC1_scaled <- scale(pca_results$x[, "PC1"])

write.csv(data_wo_features, "data/clean/antipoverty/pca.csv", row.names = F)

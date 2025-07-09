
rm(list=ls())

library(dplyr)
library(readr)

data <- read_csv("data/clean/antipoverty/data.csv", show_col_types = F) # %>%
  # filter(
  #   tot_p >= 100, # Identify and remove small shrid based on population size i.e filter out all shrids with pop size of 100 or lower
  #   !(is.na(Y05k) | is.na(Y10k) | is.na(Ycons)) # Remove rows with missing values in target variable
  # )

# Perform PCA
pca_results <- data %>%
  select(starts_with("feature_")) %>%
  select(where(~ n_distinct(.) > 1)) %>% # remove 34 cols with constant values
  prcomp(., center = TRUE, scale. = TRUE)

data_w_pca <- data %>% 
  select(shrid2, lat, lon, Ycons, Y05k, Y10k, wave, D) %>%
  mutate(
    R_PC1 = pca_results$x[, "PC1"],
    R_PC1_scaled = scale(pca_results$x[, "PC1"])
  )

write.csv(data_w_pca, "data/clean/antipoverty/pca.csv", row.names = F)

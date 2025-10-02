# =============================================================================
# Title: Merge Remote Sensing and Satellite Features
# Purpose: Combines VIIRS nighttime lights, Mosaik 4000 features, and cleaned SHRUG data
# Output: data/poverty/processed/poverty_data.csv
# =============================================================================

rm(list = ls())

## Load libraries
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
})
source("code/poverty/utils/spillover_affected_bkm.R")
options(readr.show_col_types = F)

# -----------------------------------------------------------------------------
# 1. Load VIIRS Nighttime Lights Data
# -----------------------------------------------------------------------------

viirs_annual <- read_dta("https://dataverse.harvard.edu/api/access/datafile/10742856") %>%
  filter(
    year %in% 2012:2021,
    category == "median-masked"
  ) %>%
  select(-category) %>%
  pivot_wider(
    id_cols = shrid2,
    names_from = year,
    values_from = c(
      viirs_annual_min,
      viirs_annual_max,
      viirs_annual_mean,
      viirs_annual_sum,
      viirs_annual_num_cells
    ),
    names_sep = "."
  )


# -----------------------------------------------------------------------------
# 2. Load Mosaik Remote Sensing Features
# -----------------------------------------------------------------------------

features <- list.files(
    "data/poverty/processed/features",
    pattern = "features_chunk\\d{2}\\.csv$",
    full.names = T
  ) %>%
  lapply(read_csv) %>%
  bind_rows() 


# -----------------------------------------------------------------------------
# 3. Merge Base Dataset with Remote Sensing Data
# -----------------------------------------------------------------------------

data <- read_csv("data/poverty/processed/base_data.csv") %>%
  left_join(viirs_annual, by = "shrid2") %>%
  inner_join(features, by = "shrid2") %>%
  filter(distance_km <= 1)

data <- data %>%
  mutate(spillover_20km = spillover_affected_bkm(D, centroid_lat, centroid_lon, b_km = 20))

# -----------------------------------------------------------------------------
# 5. Add Spatial Coordinates and Spillover Indicator
# -----------------------------------------------------------------------------

## 5.1 Load shapefile and compute centroids for our villages
shrids <- st_read("data/poverty/processed/shapefiles/shrids.gpkg", quiet = TRUE) %>%
  filter(shrid2 %in% data$shrid2)

centroids <- st_centroid(shrids)
coords <- st_coordinates(centroids)

centroid_coords <- shrids %>%
  mutate(
    centroid_lon = coords[, 1], 
    centroid_lat = coords[, 2]
  ) %>%
  as.data.frame() %>%
  select(shrid2, centroid_lat, centroid_lon)

## 5.2 Add coordinates to data
data <- data %>%
  left_join(centroid_coords, by = "shrid2")

## 5.3 Create spillover indicator using polygon-based approach (more accurate than centroids)
source("code/poverty/utils/spillover_polygon_based.R")

# Use hybrid approach: centroids for initial filtering, then accurate polygon distances
spillover_20km <- compute_spillover_hybrid(shrids, data$D, buffer_km = 20)

# Add spillover indicator to data
data <- data %>%
  mutate(spillover_20km = spillover_20km) 

# -----------------------------------------------------------------------------
# 4. Export Final Merged Dataset
# -----------------------------------------------------------------------------

output_path <- "data/poverty/processed/poverty_data.csv"
write_csv(data, output_path)
cat(sprintf("Saved merged dataset to: %s\n", output_path))

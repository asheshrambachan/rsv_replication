# =============================================================================
# Title: Merge Remote Sensing and Satellite Features
# Purpose: Combines VIIRS nighttime lights, Mosaik 4000 features, and cleaned SHRUG data
# Output: data/poverty/processed/poverty_data.csv
# =============================================================================

rm(list = ls())

## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
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
  bind_rows() # identical to Sammi's features except for 29 shrids


# -----------------------------------------------------------------------------
# 3. Merge Base Dataset with Remote Sensing Data
# -----------------------------------------------------------------------------

data <- read_csv("data/poverty/processed/base_data.csv") %>%
  left_join(viirs_annual, by = "shrid2") %>%
  inner_join(features, by = "shrid2") %>%
  filter(distance_km <= 1)


# -----------------------------------------------------------------------------
# 4. Export Final Merged Dataset
# -----------------------------------------------------------------------------

output_path <- "data/poverty/processed/poverty_data.csv"
write_csv(data, output_path)
cat(sprintf("Saved merged dataset to: %s\n", output_path))

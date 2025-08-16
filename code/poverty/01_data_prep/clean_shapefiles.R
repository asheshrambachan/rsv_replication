# =============================================================================
# Title: Clean and Export Spatial Shapefiles (State, Districts, SHRIDs)
# Purpose: Prepares and saves cleaned `.gpkg` shapefiles for Andhra Pradesh
# Input: Raw shapefiles from SHRUG v2.0 and v2.1
# Output: data/poverty/processed/shapefiles/*.gpkg
# =============================================================================

rm(list = ls())

## Load Libraries
suppressPackageStartupMessages({
  library(readr)
  library(sf)
})
options(readr.show_col_types = FALSE)

## Define Helper Function
save_shapefile <- function(sf_object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  st_write(sf_object, path, append = FALSE, quiet = TRUE)
  cat(sprintf("Saved cleaned shapefile to: %s\n", path))
}


# -----------------------------------------------------------------------------
# 1. Clean and Save State Shapefile
# -----------------------------------------------------------------------------
state <- st_read("data/poverty/raw/shrug-pc11state-poly-shp/state.shp", quiet = TRUE) %>%
  filter(s_name == "Andhra Pradesh")

save_shapefile(state, "data/poverty/processed/shapefiles/state.gpkg")


# -----------------------------------------------------------------------------
# 2. Clean and Save District Shapefile
# -----------------------------------------------------------------------------
district_recode <- c(
  "Sri Potti Sriramulu Nellore" = "Nellore",
  "Y.S.R." = "Kadapa"
)

districts <- st_read("data/poverty/raw/shrug-pc11dist-poly-shp/district.shp", quiet = TRUE) %>%
  filter(pc11_s_id %in% state$pc11_s_id) %>%
  mutate(d_name = recode(d_name, !!!district_recode))

save_shapefile(districts, "data/poverty/processed/shapefiles/districts.gpkg")


# -----------------------------------------------------------------------------
# 3. Clean and Save SHRID2 Shapefile
# -----------------------------------------------------------------------------
# Load shrid2 ids for filtering
shrid_ids <- read_csv("data/poverty/processed/base_data.csv", col_select = shrid2) %>% 
  pull(shrid2) %>%
  unique()

shrids <- st_read("data/poverty/raw/shrug-shrid-poly-gpkg/shrid2_open.gpkg", quiet = TRUE) %>%
  filter(shrid2 %in% shrid_ids)

save_shapefile(shrids, "data/poverty/processed/shapefiles/shrids.gpkg")

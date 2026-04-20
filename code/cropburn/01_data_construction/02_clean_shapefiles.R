# =============================================================================
# Crop Burning — Clean Shapefiles
#
# Clips the SHRUG district and village shapefiles to the study region
# (Bathinda and Faridkot districts, Punjab). The full SHRUG files cover
# all of India; subsetting here keeps file sizes manageable and speeds up
# spatial operations in the mapping scripts.
#
# Inputs:
#   data/raw/cropburn/shrug-pc11dist-poly-shp/district.shp
#   data/raw/cropburn/shrug-pc11-village-poly-shp/village_modified.shp
#
# Outputs:
#   data/clean/cropburn/shapefiles/districts.gpkg
#   data/clean/cropburn/shapefiles/villages.gpkg
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(sf)   # spatial data
})

dir.create("data/clean/cropburn/shapefiles", recursive = TRUE, showWarnings = FALSE)


# -----------------------------------------------------------------------------
# 1. District boundaries
#
# Filtered to the two study districts used by Jack et al. — Bathinda and
# Faridkot in Punjab. Used for map backgrounds and spatial merges.
# -----------------------------------------------------------------------------

districts <- st_read("data/raw/cropburn/shrug-pc11dist-poly-shp/district.shp", quiet = TRUE) %>%
  filter(d_name %in% c("Bathinda", "Faridkot"))

st_write(districts, "data/clean/cropburn/shapefiles/districts.gpkg", append = FALSE, quiet = TRUE)
cat("Saved district shapefile to: data/clean/cropburn/shapefiles/districts.gpkg\n")


# -----------------------------------------------------------------------------
# 2. Village boundaries
#
# Filtered to villages within the two study districts, identified by matching
# on pc11_d_id from the district layer above.
# -----------------------------------------------------------------------------

villages <- st_read("data/raw/cropburn/shrug-pc11-village-poly-shp/village_modified.shp", quiet = TRUE) %>%
  filter(pc11_d_id %in% districts$pc11_d_id) %>%
  select(-mdds_og)

st_write(villages, "data/clean/cropburn/shapefiles/villages.gpkg", append = FALSE, quiet = TRUE)
cat("Saved village shapefile to: data/clean/cropburn/shapefiles/villages.gpkg\n")

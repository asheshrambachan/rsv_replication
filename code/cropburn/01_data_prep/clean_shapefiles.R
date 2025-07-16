# Clear environment
rm(list = ls())

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)    # For data manipulation
  library(sf)       # For spatial data
})

# Clean and filter district shapefile (Bathinda & Faridkot only)
districts <- st_read("data/cropburn/raw/shrug-pc11dist-poly-shp/district.shp", quiet=T) %>%
  filter(d_name %in% c("Bathinda", "Faridkot"))

output_path <- "data/cropburn/processed/shapefiles/districts.gpkg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
st_write(districts, output_path, append=F, quiet=T)
cat(sprintf("Saved cleaned shapefiles to: %s\n", output_path))


# Clean and filter village shapefile (within selected districts)
villages <- st_read("data/cropburn/raw/shrug-pc11-village-poly-shp/village_modified.shp", quiet=T) %>%
  filter(pc11_d_id %in% districts$pc11_d_id) %>%
  select(-mdds_og)

output_path <- "data/cropburn/processed/shapefiles/villages.gpkg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
st_write(villages, output_path, append=F, quiet=T)
cat(sprintf("Saved cleaned shapefiles to: %s\n", output_path))


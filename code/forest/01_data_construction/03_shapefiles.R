# =============================================================================
# Forest — Clean Shapefiles
#
# Reprojects and saves the two raw shapefiles used for mapping: the PES imagery
# boundary (experimental sample region) and Uganda admin-level-2 boundaries.
# Saving clean copies here decouples 05_figures/01_map.R from the raw source
# paths and aligns with the smartcard and cropburn conventions.
#
# Inputs:
#   data/raw/forest/jayachandran_et_al_replication_files/CashForCarbon_full/
#     BaselineShapesWGS84_dissolved.shp
#   data/raw/forest/uga_admin_boundaries.shp/uga_admin2.shp
#
# Outputs:
#   data/clean/forest/shapefiles/imagery_boundary.gpkg
#   data/clean/forest/shapefiles/districts.gpkg
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(sf)
})

dir.create("data/clean/forest/shapefiles", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. PES Imagery Boundary
#
# The raw file is already in WGS84; reproject explicitly to ensure consistency.
# -----------------------------------------------------------------------------

imagery <- st_read(
  "data/raw/forest/jayachandran_et_al_replication_files/CashForCarbon_full/BaselineShapesWGS84_dissolved.shp",
  quiet = TRUE
) |> st_transform(4326)

st_write(imagery, "data/clean/forest/shapefiles/imagery_boundary.gpkg",
         layer = "imagery_boundary", append = FALSE, quiet = TRUE)
cat("Saved imagery_boundary.gpkg\n")

# -----------------------------------------------------------------------------
# 2. Uganda Admin-Level-2 Boundaries (Counties / Districts)
# -----------------------------------------------------------------------------

admin2 <- st_read(
  "data/raw/forest/uga_admin_boundaries.shp/uga_admin2.shp",
  quiet = TRUE
) |> st_transform(4326)

st_write(admin2, "data/clean/forest/shapefiles/districts.gpkg",
         layer = "districts", append = FALSE, quiet = TRUE)
cat(sprintf("Saved districts.gpkg: %d features\n", nrow(admin2)))

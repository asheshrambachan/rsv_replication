# =============================================================================
# Smartcard — Clean Shapefiles
#
# Clips the SHRUG state, district, and village polygon files to the final
# analysis sample. Shrids are filtered using the shrid2 IDs in the clean
# dataset, which reflects all upstream filtering (MOSAIKS 1km threshold,
# inner joins, etc.), so the map exactly matches the analysis sample.
#
# Inputs:
#   data/clean/smartcards/data.csv
#   data/raw/smartcards/shrug-pc11state-poly-shp/state.shp
#   data/raw/smartcards/shrug-pc11dist-poly-shp/district.shp
#   data/raw/smartcards/shrug-shrid-poly-gpkg/shrid2_open.gpkg
#
# Outputs:
#   data/clean/smartcards/shapefiles/state.gpkg
#   data/clean/smartcards/shapefiles/districts.gpkg
#   data/clean/smartcards/shapefiles/shrids.gpkg
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
})

dir.create("data/clean/smartcards/shapefiles", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. State Boundary
# -----------------------------------------------------------------------------

state <- st_read("data/raw/smartcards/shrug-pc11state-poly-shp/state.shp", quiet = TRUE) %>%
  filter(s_name == "Andhra Pradesh")

st_write(state, "data/clean/smartcards/shapefiles/state.gpkg",
         layer = "state", append = FALSE, quiet = TRUE)
cat("Saved state.gpkg\n")

# -----------------------------------------------------------------------------
# 2. District Boundaries
#
# Two districts were renamed in the PC11 census relative to common usage;
# recode to the names used throughout the rest of this project.
# -----------------------------------------------------------------------------

district_recode <- c(
  "Sri Potti Sriramulu Nellore" = "Nellore",
  "Y.S.R."                      = "Kadapa"
)

districts <- st_read("data/raw/smartcards/shrug-pc11dist-poly-shp/district.shp", quiet = TRUE) %>%
  filter(pc11_s_id %in% state$pc11_s_id) %>%
  mutate(d_name = recode(d_name, !!!district_recode))

st_write(districts, "data/clean/smartcards/shapefiles/districts.gpkg",
         layer = "districts", append = FALSE, quiet = TRUE)
cat(sprintf("Saved districts.gpkg: %d districts\n", nrow(districts)))

# -----------------------------------------------------------------------------
# 3. Village Boundaries (SHRIDs)
#
# Filtered to the villages in the final clean dataset, so the map matches
# the analysis sample exactly.
# -----------------------------------------------------------------------------

shrid_ids <- read_csv("data/clean/smartcards/data.csv",
                      col_select = shrid2) %>%
  pull(shrid2) %>%
  unique()

shrids <- st_read("data/raw/smartcards/shrug-shrid-poly-gpkg/shrid2_open.gpkg", quiet = TRUE) %>%
  filter(shrid2 %in% shrid_ids)

st_write(shrids, "data/clean/smartcards/shapefiles/shrids.gpkg",
         layer = "shrids", append = FALSE, quiet = TRUE)
cat(sprintf("Saved shrids.gpkg: %d villages\n", nrow(shrids)))

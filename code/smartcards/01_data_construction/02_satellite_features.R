# =============================================================================
# Smartcard — VIIRS Nighttime Lights and MOSAIKS Satellite Features
#
# Downloads two remote sensing datasets for the villages in base_data:
#   - VIIRS annual nighttime lights (2012–2021, median-masked)
#   - MOSAIKS 4000-feature satellite imagery vectors (2019 Planet imagery)
#
# MOSAIKS features are downloaded in chunks to stay within API limits. Each
# village centroid is matched to the nearest MOSAIKS grid point; villages with
# no grid point within 1 km are dropped.
#
# Input:  data/clean/smartcards/shapefiles/shrids.gpkg
# Output: data/interim/smartcards/viirs.Rds
#         data/interim/smartcards/centroid_coords.Rds
#         data/interim/smartcards/satellite_features.Rds
# =============================================================================

rm(list = ls())

# Install redivis if not already present (needed for MOSAIKS API)
tryCatch(
  find.package("redivis"),
  error = function(e) {
    message("Installing redivis...")
    devtools::install_github("redivis/redivis-r", ref = "main")
  }
)

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(tidyr)
  library(sf)
  library(geosphere)
})
options(readr.show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 1. VIIRS Annual Nighttime Lights (2012–2021)
#
# Wide format: one column per year x statistic combination. Using
# median-masked values only — other categories (e.g. cloud-masked) are
# excluded to keep the feature set consistent across years.
# -----------------------------------------------------------------------------

viirs_annual <- read_dta("https://dataverse.harvard.edu/api/access/datafile/10742856") %>%
  filter(year %in% 2012:2021, category == "median-masked") %>%
  rename_with(~ sub("viirs_annual", "luminosity", .x)) %>%
  select(-category) %>%
  pivot_wider(
    id_cols     = shrid2,
    names_from  = year,
    values_from = c(luminosity_min, luminosity_max, luminosity_mean,
                    luminosity_sum, luminosity_num_cells),
    names_sep   = "."
  ) %>%
  relocate(
    paste0("luminosity_min.",       2012:2021),
    paste0("luminosity_max.",       2012:2021),
    paste0("luminosity_mean.",      2012:2021),
    paste0("luminosity_sum.",       2012:2021),
    paste0("luminosity_num_cells.", 2012:2021),
    .after = shrid2
  )

cat(sprintf("VIIRS: %d villages x %d light columns\n",
            nrow(viirs_annual), ncol(viirs_annual) - 1L))

out_path <- "data/interim/smartcards/viirs.Rds"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(viirs_annual, out_path)
cat("Saved to", out_path, "\n")

# -----------------------------------------------------------------------------
# 2. Compute Village Centroids
#
# Read the cleaned shrids produced by 02_shapefiles.R. st_make_valid() is
# applied before computing centroids to handle any self-intersecting polygons.
# The centroid set at this stage covers all villages in base_data; it is
# filtered to MOSAIKS-matched villages only in section 3 before being saved.
# -----------------------------------------------------------------------------

shrids    <- st_read("data/clean/smartcards/shapefiles/shrids.gpkg", quiet = TRUE)
centroids <- st_centroid(st_make_valid(shrids))
coords    <- st_coordinates(centroids)

centroid_coords <- shrids %>%
  mutate(centroid_lon = coords[, 1], centroid_lat = coords[, 2]) %>%
  as.data.frame() %>%
  select(shrid2, centroid_lat, centroid_lon) %>%
  arrange(shrid2)

cat(sprintf("Centroids computed for %d villages\n", nrow(centroid_coords)))

# -----------------------------------------------------------------------------
# 3. Match Centroids to Nearest MOSAIKS Grid Point
#
# MOSAIKS features live on a fixed grid. We first query all grid coordinates
# within the bounding box, then match each centroid to the nearest point.
# Villages more than 1 km from any grid point are excluded (typically water
# bodies or border artefacts).
# -----------------------------------------------------------------------------

dataset <- redivis::redivis$organization("SDSS_data_repository")$dataset("mosaiks:8bqm")

buffer        <- 0.05
lon_min <- round(min(centroid_coords$centroid_lon), 2) - buffer
lon_max <- round(max(centroid_coords$centroid_lon), 2) + buffer
lat_min <- round(min(centroid_coords$centroid_lat), 2) - buffer
lat_max <- round(max(centroid_coords$centroid_lat), 2) + buffer

condition_str <- sprintf(
  "(lon >= %s AND lon <= %s) AND (lat >= %s AND lat <= %s)",
  lon_min, lon_max, lat_min, lat_max
)

mosaik_coords <- dataset$query(sprintf(
  "SELECT lon, lat FROM mosaiks_2019_planet:ergr WHERE %s", condition_str
))$to_tibble()

cat(sprintf("MOSAIKS grid: %d candidate points in bounding box\n", nrow(mosaik_coords)))

chunk_size <- 100
num_chunks <- ceiling(nrow(centroid_coords) / chunk_size)
chunks     <- vector("list", num_chunks)

for (i in seq_len(num_chunks)) {
  cat(sprintf("Matching centroids: chunk %d / %d\n", i, num_chunks))
  rows  <- ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(centroid_coords))
  chunk <- centroid_coords[rows, ]

  dist_matrix <- distm(
    chunk %>% select(centroid_lon, centroid_lat) %>% as.matrix(),
    mosaik_coords %>% as.matrix(),
    fun = distHaversine
  )
  min_idx     <- apply(dist_matrix, 1, which.min)
  min_dist_km <- dist_matrix[cbind(seq_len(nrow(dist_matrix)), min_idx)] / 1000

  chunks[[i]] <- chunk %>%
    mutate(
      mosaik_idx  = min_idx,
      distance_km = min_dist_km,
      lat         = mosaik_coords$lat[mosaik_idx],
      lon         = mosaik_coords$lon[mosaik_idx]
    )
}

coordinates <- bind_rows(chunks) %>%
  filter(distance_km <= 1) %>%
  select(-mosaik_idx, -distance_km)

cat(sprintf("Centroids matched: %d of %d within 1 km\n",
            nrow(coordinates), nrow(centroid_coords)))

out_path <- "data/interim/smartcards/centroid_coords.Rds"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(select(coordinates, shrid2, centroid_lat, centroid_lon), out_path)
cat("Saved to", out_path, "\n")

# -----------------------------------------------------------------------------
# 4. Download MOSAIKS Feature Vectors
#
# 4000 features per village, downloaded in chunks of 1000 to stay within
# query-size limits. Column names are standardised to satellite_1..satellite_4000.
# -----------------------------------------------------------------------------

chunk_size <- 1000
num_chunks <- ceiling(nrow(coordinates) / chunk_size)
chunks     <- vector("list", num_chunks)

for (i in seq_len(num_chunks)) {
  cat(sprintf("Downloading features: chunk %d / %d\n", i, num_chunks))
  rows  <- ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(coordinates))
  chunk <- coordinates[rows, ]

  where_clause <- apply(chunk, 1, function(row) {
    sprintf("(lon = %s AND lat = %s)", row["lon"], row["lat"])
  }) %>% paste(collapse = " OR ")

  features <- dataset$query(sprintf("
    SELECT * EXCEPT(shapeGroup, adm1_shapeID_geoBoundaries, adm2_shapeID_geoBoundaries)
    FROM mosaiks_2019_planet:ergr
    WHERE %s
  ", where_clause))$to_tibble()

  colnames(features)[grep("X_", colnames(features))] <- paste0("satellite_", 1:4000)
  chunks[[i]] <- left_join(chunk, features, by = c("lat", "lon"))
}

satellite_features <- bind_rows(chunks)
cat(sprintf("Satellite features: %d villages x %d feature columns\n",
            nrow(satellite_features),
            sum(startsWith(colnames(satellite_features), "satellite_"))))

# -----------------------------------------------------------------------------
# 5. Save
# -----------------------------------------------------------------------------

out_path <- "data/interim/smartcards/satellite_features.Rds"
saveRDS(satellite_features, out_path)
cat("Saved to", out_path, "\n")

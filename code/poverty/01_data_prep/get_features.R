# =============================================================================
# Title: Get Remote Sensing Features from Mosaiks API
# Purpose: Compute SHRUG polygon centroids, match them to nearest Mosaiks pixels,
#          and download corresponding feature vectors in chunks
# Output: Coordinates file and 4000-dimensional feature chunks
# =============================================================================

rm(list = ls())

## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(geosphere)
  library(redivis) # devtools::install_github("redivis/redivis-r", ref="main")
})
options(readr.show_col_types = FALSE)
sf_use_s2(FALSE)

## Define Redivis Dataset Reference
# redivis::redivis$authenticate(force_reauthentication = TRUE)
dataset <- redivis$organization("sdss")$dataset("mosaiks:8bqm")


# -----------------------------------------------------------------------------
# 1. Compute shrids Centroids and Mosaiks coordinates
# -----------------------------------------------------------------------------
coordinates_path <- "data/poverty/processed/features/coordinates.csv"

if (!file.exists(coordinates_path)) {
  shrids <- st_read("data/poverty/processed/shapefiles/shrids.gpkg", quiet = TRUE)
  centroids <- st_centroid(shrids)
  coords <- st_coordinates(centroids)
  
  centroid_coords <- shrids %>%
    mutate(
      centroid_lon = coords[, 1], 
      centroid_lat = coords[, 2]
    ) %>%
    as.data.frame() %>%
    select(shrid2, centroid_lat, centroid_lon) %>%
    arrange(shrid2)
  
  # Build bounding box for query
  buffer <- 0.05
  lon_min <- round(min(centroid_coords$centroid_lon), 2) - buffer
  lon_max <- round(max(centroid_coords$centroid_lon), 2) + buffer
  lat_min <- round(min(centroid_coords$centroid_lat), 2) - buffer
  lat_max <- round(max(centroid_coords$centroid_lat), 2) + buffer
  
  condition_str <- sprintf(
    "(lon >= %s AND lon <= %s) AND (lat >= %s AND lat <= %s)",
    lon_min, lon_max, lat_min, lat_max
  )
  
  # Query coordinates from Mosaiks
  query <- dataset$query(sprintf("
    SELECT lon, lat
    FROM mosaiks_2019_planet:ergr
    WHERE %s
  ", condition_str))
  
  mosaik_coords <- query$to_tibble()
  
  # Match each shrid's centroid coordinates to nearest mosaik coordinates
  chunk_size <- 100
  chunks <- list()
  num_rows <- nrow(centroid_coords)
  num_chunks <- ceiling(num_rows / chunk_size)
  
  for (i in seq_len(num_chunks)) {
    cat(sprintf("Matching chunk %d of %d\n", i, num_chunks))
    start_row <- (i - 1) * chunk_size + 1
    end_row <- min(i * chunk_size, num_rows)
    chunk <- centroid_coords[start_row:end_row, ]
    
    dist_matrix <- distm(
      chunk %>% select(centroid_lon, centroid_lat) %>% as.matrix(),
      mosaik_coords %>% as.matrix(),
      fun = distHaversine
    )
    
    min_idx <- apply(dist_matrix, 1, which.min)
    min_dist_km <- dist_matrix[cbind(1:nrow(dist_matrix), min_idx)] / 1000
    
    chunk <- chunk %>%
      mutate(
        mosaik_idx = min_idx,
        distance_km = min_dist_km,
        lat = mosaik_coords$lat[mosaik_idx],
        lon = mosaik_coords$lon[mosaik_idx]
      )
    
    chunks[[i]] <- chunk
  }
  
  coordinates <- bind_rows(chunks) %>%
    select(-mosaik_idx)
  dir.create(dirname(coordinates_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(coordinates, coordinates_path)
  cat(sprintf("Saved coordinates to: %s\n", coordinates_path))
} else {
  coordinates <- read_csv(coordinates_path)
}


# -----------------------------------------------------------------------------
# 2. Download Feature Vectors in Chunks
# -----------------------------------------------------------------------------

chunk_size <- 1000
num_rows <- nrow(coordinates)
num_chunks <- ceiling(num_rows / chunk_size)

for (i in seq_len(num_chunks)) {
  cat(sprintf("Querying features for chunk %d of %d\n", i, num_chunks))
  start_row <- (i - 1) * chunk_size + 1
  end_row <- min(i * chunk_size, num_rows)
  chunk <- coordinates[start_row:end_row, ]
  
  where_clause <- apply(chunk, 1, function(row) {
    sprintf("(lon = %s AND lat = %s)", row["lon"], row["lat"])
  }) %>%
    paste(collapse = " OR ")
  
  query <- dataset$query(sprintf("
    SELECT * EXCEPT(shapeGroup, adm1_shapeID_geoBoundaries, adm2_shapeID_geoBoundaries)
    FROM mosaiks_2019_planet:ergr
    WHERE %s
  ", where_clause))
  
  features <- query$to_tibble()
  # colnames(features) <- c(colnames(features)[1:2], paste0("feature_", 1:4000))
  colnames(features)[grep("X_", colnames(features))] <- paste0("feature_", 1:4000)
  features <- left_join(chunk, features, by = c("lat", "lon"))
  
  out_path <- sprintf("data/poverty/processed/features/features_chunk%02d.csv", i)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(features, out_path)
  cat(sprintf("Saved features chunk %d of %d to: %s\n", i, num_chunks, out_path))
}
# Map of PES imagery region with concentric distance buffers and county boundaries
#
# Produces a map showing:
# - PES imagery region (experimental sample) in a distinct color
# - Concentric distance buffers (0-2km, 2-5km, 5-10km, 10-20km, 20-30km)
# - County pair boundaries for the 4 observational regions
# - All ~1km pixel grid points colored by their distance band

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(ggplot2)
  library(ggtext)
})
source("code/utils/fte_theme.R")

# Output directory
output_dir <- "figures/ugandaforestcover"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Load shapefiles
# ============================================================

cat("Loading shapefiles...\n")

# PES imagery boundary
imagery <- st_read("data/clean/ugandaforestcover/shapefiles/imagery_boundary.gpkg", quiet = TRUE)

# Districts
districts <- st_read("data/clean/ugandaforestcover/shapefiles/districts.gpkg", quiet = TRUE)

# Uganda national boundary (dissolved from districts)
uganda_boundary <- st_union(districts)

# ============================================================
# Create distance buffer polygons
# ============================================================

cat("Creating distance buffers...\n")

# Work in UTM for accurate buffering
imagery_utm <- st_transform(st_geometry(imagery), 32636)

# Buffer distances in meters
buffer_dists <- c(2000, 5000, 10000)
buffer_labels <- c("Within 0-2 km", "Within 2-5 km", "Within 5-10 km")

# Create concentric ring polygons (each ring = buffer_i minus buffer_{i-1})
rings <- list()
inner <- imagery_utm

for (i in seq_along(buffer_dists)) {
  outer <- st_buffer(imagery_utm, dist = buffer_dists[i])
  ring <- st_difference(outer, st_union(inner))
  rings[[i]] <- st_sf(
    band = buffer_labels[i],
    geometry = st_geometry(ring)
  )
  inner <- outer
}

rings_sf <- do.call(rbind, rings)
# Transform back to WGS84 for plotting
rings_sf <- st_transform(rings_sf, 4326)

# Order factor for legend
rings_sf$band <- factor(rings_sf$band, levels = buffer_labels)

# ============================================================
# Compute bounding box for the map
# ============================================================

# Use the 30km buffer extent plus a small margin
outer_30km <- st_transform(st_buffer(imagery_utm, dist = 35000), 4326)
bbox <- st_bbox(outer_30km)

# Expand slightly
x_range <- bbox["xmax"] - bbox["xmin"]
y_range <- bbox["ymax"] - bbox["ymin"]
bbox["xmin"] <- bbox["xmin"] - 0.05 * x_range
bbox["xmax"] <- bbox["xmax"] + 0.05 * x_range
bbox["ymin"] <- bbox["ymin"] - 0.05 * y_range
bbox["ymax"] <- bbox["ymax"] + 0.05 * y_range

# ============================================================
# Plot: Map with PES region and distance buffers
# ============================================================

cat("Creating map...\n")

# Color palette for distance bands
band_colors <- c(
  "Experimental" = palette$darkblue,
  setNames(palette$darkgreen, buffer_labels[1]),
  setNames(palette$green,      buffer_labels[2]),
  setNames(palette$lightgreen,  buffer_labels[3])
)

outer_12km <- st_transform(st_buffer(imagery_utm, dist = 15000), 4326)
bbox_zoom <- st_bbox(outer_12km)
x_rng <- bbox_zoom["xmax"] - bbox_zoom["xmin"]
y_rng <- bbox_zoom["ymax"] - bbox_zoom["ymin"]
bbox_zoom["xmin"] <- bbox_zoom["xmin"] - 0.05 * x_rng
bbox_zoom["xmax"] <- bbox_zoom["xmax"] + 0.10 * x_rng
bbox_zoom["ymin"] <- bbox_zoom["ymin"] - 0.05 * y_rng
bbox_zoom["ymax"] <- bbox_zoom["ymax"] + 0.05 * y_rng

# Uganda boundary clipped to zoom extent (NULL if it doesn't intersect)
bbox_zoom_sfc    <- st_as_sfc(st_bbox(
  c(xmin = unname(bbox_zoom["xmin"]), ymin = unname(bbox_zoom["ymin"]),
    xmax = unname(bbox_zoom["xmax"]), ymax = unname(bbox_zoom["ymax"])),
  crs = 4326
))
uganda_in_zoom <- st_intersection(uganda_boundary, bbox_zoom_sfc)
counties_in_view <- st_intersection(st_geometry(districts), bbox_zoom_sfc)

uganda_layer   <- if (length(uganda_in_zoom) > 0 && !all(st_is_empty(uganda_in_zoom)))
  geom_sf(data = st_sf(geometry = uganda_in_zoom), fill = NA, color = "gray30",
          linewidth = 0.7, linetype = "solid")

p_zoom <- ggplot() +
  # County boundaries
  # geom_sf(data = counties_in_view, fill = NA, color = "gray50",
  #         linewidth = 0.4, linetype = "dashed") +
  geom_sf(data = counties_in_view, fill = palette$gray, color = "white", linewidth = 0.5) +

  # PES imagery region
  # geom_sf(data = imagery, aes(fill = ), color = "black",
  #         linewidth = 0.6, alpha = 0.8) +
  geom_sf(data = imagery, aes(fill = "Experimental"), linewidth = 0.2, color = "white", alpha=0.8) +

  # Distance bands
  geom_sf(data = rings_sf, aes(fill = band), linewidth = 0.2, color = NA, alpha=0.8) +

  # geom_sf(data = counties_in_view, fill = NA, color = "white", linewidth = 0.5, linetype="dashed") +

  # Uganda national boundary (if visible in zoom extent)
  # uganda_layer +
  # Color scales
  scale_fill_manual(
    name   = NULL,
    breaks = names(band_colors),
    labels = names(band_colors),
    values = band_colors,
    drop   = FALSE
  ) +
  coord_sf(
    xlim = c(bbox_zoom["xmin"], bbox_zoom["xmax"]),
    ylim = c(bbox_zoom["ymin"], bbox_zoom["ymax"]),
    expand = FALSE
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12, family=font, color="black"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.text = element_markdown(size=12, family=font, color="black"),
    legend.background = element_blank(),
    legend.direction = "vertical",
    legend.box.just = "top",
    legend.key.spacing.y = unit(0.2, "cm"),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "inside",
    legend.position.inside = c(0.8,0.15)
  )

out_zoom <-  file.path(output_dir, "map.jpeg")
ggsave(out_zoom, plot = p_zoom, height = 5, width = 6)
cat("Saved:", out_zoom, "\n")

cat("\nDone!\n")

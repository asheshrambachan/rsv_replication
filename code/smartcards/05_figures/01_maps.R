# =============================================================================
# Maps
#
# Produces four maps of the study area:
#   Figure 1(a): experimental villages only, coloured by treatment arm (D)
#   Figure 1(b): experimental + observational villages, coloured by arm (D)
#   Figure 2(a): untreated (D=0) villages with Ycons=0, coloured by sample
#   Figure 2(b): untreated (D=0) villages with Ycons=1, coloured by sample
#
# Output: figures/smartcards/maps/
# =============================================================================

rm(list=ls())

## Load libraries & theme
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggtext)
  library(sf)
})
source("code/utils/fte_theme.R")

# Joins village-level data to shrids shapefile and builds a layered ggplot list
# (state background, village fills, district borders, legend).
base_map <- function(data, fill, guide, legend.position.inside = c(0.73,0.234)) {

  # Load shapefiles
  state <- st_read("data/clean/smartcards/shapefiles/state.gpkg", quiet=T)
  districts <- st_read("data/clean/smartcards/shapefiles/districts.gpkg", quiet=T)
  shrids <- st_read("data/clean/smartcards/shapefiles/shrids.gpkg", quiet=T)
  data <- right_join(shrids, data, by="shrid2")

  list(
    geom_sf(data = state, fill=palette$gray, color=NA),
    geom_sf(data = data, aes(fill=!!ensym(fill)), color="white", linewidth=0.04),
    geom_sf(data = districts, fill=NA, color="white", linewidth=0.3),
    scale_fill_manual(name=NULL, breaks = guide$breaks, labels = guide$labels, values = guide$colors),
    theme_void(),
    theme(
      legend.text = element_markdown(size=14, family=font, color="black"),
      legend.direction = "vertical",
      legend.box.just = "top",
      legend.key.spacing.y = unit(0.2, "cm"),
      legend.key.size = unit(0.5, "cm"),
      legend.position = "inside",
      legend.position.inside = legend.position.inside
    )
  )
}

# Output directory
output_dir <- "figures/smartcards/maps"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load data
data <- readr::read_csv(
  "data/clean/smartcards/data.csv",
  col_select = c(shrid2, wave, Ycons)
  ) %>%
  mutate(
    S = case_when(
      wave == "Experimental: Treated (2010)"   ~ "e",
      wave == "Experimental: Untreated (2011)" ~ "e",
      wave == "Experimental: Untreated (2012)" ~ "e",
      wave == "Observational (N/A)"            ~ "o"
    ),
    D = if_else(wave == "Experimental: Treated (2010)", 1L, 0L),
    Y = Ycons,
    SD = paste0(S, D),
    SDY = paste0(S, D, Y)
  ) %>%
  distinct()

# Figure 1 guide: colour by sample x treatment arm (D); Y not shown
# Figure 2 guide: colour by sample only (e vs o); Y=0/1 distinguished by plot panel
guide_fig1 <- data.frame(
  breaks = c("e1", "e0", "o0"),
  labels = c("Experimental: *D=1*", "Experimental: *D=0*", "Observational: *D=0*"),
  colors = c(palette$darkblue, palette$blue, palette$green)
)
guide_fig2 <- data.frame(
  breaks = c("e01", "e00", "o01", "o00"),
  labels = c("Experimental: *D=0, Y=1*", "Experimental: *D=0, Y=0*", "Observational: *D=0, Y=1*", "Observational: *D=0, Y=0*"),
  colors = c(palette$blue, palette$blue, palette$green, palette$green)
)


# Figure 1 (a): Experimental villages only
fig_a <- ggplot() +
  base_map(
    data = filter(data, S == "e"),
    fill = SD,
    guide = guide_fig1,
    legend.position.inside = c(0.715,0.234)
  )

# Save figure
output_path <- file.path(output_dir, "map_exp.jpeg")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_a, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))

# Figure 1 (b): Experimental and observational villages
fig_b <- ggplot() +
  base_map(
    data = data,
    fill = SD,
    guide = guide_fig1,
    legend.position.inside = c(0.72,0.2)
  )

# Save figure
output_path <- file.path(output_dir, "map_full.jpeg")
ggsave(output_path, plot = fig_b, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))


# Figure 2 (a): Units with D = 0 and Ycons = 0
fig_a <- ggplot() +
  base_map(
    data = filter(data, D == 0, Y == 0),
    fill = SDY,
    guide = guide_fig2
  )

# Save figure
output_path <- file.path(output_dir, "map_Ycons_d0_y0.jpeg")
ggsave(output_path, plot = fig_a, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))

# Figure 2 (b): Units with D = 0 and Ycons = 1
fig_b <- ggplot() +
  base_map(
    data = filter(data, D == 0, Y == 1),
    fill = SDY,
    guide = guide_fig2
  )

# Save figure
output_path <- file.path(output_dir, "map_Ycons_d0_y1.jpeg")
ggsave(output_path, plot = fig_b, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))

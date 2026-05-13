# =============================================================================
# Map (Stilde overlap design)
#
# Produces one map of the study area using the overlap sample definition
# Stilde. Buffer Mandals (2011) are assigned Stilde=e,o and appear in the
# legend as "Validation: D=0" (distinct from pure experimental or observational
# villages), reflecting their role as overlap units in the Stilde design.
#
#   map_full_Stilde.jpeg  -- all villages, coloured by sample (Stilde definition)
#
# Input:  data/clean/smartcards/data.csv
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
      wave == "Experimental: Untreated (2011)" ~ "e,o",
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
guide_fig1 <- data.frame(
  breaks = c("e1", "e0", "e,o0", "o0"),
  labels = c("Experimental: *D=1*", "Experimental: *D=0*", "Validation: *D=0*", "Observational: *D=0*"),
  colors = c(palette$darkblue, palette$blue, palette$teal, palette$green)
)

# Figure 1: Experimental and observational villages
fig_b <- ggplot() +
  base_map(
    data = data,
    fill = SD,
    guide = guide_fig1,
    legend.position.inside = c(0.72,0.2)
  )

# Save figure
output_path <- file.path(output_dir, "map_full_Stilde.jpeg")
ggsave(output_path, plot = fig_b, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))

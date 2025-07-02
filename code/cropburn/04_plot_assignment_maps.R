# Load libraries, theme, font, and palette
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(ggtext)
library(readr)
source("code/common/ggplot_theme.r")  

# Load shapefiles
districts <- st_read("data/clean/cropburn/districts/districts.shp", quiet=T)
villages <- st_read("data/clean/cropburn/villages/villages.shp", quiet=T) 

# Load fields data and collapse to village level
data <- read_csv(
  "data/clean/cropburn/data.csv",
  col_types = cols(pc11_tv_id = "c"),
  col_select = c(pc11_tv_id, D, S)
  ) %>%
  drop_na(pc11_tv_id) %>%
  group_by(pc11_tv_id) %>%
  summarise(
    D = first(D),
    S = if_else(mean(S == "e") >= 0.5, "e", "o"), 
    SD = paste0(S, D),
    .groups = "drop"
  ) 
data <- right_join(villages, data, by = "pc11_tv_id")

# Base map function
base_map <- function(fill_data) {
  
  # Define legend labels and colors
  labels <- c(
    e1 = "Experimental: *D=1*", 
    e0 = "Experimental: *D=0*", 
    o1 = "Observational: *D=1*", 
    o0 = "Observational: *D=0*"
  )
  colors <- c(
    e1 = palette$darkblue,
    e0 = palette$blue, 
    o1 = palette$darkgreen, 
    o0 = palette$green
  )
  
  list(
    geom_sf(data = districts, fill = palette$gray, color = NA),
    geom_sf(data = fill_data, aes(fill = SD), linewidth = 0.2, color = "white"),
    geom_sf(data = villages, fill = NA, color = "white", linewidth = 0.2),
    geom_sf(data = districts, fill = NA, color = "white", linewidth = 0.5),
    scale_fill_manual(name = NULL, breaks = names(labels), labels = labels, values = colors),
    cropburn_map_theme
  )
}

# Plot A: Experimental villages only
fig_a <- ggplot() +
  base_map(filter(data, S == "e")) +
  theme(legend.position.inside = c(-0.006, 0.5935))

# Plot B: Experimental and observational villages
fig_b <- ggplot() +
  base_map(data) +
  theme(legend.position.inside = c(0, 0.53))

# Save figures
ggsave("output/figures/cropburn_exp.jpeg", plot = fig_a, height = 4, width = 3.9)
ggsave("output/figures/cropburn_exp_and_obs.jpeg", plot = fig_b, height = 4, width = 3.9)

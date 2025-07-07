# Clear environment
rm(list=ls())

# Load libraries, theme, font, and palette
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(sf)
  library(ggtext)
  library(readr)
})
source("code/common/ggplot_theme.r")  

# Load data and collapse to village level
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

# Define fill guide
guide <- data.frame(
  breaks = c("e1", "e0", "o1", "o0"),
  labels = c("Experimental: *D=1*", "Experimental: *D=0*", "Observational: *D=1*", "Observational: *D=0*"),
  colors = c(palette$darkblue, palette$blue, palette$darkgreen, palette$green)
)

# Plot A: Experimental villages only
fig_a <- ggplot() +
  cropburn_base_map(
    data = filter(data, S == "e"), 
    fill = SD, 
    guide = guide, 
    legend.position.inside = c(-0.006, 0.5935)
    )

# Save figure
output_path <- "output/figures/cropburn_maps/exp_only.jpeg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_a, height = 4, width = 3.9)
cat(sprintf("Saved figure to: %s\n", output_path))

# Plot B: Experimental and observational villages
fig_b <- ggplot() +
  cropburn_base_map(
    data = data, 
    fill = SD, 
    guide = guide, 
    legend.position.inside = c(0, 0.53)
    )

# Save figure
output_path <- "output/figures/cropburn_maps/exp_and_obs.jpeg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_b, height = 4, width = 3.9)
cat(sprintf("Saved figure to: %s\n", output_path))
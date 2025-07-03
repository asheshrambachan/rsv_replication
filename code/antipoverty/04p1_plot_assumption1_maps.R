# Clear environment
rm(list=ls())

# Load libraries, theme, font, and palette
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggtext)
  library(sf)
})
source("code/common/ggplot_theme.r")  

# Load data
data <- read.csv("data/clean/antipoverty/results_pca.csv") %>%
  mutate(
    D = if_else(wave == "Treatment", 1, 0),
    S = if_else(wave == "Holdout", "o", "e"), 
    SD = paste0(S, D)
  ) 

# Define legend labels and colors
labels <- c(
  e1 = "Experimental: *D=1*", 
  e0 = "Experimental: *D=0*", 
  o0 = "Observational: *D=0*"
)
colors <- c(
  e1 = palette$darkblue,
  e0 = palette$blue, 
  o0 = palette$green
)

# Plot A: Experimental villages only
fig_a <- ggplot() +
  antipoverty_base_map(
    data = filter(data, S == "e"),
    fill = SD, 
    labels = labels, 
    colors = colors
  ) +
  theme(legend.position.inside = c(0.715,0.234)) 

# Save figure
output_path <- "output/figures/antipoverty_maps/antipoverty_exp.jpeg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_a, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))

# Plot B: Experimental and observational villages
fig_b <- ggplot() +
  antipoverty_base_map(
    data = data, 
    fill = SD, 
    labels = labels, 
    colors = colors
  ) +
  theme(legend.position.inside = c(0.72,0.2)) 

# Save figure
output_path <- "output/figures/antipoverty_maps/antipoverty_exp_and_obs.jpeg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_b, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))

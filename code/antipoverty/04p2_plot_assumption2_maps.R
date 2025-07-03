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
    Y = y_cons,
    SDY = paste0(S, D, Y)
  )

# Define legend labels and colors
labels <- c(
  e01 = "Experimental: *D=0, Y=1*", 
  e00 = "Experimental: *D=0, Y=0*", 
  o01 = "Observational: *D=0, Y=1*",
  o00 = "Observational: *D=0, Y=0*"
)
colors <- c(
  e01 = palette$blue,
  e00 = palette$blue,
  o01 = palette$green,
  o00 = palette$green
)

# Plot A: Units with D = 0 and y_cons = 0
fig_a <- ggplot() +
  antipoverty_base_map(
    data = filter(data, D == 0, y_cons == 0),
    fill = SDY, 
    labels = labels, 
    colors = colors
  ) +
  theme(legend.position.inside = c(0.73,0.234)) 

# Save figure
output_path <- "output/figures/antipoverty_maps/antipoverty_D0_Ycons0.jpeg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_a, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))

# Plot B: Units with D = 0 and y_cons = 1
fig_b <- ggplot() +
  antipoverty_base_map(
    data = filter(data, D == 0, y_cons == 1),
    fill = SDY, 
    labels = labels, 
    colors = colors
  ) +
  theme(legend.position.inside = c(0.73,0.234)) 

# Save figure
output_path <- "output/figures/antipoverty_maps/antipoverty_D0_Ycons1.jpeg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_b, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))

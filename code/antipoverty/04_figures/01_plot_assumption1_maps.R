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
data <- read.csv("data/clean/antipoverty/pca.csv") %>%
  mutate(
    D = if_else(wave == "Treatment", 1, 0),
    S = if_else(wave == "Holdout", "o", "e"), 
    SD = paste0(S, D)
  ) 

# Define fill guide
guide <- data.frame(
  breaks = c("e1", "e0", "o0"),
  labels = c("Experimental: *D=1*", "Experimental: *D=0*", "Observational: *D=0*"),
  colors = c(palette$darkblue, palette$blue, palette$green)
)

# Plot A: Experimental villages only
fig_a <- ggplot() +
  antipoverty_base_map(
    data = filter(data, S == "e"), 
    fill = SD, 
    guide = guide,
    legend.position.inside = c(0.715,0.234)
  )

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
    guide = guide,
    legend.position.inside = c(0.72,0.2)
  )

# Save figure
output_path <- "output/figures/antipoverty_maps/antipoverty_exp_and_obs.jpeg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_b, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))

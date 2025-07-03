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

# Plot A: Experimental villages only
fig_a <- ggplot() +
  cropburn_base_map(filter(data, S == "e"), fill=SD, labels=labels, colors=colors) +
  theme(legend.position.inside = c(-0.006, 0.5935))

# Save figures
output_path <- "output/figures/cropburn_exp.jpeg"
ggsave(output_path, plot = fig_a, height = 4, width = 3.9)
cat(sprintf("Saved figure to: %s\n", output_path))

# Plot B: Experimental and observational villages
fig_b <- ggplot() +
  cropburn_base_map(data, fill=SD, labels=labels, colors=colors) +
  theme(legend.position.inside = c(0, 0.53))

# Save figures
output_path <- "output/figures/cropburn_exp_and_obs.jpeg"
ggsave(output_path, plot = fig_b, height = 4, width = 3.9)
cat(sprintf("Saved figure to: %s\n", output_path))
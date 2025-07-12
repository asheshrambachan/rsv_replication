# Clear environment
rm(list=ls())

# Load libraries, theme, font, and palette
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggtext)
  library(sf)
})
source("code/ggplot_theme.r")  

# Load data
data <- read_csv(
  "data/processed/poverty/data.csv",
  col_select = c(shrid2, wave, Ycons)
  ) %>%
  mutate(
    D = if_else(wave == "Treatment", 1, 0),
    S = if_else(wave == "Holdout", "o", "e"), 
    Y = Ycons,
    SD = paste0(S, D),
    SDY = paste0(S, D, Y)
  ) %>%
  distinct()

# Define fill guide
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
  antipoverty_base_map(
    data = filter(data, S == "e"), 
    fill = SD, 
    guide = guide_fig1,
    legend.position.inside = c(0.715,0.234)
  )

# Save figure
output_path <- "output/figures/poverty/maps/poverty_map_Se.jpeg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_a, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))

# Figure 1 (b): Experimental and observational villages
fig_b <- ggplot() +
  antipoverty_base_map(
    data = data, 
    fill = SD,
    guide = guide_fig1,
    legend.position.inside = c(0.72,0.2)
  )

# Save figure
output_path <- "output/figures/poverty/maps/poverty_map.jpeg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_b, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))


# Figure 2 (a): Units with D = 0 and Ycons = 0
fig_a <- ggplot() +
  antipoverty_base_map(
    data = filter(data, D == 0, Y == 0), 
    fill = SDY, 
    guide = guide_fig2
  )

# Save figure
output_path <- "output/figures/poverty/maps/poverty_map_D0_Ycons0.jpeg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_a, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))

# Figure 2 (b): Units with D = 0 and Ycons = 1
fig_b <- ggplot() +
  antipoverty_base_map(
    data = filter(data, D == 0, Y == 1),
    fill = SDY, 
    guide = guide_fig2
  )

# Save figure
output_path <- "output/figures/poverty/maps/poverty_map_D0_Ycons1.jpeg"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = fig_b, height = 4, width = 4.5)
cat(sprintf("Saved figure to: %s\n", output_path))
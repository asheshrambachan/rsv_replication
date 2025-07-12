# Clear environment
rm(list=ls())

# Load libraries, theme, font, and palette
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggtext)
})
source("code/ggplot_theme.r")  

# Load data
data <- read.csv("data/processed/poverty/pca.csv") %>%
  mutate(
    D = if_else(wave=="Treatment", 1, 0),
    S = if_else(wave == "Holdout", "o", "e"),
    R_PC1_scaled = - R_PC1_scaled
  )

# Define legend labels and colors
labels <- c(
  e10 = "Experimental: *D=1, Y=0*", 
  e11 = "Experimental: *D=1, Y=1*", 
  e00 = "Experimental: *D=0, Y=0*", 
  e01 = "Experimental: *D=0, Y=1*", 
  o00 = "Observational: *D=0, Y=0*", 
  o01 = "Observational: *D=0, Y=1*"
)
colors <- c(
  e10 = palette$darkblue, 
  e11 = palette$darkblue, 
  e00 = palette$blue, 
  e01 = palette$blue, 
  o00 = palette$green, 
  o01 = palette$green
)
linetypes <- c(
  e10 = "solid", 
  e11 = "solid", 
  e00 = "solid",
  e01 = "solid",
  o00 = "dashed", 
  o01 = "dashed"
)

for (d in c(0,1)){
  for (y in c(0,1)){
    for (Y_var in c("Ycons","Ylowinc", "Ymidinc")){
      data_y <- data %>%
        mutate(
          Y = !!sym(Y_var),
          SDY = paste0(S,D,Y)
        ) %>% 
        filter(D == d, Y == y) 
      
      density <- ggplot(data = data_y, aes(x=R_PC1_scaled, color=SDY, fill=SDY, linetype=SDY)) +
        geom_density(linewidth=0.5, alpha=0.2, position="identity") +
        labs(
          x = "First principal component of *R*",
          y = "Density"
        ) +
        scale_x_continuous(minor_breaks=seq(0,1, by=0.05), limits = c(-8.7, 1.8)) +
        scale_y_continuous(minor_breaks=seq(0,1, by=0.05), limits = c(0,0.8)) +
        scale_color_manual(name = NULL, breaks = names(labels), labels = labels, values = colors) +
        scale_fill_manual(name = NULL, breaks = names(labels), labels = labels, values = colors) +
        scale_linetype_manual(name = NULL, breaks = names(labels), labels = labels, values = linetypes) +
        density_theme
      
      # Save figure
      output_path <- sprintf("output/figures/poverty/distributions/%s/poverty_Rdist_D%s_%s%s.jpeg", Y_var, d, Y_var, y)
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
      ggsave(output_path, plot = density, height = 4, width = 4.5)
      cat(sprintf("Saved figure to: %s\n", output_path))
    }
  }
}


# =============================================================================
# Title: Plot Distribution of First Principal Component of Remote Sensing Features
# Purpose: Visualizes RSV PC1 distributions by experimental group and outcome
# Output: outputs/poverty/distributions/[Ycons|Ylowinc|Ymidinc]/poverty_Rdist_D[0|1]_[Yvar][0|1].jpeg
# =============================================================================

rm(list = ls())

## Load libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggtext)
})
source("code/ggplot_theme.r")  

## Load PCA data
data <- read.csv("data/poverty/processed/pca_data.csv") %>%
  mutate(
    D = if_else(wave == "Treatment", 1, 0),
    S = if_else(wave == "Holdout", "o", "e")
  )

## Define aesthetics
guide <- data.frame(
  breaks = c("e10","e11","e00","e01","o00","o01"),
  labels = c(
    "Experimental: *D=1, Y=0*", "Experimental: *D=1, Y=1*", 
    "Experimental: *D=0, Y=0*", "Experimental: *D=0, Y=1*", 
    "Observational: *D=0, Y=0*", "Observational: *D=0, Y=1*"
  ),
  colors = c(palette$darkblue, palette$darkblue, palette$blue, palette$blue, palette$green, palette$green),
  linetypes = c("solid", "solid", "solid","solid","dashed", "dashed")
)

## Generate and Save Plots
for (d in c(0,1)){
  for (y in c(0,1)){
    for (Y_var in c("Ycons","Ylowinc", "Ymidinc")){
      data_y <- data %>%
        mutate(
          Y = !!sym(Y_var),
          SDY = paste0(S,D,Y)
        ) %>% 
        filter(D == d, Y == y) 
      
      density <- density_base_plot(data_y, x=R_PC1_scaled, fill=SDY, guide=guide)
      
      # Save figure
      output_path <- sprintf("outputs/poverty/distributions/%s/poverty_Rdist_D%s_%s%s.jpeg", Y_var, d, Y_var, y)
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
      ggsave(output_path, plot = density, height = 4, width = 4.5)
      cat(sprintf("Saved figure to: %s\n", output_path))
    }
  }
}


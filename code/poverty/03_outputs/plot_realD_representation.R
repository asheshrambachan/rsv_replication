rm(list=ls())

## Load libraries & theme
suppressPackageStartupMessages({
  library(ggpubr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggtext)
  library(binsreg)
})
font <- "Times"

# Output directory 
output_dir <- "outputs/poverty/realD"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

data <- read.csv("data/poverty/processed/realD_representations.csv")

ylim_min <- numeric()
ylim_max <- numeric()
plot_data <- list()
for (S_var in c("synthS", "realS")){

  data_S <- data %>% filter(S==S_var)
  
  suppressWarnings({
    binscatter <- binsreg(
      data = data_S, 
      x = Y, y = H, 
      randcut=1,
      polyreg=3,
      dots=T
    )
  })
  
  ylim <- layer_scales(binscatter$bins_plot)$y$range$range
  ylim_min <- c(ylim_min, min(ylim))
  ylim_max <- c(ylim_max, max(ylim))
  
  plot_data[[S_var]] <- binscatter$data.plot$`Group Full Sample`
}

xlim <- c(0, 1)
ylim <- c(min(ylim_min), max(ylim_max))

for (S_var in c("synthS", "realS")){
  fig <- ggplot() +   
    geom_hline(aes(yintercept = 0), linewidth=0.5, color="black", linetype="dashed") +
    geom_point(data=data %>% filter(S==S_var), aes(x=Y, y=H, color = "Raw data"), size=0.01, alpha=0.5) +
    geom_point(data=plot_data[[S_var]]$data.dots, aes(x=x, y=fit, color = "Binscatter"), size=0.8) +
    geom_line(data=plot_data[[S_var]]$data.poly, aes(x=x, y=fit, color = "Polynomial fit"), linewidth=0.5) +
    labs(
      x = "Prediction of low consumption",
      y = "Optimal represention *H(R)*",
      colour = NULL
    ) +
    scale_color_manual(
      breaks = c("Raw data", "Binscatter", "Polynomial fit"),
      values = c("#8DB1CE", "#2D2F2E", "#2D2F2E")
    ) +
    scale_x_continuous(minor_breaks = seq(0,1,0.05)) + #
    scale_y_continuous(minor_breaks = seq(-2,5,0.2)) + #
    coord_cartesian(xlim = xlim, ylim = ylim) +
    theme_bw() + 
    theme(
      legend.position = "none",
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      text = element_text(color="#2D2F2E", size=14, family=font),
      axis.title = element_markdown(color="#2D2F2E", size=14, family=font),
      axis.text = element_text(color="#2D2F2E", size=12, family=font)
    ) + 
    guides(color = guide_legend(override.aes = list(size=1)))
  
  
  output_path <- file.path(output_dir, sprintf("poverty_realD_rep_%s.jpeg", S_var))
  ggsave(output_path, plot = fig, height = 4, width = 4.6)
  cat(sprintf("Saved figure to: %s\n", output_path))
  
}

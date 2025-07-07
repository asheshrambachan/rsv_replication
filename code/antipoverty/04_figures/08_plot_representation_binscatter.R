# Third: For this figure, could you output the version
# that does not normalize the X-axis (i.e., the x-axis just goes from [0,1])? 
# Mapping to the previous figures, do you know what version of the empirical 
# application this corresponds to (i.e., Random subset or buffer and holdout)?
  
rm(list = ls())
library(ggpubr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(binsreg)

font <- "Times"

data <- read.csv("data/clean/antipoverty/representations.csv")

for (S_name in c("synthetic", "real")){

  data_S <- data %>% filter(S==S_name)
  
  binscatter <- binsreg(
    data = data_S, 
    x = Y, y = H, 
    randcut=1,
    polyreg=3,
    dots=T
  )
  
  fig <- ggplot() + 
    geom_hline(yintercept = 0, linewidth=0.3, color="gray") +
    geom_point(data=data_S, aes(x=Y, y=H, color = "Raw data"), size=0.01, alpha=0.2) +
    geom_point(data=binscatter$data.plot$`Group Full Sample`$data.dots, aes(x=x, y=fit, color = "Binscatter"), size=0.8) +
    geom_line(data=binscatter$data.plot$`Group Full Sample`$data.poly, aes(x=x, y=fit, color = "Polynomial fit"), linewidth=0.5) +
    
    labs(
      x = "Prediction of low consumption",
      y = "Optimal represention *H(R)*",
      colour = NULL
    ) +
    scale_color_manual(
      breaks = c("Raw data", "Binscatter", "Polynomial fit"),
      values = c("#8DB1CE", "#2D2F2E", "#2D2F2E")
    ) +
    scale_x_continuous(minor_breaks = seq(-5,5,0.2)) +
    scale_y_continuous(limits = c(-2, 4), minor_breaks = seq(-5,5,0.2)) +
    theme_bw() + 
    theme(
      legend.position = "none",
      panel.grid.major=element_line(linewidth=0.2),
      panel.grid.minor=element_line(linewidth=0.1),
      text = element_text(color="#2D2F2E", size=14, family=font),
      axis.title = element_markdown(color="#2D2F2E", size=14, family=font),
      axis.text = element_text(color="#2D2F2E", size=12, family=font)
    ) + 
    guides(color = guide_legend(override.aes = list(size=1)))
  
  
  output_path <- sprintf("output/figures/antipoverty_representation_binscatter/%s.jpeg", S_name)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(output_path, plot = fig, height = 4, width = 4.6)
  cat(sprintf("Saved figure to: %s\n", output_path))
  
}

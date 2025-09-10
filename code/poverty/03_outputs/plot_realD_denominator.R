rm(list=ls())

## Load libraries & theme
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(latex2exp)
  library(stringr)
})
source("code/ggplot_theme.r")  

# Output directory 
output_dir <- "outputs/poverty/realD"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

guide <- data.frame(
  breaks = c("rsv_synthS", "rsv_realS"),
  labels = c("RSV: Synthetic samples", "RSV: Real samples"),
  colors = c(palette$blue, palette$darkblue),
  shapes = c(15, 19)
)


data <- read.csv("data/poverty/processed/realD_denominators.csv") %>%
  mutate(
    estimator_S = ifelse(is.na(S), estimator, paste0(estimator, "_", S)),
    estimator_S = factor(estimator_S, levels=guide$breaks, labels=guide$labels)
  )

ylim <- c(0, max(data$uci))

for (Y_var in c("Ycons", "Ylowinc", "Ymidinc")){
  fig <- data %>%
    filter(Y==Y_var) %>%
    ggplot(aes(x=estimator_S, y=coef, shape=estimator_S, color=estimator_S)) +
    geom_errorbar(aes(ymin=lci, ymax=uci), linewidth=0.65, width = 0.28) +
    geom_point(size = 2.5) +
    geom_hline(aes(yintercept = 0), linewidth=0.5, color="black", linetype="dashed") +
    labs(
      y = ifelse(Y_var=="Ycons", "Denominator (Relevance)", ""),
      shape=NULL,
      color=NULL
    ) +
    scale_color_manual(
      breaks = guide$labels,
      values = guide$colors
    ) +
    scale_shape_manual(
      breaks = guide$labels,
      values = guide$shape
    ) +
    scale_x_discrete(labels = ~ str_wrap(as.character(.x), 5, whitespace_only=F)) +
    scale_y_continuous(limits = ylim, minor_breaks = seq(-2,10,0.25)) + 
    theme_bw() +
    theme(
      legend.position="none",
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.y=element_line(linewidth=0.3),
      panel.grid.minor.y=element_line(linewidth=0.1),
      text = element_text(size=14, family=font, color="black"),
      axis.title.y = element_text(size=14, family=font, color="black"),
      axis.title.x = element_blank(),
      axis.text = element_text(size=12, family=font, color="black"),
    )
  
  output_path <- file.path(output_dir, sprintf("poverty_realD_den_%s.jpeg", Y_var))
  ggsave(output_path, plot = fig, height = 3, width = 3)
  cat(sprintf("Saved figure to: %s\n", output_path))
}

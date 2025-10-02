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
  breaks = c("benchmark_with", "rsv_synthS_with", "rsv_realS_with", "benchmark_without", "rsv_synthS_without", "rsv_realS_without"),
  labels = c("Bench-mark: w/ spill.", "RSV: synth. w/ spill.", "RSV: real w/ spill.", "Bench-mark: w/o spill.", "RSV: synth. w/o spill.", "RSV: real w/o spill."),
  colors = c("black", palette$blue, palette$darkblue, "gray", palette$green, palette$darkgreen),
  linetype = c( "solid", "solid", "solid", "solid", "solid", "solid"),
  shapes = c(17, 15, 19, 17, 15, 19)
)

data <- read.csv("data/poverty/processed/realD_coefs.csv") %>%
  mutate(
    estimator_S = ifelse(is.na(S), paste0(estimator, "_", spillover), paste0(estimator, "_", S, "_", spillover)),
    estimator_S = factor(estimator_S, levels = guide$breaks, labels = guide$labels)
  )

ylim <- c(min(data$lci), max(data$uci))

for (Y_var in c("Ycons", "Ylowinc", "Ymidinc")){
  fig <- data %>%
    filter(Y==Y_var) %>%
    ggplot(aes(x=estimator_S, y=coef, shape=estimator_S, color=estimator_S, linetype=estimator_S)) +
    geom_errorbar(aes(ymin=lci, ymax=uci), linewidth=0.65, width = 0.3) +
    geom_point(size = 2) +
    labs(
      x = "",
      y = ifelse(Y_var=="Ycons", TeX("Treatment effect $\\theta$"), ""),
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
    scale_linetype_manual(
      breaks = guide$labels,
      values = guide$linetype
    ) +
    scale_x_discrete(labels = ~ str_wrap(as.character(.x), 5, whitespace_only=F)) +
    scale_y_continuous(limits=ylim, breaks = seq(-2,2,0.05 ), minor_breaks = seq(-2,2,0.01)) +
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
      axis.text = element_text(size=9, family=font, color="black"), #, angle = 45, hjust = 1, vjust = 1),
    )
  
  output_path <- file.path(output_dir, sprintf("poverty_realD_te_%s_by_spillover.jpeg", Y_var))
  ggsave(output_path, plot = fig, height = 3, width = 3)
  cat(sprintf("Saved figure to: %s\n", output_path))
  
}

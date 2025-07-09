suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(latex2exp)
  library(stringr)
})

source("code/ggplot_theme.r")  


guide <- data.frame(
  breaks = c("benchmark", "rsv_Ssynth", "rsv_Sreal"),
  labels = c("Benchmark", "RSV: Synthetic samples", "RSV: Real samples"),
  colors = c("black", palette$blue, palette$darkblue),
  shapes = c(17, 15, 19)
)

data <- read.csv("data/processed/poverty/rsv_vs_benchmark.csv") %>%
  mutate(
    estimator_S = ifelse(is.na(S), estimator, paste0(estimator, "_", S)),
    estimator_S = factor(estimator_S, levels = guide$breaks, labels = guide$labels)
  )

for (Y_var in c("Ycons", "Ylow", "Ymid")){
  fig <- data %>%
    filter(Y==Y_var) %>%
    ggplot(aes(x=estimator_S, y=coef, shape=estimator_S, color=estimator_S)) +
    geom_errorbar(aes(ymin=lci, ymax=uci), linewidth=0.65, width = 0.45) +
    geom_point(size = 2.5) +
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
    scale_x_discrete(labels = ~ str_wrap(as.character(.x), 5, whitespace_only=F)) +
    scale_y_continuous(limits=c(-0.16, 0.015), minor_breaks = seq(-2,2,0.01)) +
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
  
  output_path <- sprintf("output/figures/poverty/rsv_vs_benchmark/poverty_te_%s.jpeg", Y_var)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(output_path, plot = fig, height = 3, width = 3)
  cat(sprintf("Saved figure to: %s\n", output_path))
  
}



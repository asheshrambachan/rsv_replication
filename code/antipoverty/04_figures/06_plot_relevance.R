suppressPackageStartupMessages({
  library(ggplot2)
  
  library(dplyr)
  library(latex2exp)
  library(stringr)
})

source("code/common/ggplot_theme.r")  

guide <- data.frame(
  breaks = c("rsv_synthetic", "rsv_real"),
  labels = c("RSV: Synthetic samples", "RSV: Real samples"),
  colors = c(palette$blue, palette$darkblue),
  shapes = c(15, 19)
)


data <- read.csv("data/clean/antipoverty/estimates.csv") %>%
  filter(!is.na(denominator_coef)) %>%
  mutate(
    estimator_S = ifelse(is.na(S), estimator, paste0(estimator, "_", S)),
    estimator_S = factor(estimator_S, levels=guide$breaks, labels=guide$labels)
  )

for (Y_name in c("cons", "05k", "10k")){
  fig <- data %>%
    filter(Y==Y_name) %>%
    ggplot(aes(x=estimator_S, y=denominator_coef, shape=estimator_S, color=estimator_S)) +
    geom_errorbar(aes(ymin=denominator_lci, ymax=denominator_uci), linewidth=0.65, width = 0.28) +
    geom_point(size = 2.5) +
    geom_hline(aes(yintercept = 0), linewidth=0.5, color="black", linetype="dashed") +
    labs(
      y = ifelse(Y_name=="cons", "Denominator (Relevance)", ""),
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
    scale_y_continuous(limits=c(0, 2.2), minor_breaks = seq(-2,3,0.1)) + #c(0.62, 2.2)
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
  
  output_path <- sprintf("output/figures/antipoverty_relevance/Y%s.jpeg", Y_name)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(output_path, plot = fig, height = 3, width = 3)
  cat(sprintf("Saved figure to: %s\n", output_path))
}

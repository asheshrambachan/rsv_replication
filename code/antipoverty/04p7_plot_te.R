suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(latex2exp)
  library(stringr)
})


font <- "Times" 

est_S_theme <- data.frame(
  levels = c("true", "rsv_random", "rsv_buffer_holdout"),
  labels = c("Benchmark", "RSV: Synthetic samples", "RSV: Real samples"),
  colors = c("black", "#8DB1CE", "#43739D"),
  shapes = c(17, 15, 19)
)

data <- read.csv("data/clean/antipoverty/results_main.csv") %>%
  filter(
    estimator=="true" |  (estimator=="rsv" & (S %in% c("buffer_holdout", "random")))
  ) %>%
  mutate(
    estimator_S = ifelse(is.na(S), estimator, paste0(estimator, "_", S)),
    estimator_S = factor(estimator_S, levels=est_S_theme$levels, labels=est_S_theme$labels)
  )

for (Y_name in c("cons", "05k", "10k")){
  fig <- data %>%
    filter(Y==Y_name) %>%
    ggplot(aes(x=estimator_S, y=coef, shape=estimator_S, color=estimator_S)) +
    geom_errorbar(aes(ymin=lci, ymax=uci), linewidth=0.65, width = 0.45) +
    geom_point(size = 2.5) +
    labs(
      x = "",
      y = ifelse(Y_name=="cons", TeX("Treatment effect $\\theta$"), ""),
      shape=NULL,
      color=NULL
    ) +
    scale_color_manual(
      breaks = est_S_theme$labels,
      values = est_S_theme$colors
    ) +
    scale_shape_manual(
      breaks = est_S_theme$labels,
      values = est_S_theme$shape
    ) +
    scale_x_discrete(labels = ~ str_wrap(as.character(.x), 5, whitespace_only=F)) +
    scale_y_continuous(limits=c(-0.16, 0.01), minor_breaks = seq(-2,2,0.01)) +
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
  
  output_path <- sprintf("output/figures/antipoverty_te/Y%s.jpeg", Y_name)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  # if (Y_name!="cons") 
    # fig <- fig + theme(axis.title.y = element_blank())
  ggsave(output_path, plot = fig, height = 3, width = 3)
  cat(sprintf("Saved figure to: %s\n", output_path))
  
}



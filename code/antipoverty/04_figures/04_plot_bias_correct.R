# Load necessary libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})
is_first <- function(n, n_list) n==n_list[1]
is_last <- function(n, n_list) n==n_list[length(n_list)]

font <- "Times"

est_theme <- data.frame(
  levels = c("rsv", "surrogate"),
  labels = c("RSV", "Common practice"),
  colors = c("#43739D", "#ff6666"),
  shapes = c(19, 23)
)

data <- read.csv("data/clean/antipoverty/rsv_vs_surrogate_correct.csv")  %>%
  mutate(
    estimator = factor(estimator, levels=est_theme$levels, labels=est_theme$labels)
  )
n_list <- sort(unique(data$sample_size))

for (n in n_list){
  fig <- data %>% 
    filter(sample_size == n) %>% 
    ggplot(aes(x = theta, y =  bias, group = estimator, shape = estimator, fill = estimator, color = estimator)) + 
    geom_hline(yintercept = 0, linetype = 'dashed', color='black', linewidth=0.5) +
    geom_line(linewidth = 0.65) +
    geom_point(size = 2.5) +  
    scale_color_manual(
      breaks = est_theme$labels,
      values = est_theme$colors
    ) +
    scale_fill_manual(
      breaks = est_theme$labels,
      values = est_theme$colors
    ) +
    scale_shape_manual(
      breaks = est_theme$labels,
      values = est_theme$shape
    ) +
    theme_bw() + 
    labs(
      x = latex2exp::TeX("Treatment effect $\\theta$"),
      y = ifelse(is_first(n, n_list), "Average bias", ""),
      color = NULL,
      shape = NULL,
      fill = NULL
    ) +
    scale_y_continuous(limits = c(-0.4, 0.25), minor_breaks = seq(-0.5, 0.25, 0.025)) +
    scale_x_continuous(limits = c(-0.1, 0.5), breaks = seq(-1, 1, 0.1)) +
    theme(
      legend.position = "inside",
      legend.justification = c(1, 1), 
      legend.position.inside = c(0.95, 0.95),
      legend.box.background = element_rect(fill="white"),
      text = element_text(size=14, family=font, color="black"),
      legend.text = element_text(size=12, family=font, color="black"),
      axis.text = element_text(size=12, family=font, color="black"),
      axis.title = element_text(size=14, family=font, color="black"),
      panel.grid.major = element_line(linewidth=0.3),
      panel.grid.minor = element_line(linewidth=0.1),
      panel.grid.minor.x = element_blank(),
    )
  
  if (n!=3000) 
    fig <- fig + theme(legend.position = "none")
  
  output_path <- sprintf("output/figures/antipoverty_bias/n%s_correct.jpeg", n)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(plot = fig, filename = output_path, width = 3, height = 3)
  cat(sprintf("Saved figure to: %s\n", output_path))
}



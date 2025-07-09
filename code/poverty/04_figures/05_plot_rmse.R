# Load necessary libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

font <- "Times"

est_theme <- data.frame(
  levels = c("rsv", "surrogate"),
  labels = c("RSV", "Common practice"),
  colors = c("#43739D", "#ff6666"),
  shapes = c(19, 23)
)

data <- read.csv("data/processed/poverty/rsv_vs_surrogate.csv")  %>%
  mutate(
    estimator = factor(estimator, levels=est_theme$levels, labels=est_theme$labels)
  )

n_list <- sort(unique(data$sample_size))
for (n in n_list){
  fig <- data %>% 
    filter(sample_size == n) %>% 
    ggplot(aes(x = theta, y = rmse, group=estimator, color=estimator, fill=estimator, shape=estimator)) + 
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
      y = ifelse(n==1000, "Root mean square error", ""),
      color = NULL,
      shape = NULL,
      fill = NULL
    ) +
    scale_x_continuous(limits = c(-0.1, 0.5), breaks = seq(-1, 1, 0.1)) +
    scale_y_continuous(limits = c(0, 0.5), minor_breaks = seq(0, 10, 0.02)) +
    theme( 
      legend.position = "inside",
      legend.justification = c(1, 1), 
      legend.position.inside = c(0.95, 0.95),
      legend.box.background = element_rect(fill = "white"),
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
  
  output_path <- sprintf("output/figures/poverty/rsv_vs_surrogate/poverty_rmse_n%s.jpeg", n)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(plot = fig, filename = output_path, width = 3, height = 3)
  cat(sprintf("Saved figure to: %s\n", output_path))
}

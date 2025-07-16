# Load necessary libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

source("code/ggplot_theme.r")  

guide <- data.frame(
  breaks = c("rsv", "surrogate"),
  labels = c("RSV", "Common practice"),
  colors = c(palette$darkblue, palette$red),
  shapes = c(19, 23)
)

data <- read.csv("data/processed/poverty/rsv_vs_surrogate.csv")

for (n in sort(unique(data$sample_size))){
  fig_bias <- data %>% 
    filter(sample_size == n) %>% 
    ggplot(aes(x = theta, y = bias, group = estimator, shape = estimator, fill = estimator, color = estimator)) + 
    geom_hline(yintercept = 0, linetype = 'dashed', color='black', linewidth=0.5) +
    geom_line(linewidth = 0.65) +
    geom_point(size = 2.5) +  
    labs(
      x = latex2exp::TeX("Treatment effect $\\theta$"),
      y = ifelse(n==1000, "Average bias", "")
    ) +
    scale_x_continuous(limits = c(-0.1, 0.5), breaks = seq(-1, 1, 0.1)) +
    scale_y_continuous(limits = c(-0.4, 0.25), minor_breaks = seq(-0.5, 0.25, 0.025)) +
    bias_rmse_theme(guide)
  
  fig_rmse <- data %>% 
    filter(sample_size == n) %>% 
    ggplot(aes(x = theta, y = rmse, group=estimator, color=estimator, fill=estimator, shape=estimator)) + 
    geom_hline(yintercept = 0, linetype = 'dashed', color='black', linewidth=0.5) +
    geom_line(linewidth = 0.65) +
    geom_point(size = 2.5) +  
    labs(
      x = latex2exp::TeX("Treatment effect $\\theta$"),
      y = ifelse(n==1000, "Root mean square error", "")
    ) +
    scale_x_continuous(limits = c(-0.1, 0.5), breaks = seq(-1, 1, 0.1)) +
    scale_y_continuous(limits = c(0, 0.5), minor_breaks = seq(0, 10, 0.02)) +
    bias_rmse_theme(guide)
  
  if (n!=3000) {
    fig_bias <- fig_bias + theme(legend.position = "none")
    fig_rmse <- fig_rmse + theme(legend.position = "none")
  }
  
  # Save figure
  output_path <- sprintf("output/figures/poverty/rsv_vs_surrogate/poverty_bias_n%s.jpeg", n)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(plot = fig_bias, filename = output_path, width = 3, height = 3)
  cat(sprintf("Saved figure to: %s\n", output_path))
  
  # Save figure
  output_path <- sprintf("output/figures/poverty/rsv_vs_surrogate/poverty_rmse_n%s.jpeg", n)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(plot = fig_rmse, filename = output_path, width = 3, height = 3)
  cat(sprintf("Saved figure to: %s\n", output_path))
}



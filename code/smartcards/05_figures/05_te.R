# =============================================================================
# Treatment Effect Figures
#
# Plots point estimates and 90% confidence intervals for the RSV estimator
# and the OLS benchmark, for each outcome. Produced for both the full sample
# and the no-spillover robustness sample.
#
# Input:  data/clean/smartcards/empirical_results.csv
# Output: figures/smartcards/empirical_te/{full,nospillover}/
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(dplyr)
  library(latex2exp)
  library(stringr)
})
source("code/utils/fte_theme.R")

alpha <- 0.1              # 90% CIs to match the paper's reporting convention
z90   <- qnorm(1 - alpha / 2)

guide <- data.frame(
  breaks = c("Benchmark", "RSV"),
  colors = c("black", palette$darkblue),
  shapes = c(17, 19)
)

all_results <- fread("data/clean/smartcards/empirical_results.csv")

# Produce figures for both the main sample and the no-spillover robustness check
for (sample in c("full", "nospillover")) {
  output_dir <- file.path("figures/smartcards/empirical_te", sample)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  res  <- all_results %>% filter(.data$sample == .env$sample)

  data <- bind_rows(
    res %>% transmute(Y_var, estimator = "Benchmark", coef = bench_coef, se = bench_se),
    res %>% transmute(Y_var, estimator = "RSV",       coef = rsv_coef,   se = rsv_se)
  ) %>%
    mutate(
      lci       = coef - z90 * se,
      uci       = coef + z90 * se,
      estimator = factor(estimator, levels = guide$breaks)
    )

  ylim   <- c(min(data$lci), max(data$uci))
  Y_vars <- unique(data$Y_var)

  for (Y_var in Y_vars) {
    fig <- data %>%
      filter(Y_var == !!Y_var) %>%
      ggplot(aes(x = estimator, y = coef, shape = estimator, color = estimator)) +
      geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed", color = "grey40") +
      geom_errorbar(aes(ymin = lci, ymax = uci), linewidth = 0.65, width = 0.28) +
      geom_point(size = 2.5) +
      scale_color_manual(breaks = guide$breaks, values = guide$colors) +
      scale_shape_manual(breaks = guide$breaks, values = guide$shapes) +
      scale_x_discrete(labels = ~str_wrap(as.character(.x), 5, whitespace_only = FALSE)) +
      scale_y_continuous(limits = ylim, minor_breaks = seq(-2, 2, 0.01)) +
      labs(
        x     = NULL,
        y     = if (Y_var == "Ycons") TeX("Treatment effect $\\theta$") else NULL,
        shape = NULL,
        color = NULL
      ) +
      theme_bw() +
      theme(
        legend.position    = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.3),
        panel.grid.minor.y = element_line(linewidth = 0.1),
        text       = element_text(size = 14, family = font, color = "black"),
        axis.title = element_text(size = 14, family = font, color = "black"),
        axis.text  = element_text(size = 12, family = font, color = "black")
      )

    output_path <- file.path(output_dir, sprintf("te_%s.pdf", Y_var))
    ggsave(output_path, plot = fig, height = 3, width = 3)
    cat(sprintf("Saved: %s\n", output_path))
  }
}

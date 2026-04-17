# =============================================================================
# Representation Figures
#
# Plots the efficient RSV representation H(R) against the simple representation
# P(Y=1 | R, S=o) using a binscatter with a cubic polynomial fit. The two
# representations coincide under a correctly specified model; deviations
# indicate where the efficient weighting adjusts the simple plug-in.
#
# Input:  data/clean/smartcard/empirical_representations.csv
# Output: figures/smartcard/empirical/{full,nospillover}/representations/
# =============================================================================

rm(list=ls())

suppressPackageStartupMessages({
  library(data.table)
  library(ggpubr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggtext)
  library(binsreg)
})
source("code/utils/fte_theme.R")

Y_vars <- c("Ycons", "Ylowinc", "Ymidinc")

all_weights <- fread("data/clean/smartcard/empirical_representations.csv")

# Produce figures for both the main sample and the no-spillover robustness check
for (sample in c("full", "nospillover")) {
output_dir <- file.path("figures/smartcard/empirical", sample, "representations")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (Y_var in Y_vars) {
  cat(sprintf("Processing %s [%s]...\n", Y_var, sample))

  # efficient: doubly-robust H(R) weights from the RSV estimator
  # simple: plug-in P(Y=1 | R, S=o) weights from the calibrate estimator
  df        <- all_weights %>% filter(.data$sample == .env$sample, .data$Y_var == .env$Y_var)
  efficient <- df$efficient
  simple    <- df$simple

  suppressWarnings({
    binscatter <- binsreg(
      x        = simple,
      y        = efficient,
      randcut  = 1,
      polyreg  = 3,
      ci       = c(3, 3),   # CIs around each binscatter dot
      dots     = TRUE
    )
  })

  plot_data <- binscatter$data.plot$`Group Full Sample`
  dots_df   <- plot_data$data.dots   # columns: x, fit
  ci_df     <- plot_data$data.ci     # columns: x, ci.l, ci.r  (from ci argument)
  poly_df   <- plot_data$data.poly

  ylim <- range(c(ci_df$ci.l, ci_df$ci.r, poly_df$fit), na.rm = TRUE)

  fig <- ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.5, color = palette$black, linetype = "dashed") +
    geom_line(data = poly_df,
              aes(x = x, y = fit),
              color = palette$black, linewidth = 0.6) +
    geom_point(aes(x = simple, y = efficient), color = palette$blue, size = 0.01, alpha = 0.5) +
    geom_point(data = dots_df,
               aes(x = x, y = fit),
               color = palette$black, size = 0.8) +
    labs(
      x = "Simple representation *P(Y=1 | R, S = o)*",
      y = "Efficient representation *H(R)*"
    ) +
    scale_x_continuous(limits = c(0, 1), minor_breaks = seq(0, 1, 0.05)) +
    scale_y_continuous(minor_breaks = seq(-2, 5, 0.2)) +
    coord_cartesian(ylim = ylim) +
    theme_bw() +
    theme(
      legend.position  = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      text             = element_text(color = palette$black, size = 14, family = font),
      axis.title.x     = element_markdown(color = palette$black, size = 14, family = font),
      axis.title.y     = element_markdown(color = palette$black, size = 14, family = font),
      axis.text        = element_text(color = palette$black, size = 12, family = font)
    )

  out_path <- file.path(output_dir, sprintf("representations_%s.pdf", Y_var))
  ggsave(out_path, plot = fig, height = 4, width = 4.6)
  cat(sprintf("Saved: %s\n", out_path))
}
}

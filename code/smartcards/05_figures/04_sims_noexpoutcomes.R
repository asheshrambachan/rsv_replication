# =============================================================================
# Simulation Figures — No Experimental Outcomes
#
# Plots normalized bias and coverage vs tau for RSV and the calibrate
# benchmark, across outcomes and observational sample sizes (n_o).
#
# Input:  data/interim/smartcards/sims_noexpoutcomes/summary_stats_{estimator}.Rds
# Output: figures/smartcards/sims_noexpoutcomes/{outcome}/
# =============================================================================

library(data.table)
library(dplyr)
library(ggplot2)
source("code/utils/fte_theme.R")

cat("=== Simulation Figures — No Experimental Outcomes ===\n\n")

OUTCOMES <- c("Ycons", "Ylowinc", "Ymidinc")

# ==============================================================================
# Load summarized results
# ==============================================================================

all_results <- fread("data/clean/smartcards/sims_noexpoutcomes.csv")

# ==============================================================================
# Theme
# ==============================================================================
guide <- data.frame(
  breaks = c("RSV", "Common practice"),
  labels = c("RSV", "Common practice"),
  colors = c(palette$darkblue, palette$red),
  shapes = c(19, 23)
)

# ==============================================================================
# Figures
# ==============================================================================
n_o_vals <- sort(unique(all_results$n_o))

outcome_snake <- c(Ycons = "y_cons", Ylowinc = "y_lowinc", Ymidinc = "y_midinc")

for (outcome in OUTCOMES) {
  outcome_lc <- outcome_snake[[outcome]]
  figures_dir <- file.path("figures/smartcards/sims_noexpoutcomes", outcome_lc)
  if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

  df_outcome <- all_results %>% filter(.data$outcome == .env$outcome)

  y_max <- max(df_outcome$normalized_bias)
  y_min <- min(df_outcome$normalized_bias)

  for (n_o in n_o_vals) {
    df <- df_outcome %>% filter(.data$n_o == .env$n_o)

    if (nrow(df) == 0) next

    # Normalized bias
    fig_bias <- ggplot(df, aes(x = tau, y = normalized_bias,
                               group = estimator, color = estimator,
                               fill = estimator, shape = estimator)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_line(linewidth = 0.65) +
      geom_point(size = 2.5) +
      labs(
        x = latex2exp::TeX("Treatment effect $\\tau$"),
        y = if (n_o==n_o_vals[1]) "Normalized bias" else ""
      ) +
      scale_y_continuous(limits = c(y_min, y_max)) +
      bias_coverage_theme(guide, c(0.05, 0.05))

    if (n_o!=n_o_vals[1])
      fig_bias <- fig_bias + theme(legend.position = "none")

    out_path <- file.path(figures_dir, sprintf("bias_%s_no%d.pdf", outcome_lc, n_o))
    ggsave(plot = fig_bias, filename = out_path, width = 3.5, height = 3)
    cat("Saved:", out_path, "\n")

    # Coverage
    fig_coverage <- ggplot(df, aes(x = tau, y = coverage,
                                   group = estimator, color = estimator,
                                   fill = estimator, shape = estimator)) +
      geom_hline(yintercept = 0.95, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_line(linewidth = 0.65) +
      geom_point(size = 2.5) +
      labs(
        x = latex2exp::TeX("Treatment effect $\\tau$"),
        y = if (n_o==n_o_vals[1]) "Coverage" else ""
      ) +
      scale_y_continuous(limits = c(0, 1)) +
      bias_coverage_theme(guide, c(0.05, 0.1))

    if (n_o!=n_o_vals[1])
      fig_coverage <- fig_coverage + theme(legend.position = "none")

    out_path <- file.path(figures_dir, sprintf("coverage_%s_no%d.pdf", outcome_lc, n_o))
    ggsave(plot = fig_coverage, filename = out_path, width = 3.5, height = 3)
    cat("Saved:", out_path, "\n")
  }
}

cat("\nFigures saved to: figures/smartcards/sims_noexpoutcomes/{outcome}/\n")

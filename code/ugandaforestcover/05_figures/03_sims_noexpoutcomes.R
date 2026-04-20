library(data.table)
library(dplyr)
library(ggplot2)
source("code/utils/fte_theme.R")

cat("=== No-Labels Simulation Figures ===\n\n")

outcome <- "Ybin"

# ==============================================================================
# Load summarized results
# ==============================================================================

all_results <- fread("data/clean/ugandaforestcover/sims_noexpoutcomes.csv") %>%
  mutate(alpha_o_id = factor(alpha_o_id,
    levels = c("02km", "05km", "10km"),
    labels = c("0 - 2 km", "0 - 5 km", "0 - 10 km")
  )) %>%
  filter(estimator != "PTE")

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
figures_dir <- file.path("figures/ugandaforestcover", paste0("sims_noexpoutcomes_", outcome))
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

n_o_vals      <- sort(unique(all_results$n_o))
alpha_o_vals  <- levels(all_results$alpha_o_id)

for (alpha_o in alpha_o_vals) {
  df <- all_results %>%
    filter(.data$outcome == .env$outcome,
           .data$alpha_o_id == .env$alpha_o)

  y_max  <- max(df$normalized_bias)
  y_min  <- min(df$normalized_bias)

  for (n_o in n_o_vals) {
    df <- all_results %>%
      filter(.data$outcome == .env$outcome,
             .data$alpha_o_id == .env$alpha_o,
             .data$n_o     == .env$n_o)

    if (nrow(df) == 0) next

    slug <- c("0 - 2 km" = "02km", "0 - 5 km" = "05km", "0 - 10 km" = "10km")[[alpha_o]]

    # Normalized bias
    fig_bias <- ggplot(df, aes(x = tau, y = normalized_bias,
                                group = estimator, color = estimator,
                                fill = estimator, shape = estimator)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_line(linewidth = 0.65) +
      geom_point(size = 2.5) +
      labs(
        x = latex2exp::TeX("Treatment effect $\\tau$"),
        y = ifelse(n_o==n_o_vals[1], "Normalized bias", "")
      ) +
      scale_y_continuous(limits = c(y_min, y_max)) +
      bias_coverage_theme(guide, legend.position.inside = c(0.05, 0.05))

    if (n_o!=n_o_vals[1])
      fig_bias <- fig_bias + theme(legend.position = "none")

    out_path <- file.path(figures_dir, sprintf("bias_%s_no%04d.jpeg", slug, n_o))
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
        y = ifelse(n_o==n_o_vals[1], "Coverage", "")
      ) +
      scale_y_continuous(limits = c(0, 1)) +
      bias_coverage_theme(guide, legend.position.inside = c(0.05, 0.05))

    if (n_o!=n_o_vals[1])
      fig_coverage <- fig_coverage + theme(legend.position = "none")

    out_path <- file.path(figures_dir, sprintf("coverage_%s_no%04d.jpeg", slug, n_o))
    ggsave(plot = fig_coverage, filename = out_path, width = 3.5, height = 3)
    cat("Saved:", out_path, "\n")
  }
}

cat("\nFigures saved to:", figures_dir, "\n")

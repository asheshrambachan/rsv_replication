library(data.table)
library(dplyr)
library(ggplot2)
source("code/utils/fte_theme.R")

cat("=== Labels Simulation Figures ===\n\n")

outcome <- "Ybin"
alpha   <- 0.05

# ==============================================================================
# Load summarized results
# ==============================================================================

all_results <- fread("data/clean/ugandaforestcover/sims_expoutcomes.csv") %>%
  mutate(alpha_o_id = factor(alpha_o_id,
                             levels = c("02km", "05km", "10km"),
                             labels = c("0 - 2 km", "0 - 5 km", "0 - 10 km")
  ))

# ==============================================================================
# Theme
# ==============================================================================
guide <- data.frame(
  breaks = c("RSV", "PPIV", "PPIO", "Benchmark"),
  labels = c("RSV", "PPIV", "PPIO", "Benchmark"),
  colors = c(palette$darkblue, palette$red, palette$green, palette$black),
  shapes = c(19, 23, 22, 18)
)

# ==============================================================================
# Figures: bias and coverage
# ==============================================================================
figures_dir <- file.path("figures/ugandaforestcover", paste0("sims_expoutcomes_", outcome))
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

alpha_o_vals <- levels(all_results$alpha_o_id)
n_eo_vals    <- sort(unique(all_results$n_eo))

for (alpha_o in alpha_o_vals) {

  # shared y limits for bias across n_eo panels in this (outcome, alpha_o) slice
  df_slice <- all_results %>%
    filter(.data$outcome == .env$outcome, .data$alpha_o_id == .env$alpha_o)
  y_bias_min <- min(df_slice$normalized_bias, na.rm = TRUE)
  y_bias_max <- max(df_slice$normalized_bias, na.rm = TRUE)

  slug <- c("0 - 2 km" = "02km", "0 - 5 km" = "05km", "0 - 10 km" = "10km")[[alpha_o]]

  for (n_eo in n_eo_vals) {
    df <- df_slice %>% filter(.data$n_eo == .env$n_eo) %>% filter(estimator!="Benchmark")

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
        y = ifelse(n_eo == n_eo_vals[1], "Normalized bias", "")
      ) +
      scale_y_continuous(limits = c(y_bias_min, y_bias_max)) +
      bias_coverage_theme(guide, legend.position.inside = c(0.05, 0.3))

    if (n_eo != n_eo_vals[1])
      fig_bias <- fig_bias + theme(legend.position = "none")

    out_path <- file.path(figures_dir, sprintf("bias_%s_neo%d.jpeg", slug, n_eo))
    ggsave(plot = fig_bias, filename = out_path, width = 3, height = 3)
    cat("Saved:", out_path, "\n")

    # Coverage
    df <- df_slice %>% filter(.data$n_eo == .env$n_eo) %>% filter(estimator %in% c("RSV", "PPIV"))
    fig_coverage <- ggplot(df, aes(x = tau, y = coverage,
                                    group = estimator, color = estimator,
                                    fill = estimator, shape = estimator)) +
      geom_hline(yintercept = 1 - alpha, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_line(linewidth = 0.65) +
      geom_point(size = 2.5) +
      labs(
        x = latex2exp::TeX("Treatment effect $\\tau$"),
        y = ifelse(n_eo == n_eo_vals[1], "Coverage", "")
      ) +
      scale_y_continuous(limits = c(0, 1)) +
      bias_coverage_theme(guide, legend.position.inside = c(0.05, 0.05))

    if (n_eo != n_eo_vals[1])
      fig_coverage <- fig_coverage + theme(legend.position = "none")

    out_path <- file.path(figures_dir, sprintf("coverage_%s_neo%d.jpeg", slug, n_eo))
    ggsave(plot = fig_coverage, filename = out_path, width = 3.5, height = 3)
    cat("Saved:", out_path, "\n")
  }
}

# ==============================================================================
# Figures: relative SE vs RSV
# ==============================================================================
guide_relse <- guide %>% filter(breaks != "RSV")

join_cols <- c("outcome", "tau", "n_e", "n_eo", "n_o", "alpha_o_id")
relse_df <- left_join(
  all_results %>% filter(estimator!="RSV") %>% select(all_of(c(join_cols, "estimator", "avg_se"))),
  all_results %>% filter(estimator=="RSV") %>% select(all_of(c(join_cols, "avg_se"))) %>% rename(avg_se_rsv = avg_se),
  by = join_cols
) %>%
  mutate(rel_se = 1 - avg_se_rsv / avg_se)

for (alpha_o in alpha_o_vals) {
  slug <- c("0 - 2 km" = "02km", "0 - 5 km" = "05km", "0 - 10 km" = "10km")[[alpha_o]]

  df_slice <- relse_df %>%
    filter(.data$outcome == .env$outcome, .data$alpha_o_id == .env$alpha_o)
  y_min <- min(df_slice$rel_se * 100, na.rm = TRUE) # *5
  y_max <- max(df_slice$rel_se * 100, na.rm = TRUE)

  for (n_eo in n_eo_vals) {
    df <- df_slice %>% filter(.data$n_eo == .env$n_eo) %>% filter(estimator!="PPIO")
    if (nrow(df) == 0) next

    fig_relse <- ggplot(df, aes(x = tau, y = rel_se * 100,
                                 group = estimator, color = estimator,
                                 fill = estimator, shape = estimator)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_line(linewidth = 0.65) +
      geom_point(size = 2.5) +
      labs(
        x = latex2exp::TeX("Treatment effect $\\tau$"),
        y = ifelse(n_eo == n_eo_vals[1], "Relative Reduction in SE (%)", "")
      ) +
      scale_y_continuous(limits = c(y_min, y_max)) +
      bias_coverage_theme(guide_relse, legend.position.inside = c(0.05, 0.05))

    if (n_eo != n_eo_vals[1])
      fig_relse <- fig_relse + theme(legend.position = "none")

    out_path <- file.path(figures_dir, sprintf("rel_se_%s_neo%d.jpeg", slug, n_eo))
    ggsave(plot = fig_relse, filename = out_path, width = 3.5, height = 3)
    cat("Saved:", out_path, "\n")
  }
}

cat("\nFigures saved to:", figures_dir, "\n")

# =============================================================================
# Forest — Assumption: Stability: Density Figures
#
# Visualises f(R | S_e=1, Y=y) vs f(R | S_o=1, Y=y) for each observational
# distance band and level of Y. Supports the stability assumption: if the
# sensing mechanism f(R | X, Y) is the same across the experimental and
# observational samples, the two densities should overlap within each Y class.
#
# R = E[Y|X] = sum_k P(Y=k|X) * k, computed from the random forest class
# probability predictions produced by 01_data_construction/07_rf_predict.R.
#
# Input:  data/clean/ugandaforestcover/data_Ybin.csv
# Output: figures/ugandaforestcover/stability/
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ggtext)
})
source("code/utils/fte_theme.R")

# -----------------------------------------------------------------------------
# 1. Plot Helper
# -----------------------------------------------------------------------------

make_density_plot <- function(density_dt) {

  group_colors <- c(
    "Experimental"  = palette$darkblue,
    "Observational" = palette$green
  )

  group_linetypes <- c(
    "Experimental"  = "solid",
    "Observational" = "dashed"
  )

  y_val <- unique(density_dt$Y)
  group_labels <- c(
    "Experimental"  = paste0("Experimental: *D=0, Y=",  y_val, "*"),
    "Observational" = paste0("Observational: *D=0, Y=", y_val, "*")
  )

  y_max <- max(tapply(density_dt$R, density_dt$S, function(x) max(density(x)$y))) * 1.3
  y_maj <- scales::breaks_pretty(n = 4)(c(0, y_max))
  y_min <- seq(0, y_max, by = (y_maj[2] - y_maj[1]) / 6)

  ggplot(density_dt, aes(x = R, fill = S, color = S, linetype = S)) +
    geom_density(linewidth = 0.5, alpha = 0.2, position = "identity") +
    labs(x = "*R*", y = "Density") +
    scale_x_continuous(minor_breaks = seq(0, 1, by = 0.05), limits = c(0, 1)) +
    scale_y_continuous(breaks = y_maj, minor_breaks = y_min, limits = c(0, y_max)) +
    scale_color_manual(name = NULL, values = group_colors, labels = group_labels) +
    scale_fill_manual( name = NULL, values = group_colors, labels = group_labels) +
    scale_linetype_manual(name = NULL, values = group_linetypes, labels = group_labels) +
    theme_bw() +
    theme(
      text                   = element_text(size = 14, family = font),
      legend.text            = element_markdown(size = 14, family = font),
      axis.title.x           = element_markdown(size = 14, family = font),
      axis.title.y           = element_text(size = 14, family = font),
      axis.text              = element_markdown(size = 12),
      legend.position        = "inside",
      legend.justification   = c(0, 1),
      legend.position.inside = c(0.04, 0.95),
      legend.key.spacing.y   = unit(0.2, "cm"),
      legend.key.size        = unit(0.5, "cm"),
      legend.background      = element_blank(),
      panel.grid.major.x     = element_blank(),
      panel.grid.minor.x     = element_blank(),
      panel.grid.major.y     = element_line(linewidth = 0.3),
      panel.grid.minor.y     = element_line(linewidth = 0.1),
      axis.ticks             = element_line(linewidth = 0.3)
    )
}

# -----------------------------------------------------------------------------
# 2. Load Data and Build Scalar Predicted Score
#
# R = E[Y|X] = sum_k P(Y=k|X) * k, computed from the class probability
# columns (R0, R1, ...) produced by 07_rf_predict.R.
# -----------------------------------------------------------------------------

outcome <- "Ybin"

in_path <- file.path("data/clean/ugandaforestcover", paste0("data_", outcome, ".csv"))
DT      <- fread(in_path)

y_levels <- sort(unique(na.omit(DT$Y)))
r_cols   <- paste0("R", y_levels)
P        <- as.matrix(DT[, ..r_cols])
DT[, R  := as.numeric(P %*% as.numeric(y_levels))]

# -----------------------------------------------------------------------------
# 3. Generate and Save Figures
#
# One figure per (distance band, Y class): experimental sample (S_e=1) vs
# observational buffer band (S_o_*=1).
# -----------------------------------------------------------------------------

for (So_col in c("S_o_02km", "S_o_05km", "S_o_10km")) {
  so_slug <- sub("S_o_", "", So_col)   # e.g. "02km"

  for (y_val in y_levels) {
    plot_dt <- rbind(
      DT[S_e == 1         & Y == y_val, .(Y, R, S = "Experimental")],
      DT[get(So_col) == 1 & Y == y_val, .(Y, R, S = "Observational")]
    )
    plot_dt[, S := factor(S, levels = c("Experimental", "Observational"))]

    p <- make_density_plot(plot_dt)

    out_path <- file.path("figures/ugandaforestcover/stability", 
                          sprintf("stability_%s_%s_y%s.pdf", tolower(outcome), so_slug, y_val))
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggsave(out_path, plot = p, height = 4, width = 4.5)
    cat("Saved:", out_path, "\n")
  }
}

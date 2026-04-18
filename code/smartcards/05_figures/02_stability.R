# =============================================================================
# Assumption 2(i) — Stability: Density Figures
#
# Visualises f(R | S=e, D=d, Y=y) vs f(R | S=o, D=d, Y=y) for each
# treatment arm, outcome, and level of Y. Supports the stability assumption
# (S ⊥ R | X, D, Y): if the sensing mechanism is stable across samples, the
# experimental and observational densities should overlap within each (D, Y)
# cell. R is represented by its standardised first principal component.
#
# Input:  data/clean/smartcards/data.csv
# Output: figures/smartcards/stability/[y_cons|y_lowinc|y_midinc]/
# =============================================================================

rm(list = ls())

## Load libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(ggtext)
})
source("code/utils/fte_theme.R")

## Load required columns from the cleaned dataset
data <- read_csv("data/clean/smartcards/data.csv",
                 col_select = c(wave, D, R_PC1_scaled, Ycons, Ylowinc, Ymidinc)) %>%
  mutate(
    S = case_when(
      wave == "Experimental: Treated (2010)"   ~ "e",
      wave == "Experimental: Untreated (2011)" ~ "e",
      wave == "Experimental: Untreated (2012)" ~ "e",
      wave == "Observational (N/A)"            ~ "o"
    ),
    D = if_else(wave == "Experimental: Treated (2010)", 1L, 0L),
    R_PC1_scaled = -R_PC1_scaled,
  ) %>%
  tidyr::separate_rows(S, sep = ",") %>%
  mutate(
    group = paste0("D", D, "_", S)
  ) %>%
  select(D, S, group, R_PC1_scaled, Ycons, Ylowinc, Ymidinc)

## Generate and Save Plots
for (d in c(0,1)){
  for (y in c(0,1)){
    for (Y_var in c("Ycons","Ylowinc", "Ymidinc")){
      data_yd <- data %>%
        mutate(Y = !!sym(Y_var)) %>%
        filter(D==d, Y==y)

      # Define aesthetics
      group_colors <- c(
        D0_e = palette$blue,
        D0_o = palette$green,
        D1_e = palette$darkblue,
        D1_o = palette$darkgreen
      )

      group_linetypes <- c(
        D0_e = "solid",
        D0_o = "dashed",
        D1_e = "solid",
        D1_o = "dashed"
      )

      group_labels <- c(
        D0_e = paste0("Experimental: *D=0, Y=", y, "*"),
        D1_e = paste0("Experimental: *D=1, Y=", y, "*"),
        D0_o = paste0("Observational: *D=0, Y=", y, "*"),
        D1_o = paste0("Observational: *D=1, Y=", y, "*")
      )

      density <- ggplot(data_yd, aes(x = R_PC1_scaled,
                          color = group,
                          fill = group,
                          linetype = group)) +
        geom_density(alpha = 0.2, linewidth = 0.5) +
        labs(
          x = "First principal component of *R*",
          y = "Density"
        ) +
        scale_x_continuous(minor_breaks = seq(0, 1, by = 0.05), limits = c(-9, 2)) +
        scale_y_continuous(minor_breaks = seq(0, 1, by = 0.05), limits = c(0, 0.8)) +
        scale_color_manual(name = NULL, labels = group_labels, values = group_colors) +
        scale_fill_manual(name = NULL, labels = group_labels, values = group_colors) +
        scale_linetype_manual(name = NULL, labels = group_labels, values = group_linetypes) +
        theme_bw() +
        theme(
          text = element_text(size=14, family=font),
          legend.text = element_markdown(size=14, family=font),
          axis.text=element_markdown(size=12),

          axis.title.x = element_markdown(size = 14, family = font),
          axis.title.y = element_text(size = 14, family = font),

          legend.position = "inside",
          legend.justification = c(0, 1),
          legend.position.inside = c(0.04, 0.95),
          legend.key.spacing.y = unit(0.2, "cm"),
          legend.key.size = unit(0.5, "cm"),
          legend.background = element_blank(),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_line(linewidth=0.3),
          panel.grid.minor.y=element_line(linewidth=0.1),
          axis.ticks=element_line(linewidth=0.3),
        )

      # Save figure
      y_var_snake <- c(Ycons = "y_cons", Ylowinc = "y_lowinc", Ymidinc = "y_midinc")[[Y_var]]
      output_path <- sprintf("figures/smartcards/stability/%s/smartcard_r_dist_d%s_%s%s.pdf", y_var_snake, d, y_var_snake, y)
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
      ggsave(output_path, plot = density, height = 4, width = 4.5)
      cat(sprintf("Saved figure to: %s\n", output_path))
    }
  }
}

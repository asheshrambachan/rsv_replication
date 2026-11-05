# =============================================================================
# Shared plotting functions for stability conditional on binary secondary Y
#
# Mirrors 03_stability_S.R and 04_stability_Stilde.R, adding the secondary
# category and restricting to D=0 to match the corresponding stability tests.
# Supply the secondary column and any conversion options explicitly.
# Without a cutoff, the column must already be binary, as in the tests.
#
# Reads the matching S or Stilde secondary-outcome test results. Does not
# rerun tests or rebuild data. Checks the cutoff, reference count, cell counts,
# overlap counts and KS distances before writing any plots. Recomputing the
# percentile at full precision avoids rounding of the cutoff in the results CSV.
#
# Uses the existing colors, fonts and reflected PC1 display. Within each cell,
# both sample densities use a common bandwidth estimated from unique records.
#
# Entry points:
#   code/smartcards/05_figures/09_stability_S_secondary.R
#   code/smartcards/05_figures/10_stability_Stilde_secondary.R
#
# Output: figures/smartcards/stability_{S,Stilde}_secondary/SPEC/OUTCOME/
#         d0_y{0,1}_ytilde{0,1}.jpeg
# Sourcing defines functions only; it does not generate figures.
# =============================================================================

source("code/utils/stability_binary_secondary.R")
suppressPackageStartupMessages(source("code/utils/fte_theme.R"))

plot_stability_binary_secondary <- function(
    secondary_var, percentile = NULL, threshold = NULL,
    direction = "above", input_csv = "data/clean/smartcards/data.csv",
    results_csv = NULL,
    output_dir = NULL, sample_definition = c("S", "Stilde")) {
  if (missing(secondary_var) || !is.character(secondary_var) ||
      length(secondary_var) != 1L || is.na(secondary_var) || !nzchar(secondary_var)) {
    stop("Supply the name of the secondary-outcome column.")
  }
  if (!is.null(threshold) && !is.null(percentile)) {
    stop("Specify either threshold or percentile, not both.")
  }
  sample_definition <- match.arg(sample_definition)
  if (is.null(output_dir)) {
    output_dir <- paste0("figures/smartcards/stability_", sample_definition, "_secondary")
  }
  tag <- if (!is.null(percentile)) {
    paste("percentile", percentile, direction, sep = "_")
  } else if (!is.null(threshold)) {
    paste("threshold", threshold, direction, sep = "_")
  } else ""
  spec_name <- gsub("[^[:alnum:]_-]", "_", secondary_var)
  if (nzchar(tag)) spec_name <- paste0(spec_name, "_", gsub("[^[:alnum:]_-]", "_", tag))
  if (is.null(results_csv)) {
    results_csv <- paste0(
      "data/clean/smartcards/assumption_stability_", sample_definition,
      "_secondary_", spec_name, ".csv"
    )
    # Keep previously generated combined results usable until tests are rerun.
    if (!file.exists(results_csv)) {
      legacy_csv <- paste0(
        "data/clean/smartcards/assumption_stability_binary_secondary_", spec_name, ".csv"
      )
      if (file.exists(legacy_csv)) results_csv <- legacy_csv
    }
  }
  if (!file.exists(results_csv)) stop("Run the matching stability test first: ", results_csv)
  results <- readr::read_csv(results_csv, show_col_types = FALSE, progress = FALSE)
  required_results <- c(
    "sample_definition", "outcome", "Y", "secondary", "secondary_value",
    "secondary_method", "secondary_cutoff", "secondary_percentile",
    "secondary_direction", "secondary_reference_n", "n_exp", "n_obs",
    "n_overlap", "ks_stat", "cluster_pval", "status"
  )
  if (!all(required_results %in% names(results))) stop("Test results lack required metadata.")
  results <- results[which(results$sample_definition == sample_definition), ]
  if (!nrow(results) || anyNA(results$secondary) || any(results$secondary != secondary_var)) {
    stop("Test results do not match the selected secondary measurement.")
  }
  if (anyNA(results$sample_definition) ||
      any(!results$sample_definition %in% c("S", "Stilde")) ||
      anyNA(results$Y) || any(!results$Y %in% c(0, 1)) ||
      anyNA(results$secondary_value) || any(!results$secondary_value %in% c(0, 1))) {
    stop("Invalid sample definitions or category labels in the test results.")
  }
  keys <- results[c("sample_definition", "outcome", "Y", "secondary_value")]
  if (anyDuplicated(keys)) stop("Test results contain duplicate comparison cells.")

  Y_vars <- unique(results$outcome)
  required <- unique(c("shrid2", "wave", "D", "R_PC1_scaled", Y_vars, secondary_var))
  data <- readr::read_csv(
    input_csv, col_select = tidyselect::all_of(required),
    show_col_types = FALSE, progress = FALSE
  )
  if (anyNA(data$shrid2) || anyDuplicated(data$shrid2)) {
    stop("Expected one record per village before expanding sample roles.")
  }
  secondary <- dichotomize_secondary(data[[secondary_var]], threshold, percentile, direction)
  equal_number <- function(x, y) {
    if (length(y) != 1L) return(FALSE)
    if (is.na(y)) return(all(is.na(x)))
    !anyNA(x) && all(abs(x - y) <= 1e-12 * max(1, abs(y)))
  }
  if (anyNA(results$secondary_method) || any(results$secondary_method != secondary$method) ||
      !equal_number(results$secondary_cutoff, secondary$cutoff) ||
      !equal_number(results$secondary_percentile, secondary$percentile) ||
      !equal_number(results$secondary_reference_n, secondary$reference_n) ||
      !identical(as.character(unique(results$secondary_direction)), secondary$direction)) {
    stop("Current data or conversion settings do not match the saved test specification.")
  }
  data$.secondary_binary <- secondary$values
  study_waves <- c("Experimental: Treated (2010)",
                   "Experimental: Untreated (2011)",
                   "Experimental: Untreated (2012)")
  obs_wave <- "Observational (N/A)"
  buffer_wave <- "Experimental: Untreated (2011)"
  eligible <- !is.na(data$D) & data$D == 0 & data$wave %in% c(study_waves, obs_wave)
  data <- data[eligible, ]

  # Validate every comparison before creating output directories or figures.
  cells <- vector("list", nrow(results))
  for (i in seq_len(nrow(results))) {
    row <- results[i, ]
    keep <- !is.na(data[[row$outcome]]) & data[[row$outcome]] == row$Y &
      !is.na(data$.secondary_binary) & data$.secondary_binary == row$secondary_value &
      is.finite(data$R_PC1_scaled)
    cell <- data[which(keep), ]
    e <- cell$wave %in% study_waves
    o <- cell$wave == obs_wave |
      (row$sample_definition == "Stilde" & cell$wave == buffer_wave)
    if (sum(e) != row$n_exp || sum(o) != row$n_obs || sum(e & o) != row$n_overlap) {
      stop("Saved sample counts do not match current data for comparison ", i, ".")
    }
    if (sum(e) && sum(o)) {
      grid <- sort(unique(cell$R_PC1_scaled))
      distance <- max(abs(
        findInterval(grid, sort(cell$R_PC1_scaled[e])) / sum(e) -
          findInterval(grid, sort(cell$R_PC1_scaled[o])) / sum(o)
      ))
      if (!equal_number(row$ks_stat, distance)) {
        stop("Saved KS statistic does not match current data for comparison ", i, ".")
      }
    }
    cells[[i]] <- list(data = cell, e = e, o = o)
  }

  complement <- c(above = "<=", "at-or-above" = "<", below = ">=", "at-or-below" = ">")
  operators <- c(above = ">", "at-or-above" = ">=", below = "<", "at-or-below" = "<=")
  paths <- character()
  for (i in seq_len(nrow(results))) {
    row <- results[i, ]
    cell <- cells[[i]]$data
    e <- cells[[i]]$e
    o <- cells[[i]]$o
    if (sum(e) < 2L || sum(o) < 2L || length(unique(cell$R_PC1_scaled)) < 2L) {
      warning("Density not drawn for comparison ", i, ": insufficient score variation or sample size.")
      next
    }
    # Reflect PC1 only for display, matching the original density scripts.
    plotting <- rbind(
      data.frame(score = -cell$R_PC1_scaled[e], group = "e"),
      data.frame(score = -cell$R_PC1_scaled[o], group = "o")
    )
    plotting$group <- factor(plotting$group, levels = c("e", "o"))
    bandwidth <- stats::bw.nrd0(-cell$R_PC1_scaled)
    group_names <- if (row$sample_definition == "S") {
      c(e = "Experimental", o = "Observational")
    } else {
      c(e = "Experimental and validation", o = "Observational and validation")
    }
    group_labels <- as.expression(lapply(group_names, function(label) {
      bquote(paste(.(label), ": ", D, "=0, ", Y, "=", .(row$Y), ", ",
                   widetilde(Y), "=", .(row$secondary_value)))
    }))
    condition <- if (secondary$method == "binary_input") {
      paste0(secondary_var, " = ", row$secondary_value)
    } else {
      op <- if (row$secondary_value == 1) operators[[direction]] else complement[[direction]]
      paste(secondary_var, op, format(secondary$cutoff, digits = 8, trim = TRUE))
    }
    p_label <- if (is.na(row$cluster_pval)) {
      paste0("p not estimated (", row$status, ")")
    } else paste0("Mandal-bootstrap p = ", formatC(row$cluster_pval, digits = 3, format = "f"))
    if (row$status == "few_clusters") p_label <- paste0(p_label, " [few mandals]")
    caption <- paste0(
      row$sample_definition, " design | ", p_label, "\n",
      "Experimental n = ", row$n_exp, "; observational n = ", row$n_obs,
      "; overlap = ", row$n_overlap
    )

    density <- ggplot2::ggplot(plotting, ggplot2::aes(
      x = score, color = group, fill = group, linetype = group
    )) +
      ggplot2::geom_density(alpha = 0.2, linewidth = 0.5, bw = bandwidth) +
      ggplot2::labs(
        title = bquote(paste(.(row$outcome), ": ", Y, " = ", .(row$Y), ", ",
                             widetilde(Y), " = ", .(row$secondary_value))),
        subtitle = condition,
        x = "First principal component of *R*", y = "Density", caption = caption
      ) +
      ggplot2::scale_color_manual(name = NULL, breaks = c("e", "o"),
                                 labels = group_labels, values = c(e = palette$blue, o = palette$green)) +
      ggplot2::scale_fill_manual(name = NULL, breaks = c("e", "o"),
                                labels = group_labels, values = c(e = palette$blue, o = palette$green)) +
      ggplot2::scale_linetype_manual(name = NULL, breaks = c("e", "o"),
                                    labels = group_labels, values = c(e = "solid", o = "dashed")) +
      # Zoom after density estimation so observations outside the view are retained.
      ggplot2::coord_cartesian(xlim = c(-9, 2)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 14, family = font),
        plot.title = ggplot2::element_text(size = 14),
        plot.subtitle = ggplot2::element_text(size = 10),
        plot.caption = ggplot2::element_text(size = 9, hjust = 0),
        legend.text = ggplot2::element_text(size = 10, family = font),
        axis.text = ggtext::element_markdown(size = 12),
        axis.title.x = ggtext::element_markdown(size = 14, family = font),
        legend.position = "inside", legend.justification = c(0, 1),
        legend.position.inside = c(0.04, 0.95),
        legend.background = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(linewidth = 0.3),
        panel.grid.minor.y = ggplot2::element_line(linewidth = 0.1),
        axis.ticks = ggplot2::element_line(linewidth = 0.3)
      )
    path <- file.path(output_dir, spec_name, row$outcome,
                      sprintf("d0_y%s_ytilde%s.jpeg", row$Y, row$secondary_value))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_jpeg else "jpeg"
    ggplot2::ggsave(path, plot = density, device = device,
                    height = 4.5, width = 5.5, dpi = 300, bg = "white")
    message("Saved: ", path)
    paths <- c(paths, path)
  }
  invisible(paths)
}

run_stability_secondary_figures_cli <- function(plotter, args = commandArgs(trailingOnly = TRUE)) {
  options <- parse_binary_stability_args(args)
  if (!is.null(options$help)) {
    cat("Usage: Rscript SCRIPT COLUMN [--percentile P | --threshold VALUE] [--direction RULE]\n")
  } else {
    do.call(plotter,
            options[c("secondary_var", "threshold", "percentile", "direction")])
  }
}

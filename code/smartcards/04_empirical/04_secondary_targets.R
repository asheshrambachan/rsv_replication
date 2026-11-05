# Construct and save q and its frozen centering state (issue #8).
# Does not fit g or adjust/calibrate H. Existing clean data and fits are untouched.
#
# Binary:
# Rscript code/smartcards/04_empirical/04_secondary_targets.R \
#   ent_own_share --threshold 0 --direction above
# Continuous, without dichotomizing:
# Rscript code/smartcards/04_empirical/04_secondary_targets.R \
#   ent_own_share --basis polynomial --degree 2
# Finite joint categories (use columns with finite categorical support):
# Rscript code/smartcards/04_empirical/04_secondary_targets.R \
#   CATEGORY_A,CATEGORY_B --basis joint
#
# A column is required. The default basis requires a binary column or an
# explicit cutoff. Continuous bases establish selected-moment restrictions;
# full conditional calibration also requires the appendix span assumption.
# Joint indicators omit one reference category per Y group. Population support
# coverage is not established by an observed category table.
# No X, whitening, or cell-proportion scaling is used.
# Run separately; this prototype is not part of 00_run_all.R.
# Output: data/interim/smartcards/secondary_targets/SPEC/{S,Stilde}/OUTCOME.Rds
#         data/interim/smartcards/secondary_targets/SPEC/diagnostics.csv

source("R/secondary_calibration.R")
source("code/utils/stability_binary_secondary.R")

parse_smartcards_secondary_target_args <- function(args) {
  usage <- paste(
    "Usage: Rscript 04_secondary_targets.R COLUMN[,COLUMN...]",
    "[--threshold VALUE | --percentile P] [--direction RULE]",
    "[--basis binary|polynomial|joint] [--degree K]"
  )
  if (any(args %in% c("--help", "-h"))) return(list(help = usage))
  remaining <- character()
  extra <- list()
  i <- 1L
  while (i <= length(args)) {
    if (args[i] %in% c("--basis", "--degree")) {
      key <- substring(args[i], 3L)
      if (!is.null(extra[[key]])) stop("Repeated option: --", key)
      if (i == length(args) || startsWith(args[i + 1L], "--")) {
        stop("Missing value for ", args[i])
      }
      extra[[key]] <- args[i + 1L]
      i <- i + 2L
    } else {
      remaining <- c(remaining, args[i])
      i <- i + 1L
    }
  }
  if (!length(remaining)) stop(usage, call. = FALSE)
  options <- parse_binary_stability_args(remaining, "04_secondary_targets.R")
  mode <- if (is.null(extra$basis)) "binary" else extra$basis
  if (!mode %in% c("binary", "polynomial", "joint")) stop("--basis must be binary, polynomial, or joint.")
  columns <- trimws(strsplit(options$secondary_var, ",", fixed = TRUE)[[1]])
  if (!length(columns) || any(!nzchar(columns)) || anyDuplicated(columns)) {
    stop("Supply distinct, nonempty secondary column names.")
  }
  if (mode == "binary" && length(columns) != 1L) stop("The binary basis requires one column.")
  if (mode != "binary" &&
      (!is.null(options$threshold) || !is.null(options$percentile) ||
       "--direction" %in% remaining)) {
    stop("Polynomial/joint bases use original measurements; do not supply dichotomization options.")
  }
  if (mode != "polynomial" && !is.null(extra$degree)) stop("--degree requires --basis polynomial.")
  if (mode == "polynomial" && is.null(extra$degree)) stop("Specify --degree for a polynomial basis.")
  options["basis"] <- list(switch(mode,
    binary = NULL,
    polynomial = secondary_polynomial_basis(suppressWarnings(as.numeric(extra$degree))),
    joint = secondary_joint_basis()
  ))
  options$basis_mode <- mode
  options$secondary_vars <- columns
  options
}

run_smartcards_secondary_targets <- function(
    args = commandArgs(trailingOnly = TRUE),
    input_csv = "data/clean/smartcards/data.csv",
    output_dir = "data/interim/smartcards/secondary_targets") {
  options <- parse_smartcards_secondary_target_args(args)
  if (!is.null(options$help)) {
    cat(options$help, "\n")
    return(invisible(NULL))
  }
  Y_vars <- c("Ycons", "Ylowinc", "Ymidinc")
  required <- unique(c("shrid2", "clusters", "wave", Y_vars, options$secondary_vars))
  data <- readr::read_csv(
    input_csv, col_select = tidyselect::all_of(required),
    show_col_types = FALSE, progress = FALSE
  )
  if (anyNA(data$shrid2) || anyDuplicated(data$shrid2)) stop("Expected one record per shrid2.")
  if (options$basis_mode == "binary") {
    conversion <- dichotomize_secondary(
      data[[options$secondary_var]], options$threshold, options$percentile, options$direction
    )
    measurements <- conversion$values
    conversion <- conversion[setdiff(names(conversion), "values")]
    tag <- if (!is.null(options$percentile)) {
      paste("percentile", options$percentile, options$direction, sep = "_")
    } else if (!is.null(options$threshold)) {
      paste("threshold", options$threshold, options$direction, sep = "_")
    } else ""
  } else {
    measurements <- data[options$secondary_vars]
    conversion <- list(method = options$basis_mode, columns = options$secondary_vars,
                       degree = options$basis$degree)
    tag <- if (options$basis_mode == "polynomial") {
      paste0("polynomial_degree_", options$basis$degree)
    } else "joint_categories"
  }
  spec <- gsub("[^[:alnum:]_-]", "_", paste(options$secondary_vars, collapse = "__"))
  if (nzchar(tag)) spec <- paste0(spec, "_", gsub("[^[:alnum:]_-]", "_", tag))
  root <- file.path(output_dir, spec)
  study <- c("Experimental: Treated (2010)", "Experimental: Untreated (2011)",
             "Experimental: Untreated (2012)")
  S_e <- data$wave %in% study
  # Validate every specification before writing any of the outputs.
  outputs <- list()
  diagnostics <- list()
  for (design in c("S", "Stilde")) {
    S_o <- data$wave %in% c(
      "Observational (N/A)", if (design == "Stilde") "Experimental: Untreated (2011)"
    )
    secondary <- measurements
    if (is.data.frame(secondary)) {
      for (v in names(secondary)) secondary[[v]][!S_o] <- NA
    } else secondary[!S_o] <- NA
    for (v in Y_vars) {
      Y <- data[[v]]
      Y[!S_o] <- NA
      state <- fit_secondary_targets(Y, secondary, S_o, S_e, basis = options$basis)
      q <- apply_secondary_targets(state, Y, S_o, secondary, S_e)
      path <- file.path(root, design, paste0(v, ".Rds"))
      outputs[[path]] <- list(
        shrid2 = data$shrid2, clusters = data$clusters,
        sample_definition = design, outcome = v, S_e = S_e, S_o = S_o,
        Y = Y, secondary = secondary, q = q, state = state, conversion = conversion
      )
      diag <- state$diagnostics
      diag$sample_definition <- design
      diag$outcome <- v
      means <- colMeans(q)
      diag$mean_q <- vapply(state$groups, function(g) {
        if (length(g$target_columns) == 1L) means[g$target_columns] else NA_real_
      }, numeric(1))
      diag$max_abs_mean_q <- vapply(state$groups, function(g) {
        if (length(g$target_columns)) max(abs(means[g$target_columns])) else 0
      }, numeric(1))
      diag$n_experimental_only <- sum(S_e & !S_o)
      diag$n_observational_only <- sum(S_o & !S_e)
      diag$n_overlap <- sum(S_o & S_e)
      diagnostics[[length(diagnostics) + 1L]] <- diag
    }
  }
  for (path in names(outputs)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(outputs[[path]], path)
    message("Saved: ", path)
  }
  diagnostics <- do.call(rbind, diagnostics)
  utils::write.csv(diagnostics, file.path(root, "diagnostics.csv"), row.names = FALSE)
  print(diagnostics, row.names = FALSE)
  message("Constructed q only. Full conditional calibration has not been established.")
  invisible(list(paths = names(outputs), diagnostics = diagnostics))
}

if (sys.nframe() == 0L) run_smartcards_secondary_targets()

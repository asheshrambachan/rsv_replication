# =============================================================================
# Shared functions -- Stability conditional on a binary secondary outcome
#
# Tests equality of the R_PC1_scaled distributions across sample roles,
# conditional on D=0, primary Y, and binary secondary outcome T.
#
# Uses the same sample definitions as 02_stability_S.R and
# 03_stability_Stilde.R. Buffer villages (2011) belong to both roles under
# Stilde and remain single records when calculating cluster contributions.
#
# The statistic is the two-sample KS distance within each (Y, T) cell.
# P-values use a centered mandal-level multiplier bootstrap, rather than
# the independent-observation p-values returned by stats::ks.test().
# Inference is asymptotic and requires independent mandals, no dominating
# cluster, and adequate support in both sample roles. Failure to reject
# does not establish stability. This diagnostic concerns PC1, not all R.
# Missing measurements are excluded only from the diagnostic and reported.
#
# Run from the project root, supplying the column name:
#   Rscript code/smartcards/02_assumption_tests/04_stability_S_secondary.R COLUMN
#   Rscript code/smartcards/02_assumption_tests/05_stability_Stilde_secondary.R COLUMN
#   ... COLUMN --percentile 75 --direction above --B 2000 --seed 42
#   ... COLUMN --threshold 0.05 --direction at-or-above
# Without a cutoff the column must already be binary. Legacy positional
# bootstrap-draw and seed arguments are also supported.
#
# Percentiles use all nonmissing input-column values, computed once before
# sample-role and Y splits (R quantile type 7). Missing values stay missing.
# Ties can make the resulting group proportions differ from the percentile.
# Bootstrap draws hold the chosen cutoff fixed; they do not account for
# estimating a population quantile. Tests condition on the resulting binary
# indicator, not on the original continuous measurement.
# --warn-clusters controls a reporting warning only (default 10; 0 disables).
#
# Or source this file and call run_stability_binary_secondary("COLUMN").
# Sourcing defines functions only; it does not run tests or write files.
# =============================================================================

source("code/utils/clustered_ks.R")

dichotomize_secondary <- function(x, threshold = NULL, percentile = NULL,
                                 direction = c("above", "at-or-above", "below",
                                               "at-or-below")) {
  direction <- match.arg(direction)
  if (!(is.numeric(x) || is.logical(x))) stop("The secondary column must be numeric.")
  if (any(!is.na(x) & !is.finite(x))) {
    stop("The secondary column must contain finite values or missing values.")
  }
  if (!is.null(threshold) && !is.null(percentile)) {
    stop("Specify either threshold or percentile, not both.")
  }
  reference <- x[!is.na(x)]
  cutoff <- NA_real_
  method <- "binary_input"
  if (!is.null(percentile)) {
    if (!is.numeric(percentile) || length(percentile) != 1L ||
        !is.finite(percentile) || percentile < 0 || percentile > 100) {
      stop("percentile must be a number between 0 and 100.")
    }
    if (!length(reference)) stop("Cannot estimate a percentile from an all-missing column.")
    cutoff <- as.numeric(stats::quantile(reference, percentile / 100, type = 7,
                                         names = FALSE))
    method <- "percentile"
  } else if (!is.null(threshold)) {
    if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold)) {
      stop("threshold must be a single finite number in the original units.")
    }
    cutoff <- threshold
    method <- "fixed_threshold"
  }

  binary <- rep(NA_integer_, length(x))
  observed <- !is.na(x)
  if (method == "binary_input") {
    if (any(!reference %in% c(0, 1))) {
      stop("The secondary column must be 0/1, or specify --threshold or --percentile.")
    }
    if (direction != "above") stop("direction requires a threshold or percentile.")
    binary[observed] <- as.integer(reference)
  } else {
    binary[observed] <- as.integer(switch(
      direction,
      above = reference > cutoff,
      "at-or-above" = reference >= cutoff,
      below = reference < cutoff,
      "at-or-below" = reference <= cutoff
    ))
  }
  list(values = binary, method = method, cutoff = cutoff,
       percentile = if (is.null(percentile)) NA_real_ else percentile,
       direction = if (method == "binary_input") NA_character_ else direction,
       reference_n = length(reference),
       n_at_cutoff = if (method == "binary_input") NA_integer_ else sum(reference == cutoff))
}

run_stability_binary_secondary <- function(
    secondary_var,
    input_csv = "data/clean/smartcards/data.csv",
    Y_vars = c("Ycons", "Ylowinc", "Ymidinc"),
    sample_definitions = c("S", "Stilde"),
    B = 2000L, seed = 42L, warn_clusters = 10L,
    output_csv = NULL, data = NULL,
    threshold = NULL, percentile = NULL, direction = "above") {
  if (!is.character(secondary_var) || length(secondary_var) != 1L ||
      is.na(secondary_var) || !nzchar(secondary_var)) {
    stop("Supply the name of the secondary-outcome column.")
  }
  if (!is.numeric(B) || length(B) != 1L || !is.finite(B) ||
      B < 1 || B > .Machine$integer.max || B != as.integer(B)) {
    stop("B must be a positive integer within the integer limit.")
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max - 1000 || seed != as.integer(seed)) {
    stop("seed must be a nonnegative integer below the integer limit.")
  }
  if (!is.numeric(warn_clusters) || length(warn_clusters) != 1L ||
      !is.finite(warn_clusters) || warn_clusters < 0 ||
      warn_clusters > .Machine$integer.max || warn_clusters != as.integer(warn_clusters)) {
    stop("warn_clusters must be a nonnegative integer within the integer limit.")
  }
  if (!length(sample_definitions) || anyDuplicated(sample_definitions) ||
      any(!sample_definitions %in% c("S", "Stilde"))) {
    stop("sample_definitions must select S, Stilde, or both.")
  }
  if (!is.character(Y_vars) || !length(Y_vars) || anyNA(Y_vars) ||
      anyDuplicated(Y_vars)) stop("Y_vars must contain unique outcome names.")

  required <- unique(c("shrid2", "clusters", "wave", "D", "R_PC1_scaled",
                       Y_vars, secondary_var))
  if (is.null(data)) {
    if (!requireNamespace("readr", quietly = TRUE)) stop("Install readr first.")
    data <- readr::read_csv(
      input_csv, col_select = tidyselect::all_of(required),
      show_col_types = FALSE, progress = FALSE
    )
  }
  if (!all(required %in% names(data))) {
    stop("Missing columns: ", paste(setdiff(required, names(data)), collapse = ", "))
  }
  if (anyNA(data$shrid2) || anyDuplicated(data$shrid2)) {
    stop("Expected one record per nonmissing shrid2; do not duplicate overlap records.")
  }
  for (v in unique(c(Y_vars, "D"))) {
    x <- data[[v]]
    if (!(is.numeric(x) || is.logical(x)) || any(!is.na(x) & !x %in% c(0, 1))) {
      stop(v, " must contain only 0, 1, or missing values.")
    }
  }
  if (!is.numeric(data$R_PC1_scaled)) stop("R_PC1_scaled must be numeric.")
  secondary <- tryCatch(
    dichotomize_secondary(data[[secondary_var]], threshold, percentile, direction),
    error = function(e) stop(secondary_var, ": ", conditionMessage(e), call. = FALSE)
  )
  if (secondary$method != "binary_input") {
    message(sprintf("%s: 1 means %s %.12g (%s); reference n=%d, ties=%d.",
                    secondary_var, secondary$direction, secondary$cutoff,
                    secondary$method, secondary$reference_n, secondary$n_at_cutoff))
  }

  study_waves <- c("Experimental: Treated (2010)",
                   "Experimental: Untreated (2011)",
                   "Experimental: Untreated (2012)")
  obs_wave <- "Observational (N/A)"
  buffer_wave <- "Experimental: Untreated (2011)"
  known <- data$wave %in% c(study_waves, obs_wave)
  eligible <- !is.na(data$D) & data$D == 0 & known
  d <- as.data.frame(data[eligible, required, drop = FALSE])
  # Keep the original column and all primary outcomes unchanged.
  d$.secondary_binary <- secondary$values[eligible]
  if (anyNA(d$clusters) || any(!nzchar(trimws(as.character(d$clusters))))) {
    stop("Every eligible record must have a mandal cluster identifier.")
  }
  d$clusters <- as.character(d$clusters)

  results <- list()
  test_id <- 0L
  for (definition in sample_definitions) {
    in_exp <- d$wave %in% study_waves
    in_obs <- d$wave == obs_wave |
      (definition == "Stilde" & d$wave == buffer_wave)
    for (Y_var in Y_vars) {
      for (y in c(0, 1)) {
        in_y <- !is.na(d[[Y_var]]) & d[[Y_var]] == y
        missing_t <- in_y & is.na(d$.secondary_binary)
        missing_r <- in_y & !is.finite(d$R_PC1_scaled)
        complete <- in_y & !missing_t & !missing_r
        for (t in c(0, 1)) {
          test_id <- test_id + 1L
          keep <- which(complete & !is.na(d$.secondary_binary) &
                          d$.secondary_binary == t)
          message(sprintf("%s: %s=%s, %s=%s", definition, Y_var, y, secondary_var, t))
          fit <- clustered_ks(
            r = d$R_PC1_scaled[keep], clusters = d$clusters[keep],
            in_exp = in_exp[keep], in_obs = in_obs[keep],
            B = B, seed = seed + test_id - 1L, warn_clusters = warn_clusters
          )
          results[[test_id]] <- data.frame(
            sample_definition = definition, outcome = Y_var, Y = y,
            secondary = secondary_var, secondary_value = t,
            secondary_method = secondary$method,
            secondary_cutoff = secondary$cutoff,
            secondary_percentile = secondary$percentile,
            secondary_direction = secondary$direction,
            secondary_reference_n = secondary$reference_n,
            secondary_n_at_cutoff = secondary$n_at_cutoff,
            as.data.frame(fit),
            n_candidates_y = sum(in_y),
            n_missing_secondary_y = sum(missing_t),
            n_missing_R_y = sum(missing_r),
            n_missing_primary_eligible = sum(is.na(d[[Y_var]])),
            B = B, seed = seed + test_id - 1L,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  results <- do.call(rbind, results)
  print(results, row.names = FALSE)
  if (!is.null(output_csv)) {
    dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(results, output_csv, row.names = FALSE)
    message("Saved: ", output_csv)
  }
  invisible(results)
}

parse_binary_stability_args <- function(args, script_name = "STABILITY_SCRIPT.R") {
  usage <- paste(
    paste("Usage: Rscript", script_name, "COLUMN"),
    "[--percentile P | --threshold VALUE]",
    "[--direction above|at-or-above|below|at-or-below] [--B 2000] [--seed 42]",
    "[--warn-clusters 10]"
  )
  if (any(args %in% c("--help", "-h"))) return(list(help = usage))
  if (!length(args) || startsWith(args[[1]], "--")) stop(usage, call. = FALSE)
  options <- list(secondary_var = args[[1]], threshold = NULL, percentile = NULL,
                  direction = "above", B = 2000L, seed = 42L, warn_clusters = 10L)
  seen <- character()
  positional <- 0L
  i <- 2L
  while (i <= length(args)) {
    token <- args[[i]]
    if (startsWith(token, "--")) {
      key <- chartr("-", "_", substring(token, 3L))
      if (!key %in% c("threshold", "percentile", "direction", "B", "seed", "warn_clusters")) {
        stop("Unknown option: ", token, call. = FALSE)
      }
      if (i == length(args) || startsWith(args[[i + 1L]], "--")) {
        stop("Missing value for ", token, call. = FALSE)
      }
      value <- args[[i + 1L]]
      i <- i + 2L
    } else {
      positional <- positional + 1L
      if (positional > 2L) stop(usage, call. = FALSE)
      key <- c("B", "seed")[[positional]]
      value <- token
      i <- i + 1L
    }
    if (key %in% seen) stop("Repeated option: ", key, call. = FALSE)
    seen <- c(seen, key)
    options[[key]] <- if (key == "direction") value else suppressWarnings(as.numeric(value))
  }
  if (!is.null(options$threshold) && !is.null(options$percentile)) {
    stop("Specify either --threshold or --percentile, not both.", call. = FALSE)
  }
  options
}

# Keep the same random draws as the former combined S/Stilde run:
# with the default three outcomes, S uses seeds 42--53 and Stilde 54--65.
run_stability_binary_secondary_cli <- function(
    sample_definition, args = commandArgs(trailingOnly = TRUE)) {
  sample_definition <- match.arg(sample_definition, c("S", "Stilde"))
  script_name <- if (sample_definition == "S") {
    "04_stability_S_secondary.R"
  } else "05_stability_Stilde_secondary.R"
  options <- parse_binary_stability_args(args, script_name)
  if (!is.null(options$help)) {
    cat(options$help, "\n")
    return(invisible(NULL))
  }
  tag <- if (!is.null(options$percentile)) {
    paste("percentile", options$percentile, options$direction, sep = "_")
  } else if (!is.null(options$threshold)) {
    paste("threshold", options$threshold, options$direction, sep = "_")
  } else ""
  safe_name <- gsub("[^[:alnum:]_-]", "_", options$secondary_var)
  if (nzchar(tag)) safe_name <- paste0(safe_name, "_", gsub("[^[:alnum:]_-]", "_", tag))
  options$sample_definitions <- sample_definition
  if (sample_definition == "Stilde") options$seed <- options$seed + 12L
  options$output_csv <- paste0(
    "data/clean/smartcards/assumption_stability_", sample_definition,
    "_secondary_", safe_name, ".csv"
  )
  do.call(run_stability_binary_secondary, options)
}

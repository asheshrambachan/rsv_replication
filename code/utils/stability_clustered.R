# =============================================================================
# Shared runner for Smartcards stability tests with clustered p-values
#
# Tests R_PC1_scaled independent of sample role conditional on D=0 and Y.
# Mirrors the comparisons in 02_stability_S.R and 03_stability_Stilde.R:
#   S:      experimental villages versus nonstudy villages.
#   Stilde: experimental villages versus buffer plus nonstudy villages.
# Experimental roles include the 2010, 2011, and 2012 cohorts, restricted
# to D=0. Buffer villages (2011) contribute to both roles under Stilde.
#
# KS distances are unchanged. P-values use mandal-level multiplier draws
# and preserve dependence from overlapping records. Inference is asymptotic
# and requires independent mandals, no dominating cluster, and sufficient
# support.
# Failure to reject does not establish stability; this tests PC1, not all R.
#
# Entry points: 02_stability_S.R and 03_stability_Stilde.R in
# code/smartcards/02_assumption_tests/.
# warn_clusters in the R function is a warning threshold only, not a cutoff.
# Sourcing defines functions only; the entry points write the original CSVs.
# =============================================================================

source("code/utils/clustered_ks.R")

run_stability_clustered <- function(
    input_csv = "data/clean/smartcards/data.csv",
    Y_vars = c("Ycons", "Ylowinc", "Ymidinc"),
    sample_definitions = c("S", "Stilde"),
    B = 2000L, seed = 42L, warn_clusters = 10L,
    output_csv = NULL, data = NULL) {
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
  if (!is.character(Y_vars) || !length(Y_vars) || anyNA(Y_vars) ||
      any(!nzchar(Y_vars)) || anyDuplicated(Y_vars)) {
    stop("Y_vars must contain unique, nonempty outcome names.")
  }
  if (!length(sample_definitions) || anyDuplicated(sample_definitions) ||
      any(!sample_definitions %in% c("S", "Stilde"))) {
    stop("sample_definitions must select S, Stilde, or both.")
  }

  required <- unique(c("shrid2", "clusters", "wave", "D", "R_PC1_scaled", Y_vars))
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

  study_waves <- c("Experimental: Treated (2010)",
                   "Experimental: Untreated (2011)",
                   "Experimental: Untreated (2012)")
  obs_wave <- "Observational (N/A)"
  buffer_wave <- "Experimental: Untreated (2011)"
  known <- data$wave %in% c(study_waves, obs_wave)
  eligible <- !is.na(data$D) & data$D == 0 & known
  d <- as.data.frame(data[eligible, required, drop = FALSE])
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
        test_id <- test_id + 1L
        in_y <- !is.na(d[[Y_var]]) & d[[Y_var]] == y
        missing_r <- in_y & !is.finite(d$R_PC1_scaled)
        keep <- which(in_y & !missing_r)
        message(sprintf("%s: %s=%s", definition, Y_var, y))
        fit <- clustered_ks(
          r = d$R_PC1_scaled[keep], clusters = d$clusters[keep],
          in_exp = in_exp[keep], in_obs = in_obs[keep],
          B = B, seed = seed + test_id - 1L, warn_clusters = warn_clusters
        )
        results[[test_id]] <- data.frame(
          sample_definition = definition, outcome = Y_var, Y = y,
          as.data.frame(fit),
          n_candidates_y = sum(in_y), n_missing_R_y = sum(missing_r),
          n_missing_primary_eligible = sum(is.na(d[[Y_var]])),
          B = B, seed = seed + test_id - 1L,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  results <- do.call(rbind, results)
  if (!is.null(output_csv)) {
    dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(results, output_csv, row.names = FALSE)
    message("Saved: ", output_csv)
  }
  invisible(results)
}


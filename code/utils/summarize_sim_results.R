
summarize_sim_results <- function(all_results,
                                  by_cols = c("outcome", "tau", "n_e"),
                                  alpha = 0.05) {
  stopifnot(is.character(by_cols), length(by_cols) > 0)

  ci_mult <- qnorm(1 - alpha / 2)

  # Basic column checks (fail early with a helpful message)
  required <- c("tauhat", "tau", "se", by_cols)
  missing <- setdiff(required, names(all_results))
  if (length(missing) > 0) {
    stop("Missing required columns in results: ", paste(missing, collapse = ", "))
  }

  summary_stats <- all_results[, .(
    avg_bias = mean(tauhat - tau, na.rm = TRUE),
    sd_est   = sd(tauhat, na.rm = TRUE),
    avg_se   = mean(se, na.rm = TRUE),
    coverage = mean(
      tau >= tauhat - ci_mult * se &
        tau <= tauhat + ci_mult * se,
      na.rm = TRUE
    ),
    rmse     = sqrt(mean((tauhat - tau)^2, na.rm = TRUE)),
    n_sims   = sum(!is.na(tauhat)),
    prop_na  = mean(is.na(tauhat))
  ), by = by_cols]

  # summary_stats[, normalized_bias := avg_bias / avg_se]
  summary_stats[, normalized_bias := avg_bias / sd_est]

  # Put columns in a consistent order: grouping + metrics
  metric_cols <- c("normalized_bias", "coverage", "rmse",
                   "avg_bias", "sd_est", "avg_se",
                   "n_sims", "prop_na")
  setcolorder(summary_stats, c(by_cols, metric_cols))

  invisible(summary_stats)
}

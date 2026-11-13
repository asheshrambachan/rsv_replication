# Estimating denominators, standardized separation and normal denominator CIs.
local({
  source("code/smartcards/04_empirical_variance_regression/settings.R", local = TRUE)
  cfg <- variance_empirical_settings
  rows <- list()
  for (sample in cfg$samples) for (Y_var in cfg$Y_vars) {
    path <- file.path(cfg$interim_dir, sample, paste0(Y_var, ".Rds"))
    if (!file.exists(path)) stop("Run 01_empirical.R first; missing ", path)
    report <- variance_empirical_summary(readRDS(path), cfg$level)
    rows[[length(rows) + 1L]] <- data.frame(sample = sample, Y_var = Y_var,
      estimator = report$stage, estimate = report$denominator,
      separation = report$separation, se = report$denominator_se,
      ci_lower = report$denom_ci_lower, ci_upper = report$denom_ci_upper,
      level = cfg$level, failed_draws = report$failed_draws, status = report$inference_status)
  }
  dir.create(cfg$clean_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cfg$clean_dir, "relevance.csv")
  utils::write.csv(do.call(rbind, rows), path, row.names = FALSE)
  cat("Saved:", path, "\n")
})

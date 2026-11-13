# Estimates and normal CIs for unit and regression-variance RSV, no calibration.
local({
  source("code/smartcards/04_empirical_variance_regression/settings.R", local = TRUE)
  cfg <- variance_empirical_settings
  rows <- list()
  for (sample in cfg$samples) for (Y_var in cfg$Y_vars) {
    path <- file.path(cfg$interim_dir, sample, paste0(Y_var, ".Rds"))
    if (!file.exists(path)) stop("Run 01_empirical.R first; missing ", path)
    saved <- readRDS(path)
    rows[[length(rows) + 1L]] <- variance_empirical_summary(saved, cfg$level)
  }
  results <- do.call(rbind, rows)
  confidence_intervals <- results[c("sample", "Y_var", "stage", "level", "ci_lower", "ci_upper", "ci_type")]
  diagnostics <- results[c("sample", "Y_var", "stage", "variance_method", "preliminary_tau",
                           "variance_floor", "floored_share", "sigma2_quantile", "H_min", "H_max")]
  dir.create(cfg$clean_dir, recursive = TRUE, showWarnings = FALSE)
  outputs <- list(results = results, confidence_intervals = confidence_intervals,
                  variance_diagnostics = diagnostics)
  for (name in names(outputs)) {
    path <- file.path(cfg$clean_dir, paste0(name, ".csv"))
    utils::write.csv(outputs[[name]], path, row.names = FALSE)
    cat("Saved:", path, "\n")
  }
})

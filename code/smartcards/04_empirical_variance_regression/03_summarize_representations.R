# Row-aligned unit/learned-variance H with IDs, masks and variance predictions.
local({
  source("code/smartcards/04_empirical_variance_regression/settings.R", local = TRUE)
  cfg <- variance_empirical_settings
  rows <- list()
  for (sample in cfg$samples) for (Y_var in cfg$Y_vars) {
    path <- file.path(cfg$interim_dir, sample, paste0(Y_var, ".Rds"))
    if (!file.exists(path)) stop("Run 01_empirical.R first; missing ", path)
    saved <- readRDS(path)
    stopifnot(nrow(saved$observations) == nrow(saved$fit$data))
    H <- as.data.frame(lapply(saved$fits, function(fit) as.numeric(fit$weights[[1]])))
    rows[[length(rows) + 1L]] <- data.frame(sample = sample, Y_var = Y_var,
      saved$observations, H, pred_sigma2 = saved$fit$data$pred_sigma2,
      sigma2 = pmax(saved$fit$data$pred_sigma2, saved$fit$variance$floor))
  }
  dir.create(cfg$clean_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cfg$clean_dir, "representations.csv")
  utils::write.csv(do.call(rbind, rows), path, row.names = FALSE)
  cat("Saved:", path, "\n")
})

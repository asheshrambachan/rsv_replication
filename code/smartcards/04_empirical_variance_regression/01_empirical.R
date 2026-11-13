# Full-sample RSV with squared-full-residual variance regression, no secondary
# outcomes or calibration. Saves point checkpoints before fixed-prediction SEs.
local({
  for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file, local = TRUE)
  source("code/smartcards/04_empirical_variance_regression/settings.R", local = TRUE)
  cfg <- variance_empirical_settings
  if (!length(cfg$Y_vars) || anyDuplicated(cfg$Y_vars) ||
      any(!cfg$Y_vars %in% c("Ycons", "Ylowinc", "Ymidinc")) ||
      !length(cfg$samples) || anyDuplicated(cfg$samples) ||
      any(!cfg$samples %in% c("full", "nospillover"))) stop("Check outcomes and samples in settings.R.")
  if (length(cfg$level) != 1L || !is.finite(cfg$level) || cfg$level <= 0 || cfg$level >= 1) {
    stop("level must be between zero and one.")
  }
  data <- as.data.frame(data.table::fread(cfg$input_csv))
  R_columns <- grep("^(luminosity_|satellite_)", names(data), value = TRUE)
  required <- c("shrid2", "clusters", "wave", "D", cfg$Y_vars,
                if ("nospillover" %in% cfg$samples) "spillover_20km")
  if (!length(R_columns) || any(!required %in% names(data))) stop("Required data columns are missing.")
  if (anyNA(data$shrid2) || anyDuplicated(data$shrid2)) stop("Expected one row per shrid2.")
  if (anyNA(data$clusters) || any(!nzchar(trimws(as.character(data$clusters))))) stop("Missing mandal IDs.")
  waves <- c("Experimental: Treated (2010)", "Experimental: Untreated (2011)",
             "Experimental: Untreated (2012)", "Observational (N/A)")
  data <- data[data$wave %in% waves, , drop = FALSE]
  models <- setNames(rep(list(cfg$rf_spec), 4L), c("Y", "D", "S_e", "S_o"))
  for (sample in cfg$samples) {
    selected <- if (sample == "full") rep(TRUE, nrow(data)) else {
      if (anyNA(data$spillover_20km) || any(!data$spillover_20km %in% c(0, 1))) {
        stop("spillover_20km must be observed and logical/0/1.")
      }
      !as.logical(data$spillover_20km)
    }
    df <- data[selected, , drop = FALSE]
    S_e <- df$wave %in% waves[1:3]
    S_o <- df$wave %in% waves[c(2, 4)]
    D <- df$D
    D[!S_e] <- NA_real_
    out_dir <- file.path(cfg$interim_dir, sample)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    for (Y_var in cfg$Y_vars) {
      cat("\nVariance-regression empirical:", sample, Y_var, "; villages:", nrow(df), "\n")
      Y <- df[[Y_var]]
      Y[!S_o] <- NA_real_
      fit <- rsv(Y, D, S_e, S_o, as.matrix(df[R_columns]), y_levels = c(0, 1), yK = 0,
        models = models, sigma2.method = "regression", sigma2_model = cfg$rf_spec,
        sigma2.quantile = cfg$sigma2.quantile, seed = cfg$seed, num.threads = cfg$num.threads)
      # The unit fit reuses exactly the same initial predictions; no forests refit.
      unit <- rsv_fitted(Y, D, S_e, S_o, y_levels = c(0, 1), yK = 0,
        pred_Y = fit$data[c("pred_Y0", "pred_Y1")], pred_D = fit$data$pred_D,
        pred_S_e = fit$data$pred_S_e, pred_S_o = fit$data$pred_S_o,
        theta_init = fit$theta_init, pred_sigma2 = rep(1, nrow(df)), sigma2.lower = 1)
      fits <- list(uncalibrated_unit = unit, uncalibrated_updated = fit)
      observations <- cbind(df[c("shrid2", "clusters", "wave")],
                             fit$data[c("Y", "D", "S_e", "S_o")])
      bundle <- list(fit = fit, fits = fits, observations = observations,
        sample = sample, Y_var = Y_var, settings = cfg,
        status = "point estimates saved; inference pending")
      out_path <- file.path(out_dir, paste0(Y_var, ".Rds"))
      saveRDS(bundle, out_path)
      cat("  Point estimates saved; unstratified mandal score bootstrap next.\n")
      for (stage in names(fits)) {
        cat("  Stage:", stage, "; draws:", cfg$B, "\n")
        fits[[stage]] <- add_se(fits[[stage]], method = "score_bootstrap", B = cfg$B,
          clusters = df$clusters, strata = NULL, seed = cfg$seed, num.threads = cfg$num.threads)
        bundle$fits <- fits
        bundle$fit <- fits$uncalibrated_updated
        saveRDS(bundle, out_path)  # Keep completed stages if a later bootstrap stops.
      }
      bundle$status <- "complete"
      saveRDS(bundle, out_path)
      cat("Saved:", out_path, "\n")
      print(variance_empirical_summary(bundle, cfg$level), row.names = FALSE)
      invisible(gc())
    }
  }
})

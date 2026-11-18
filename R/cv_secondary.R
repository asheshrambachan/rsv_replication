# Cross-fitted secondary calibration for binary primary outcomes, without X.
# Prediction models and training target centers are learned outside each fold.
# Analysis q is centered on pooled observational records; g is never recentered.
# The same variance weights are used for each with/without calibration pair.

.cv_secondary_evaluate <- function(df, secondary, g, basis, theta_init,
                                   sigma2.quantile, sigma2.lower,
                                   target_names = colnames(g)) {
  state <- fit_secondary_targets(df$Y, secondary, df$S_o, df$S_e, basis = basis)
  targets <- apply_secondary_targets(state, df$Y, df$S_o, secondary, df$S_e)
  if (any(!colnames(targets) %in% target_names) || !identical(colnames(g), target_names)) {
    stop("Calibration target coordinates changed.")
  }
  q <- matrix(0, nrow(df), length(target_names), dimnames = list(NULL, target_names))
  q[, colnames(targets)] <- targets
  counts <- compute_counts(df, c(0, 1), character())
  deltas <- compute_deltas(df, c(0, 1), 0, use_pred = FALSE)
  if (any(!is.finite(as.matrix(deltas)))) stop("Absent outcome/treatment cell or nonfinite deltas.")
  H0 <- compute_H(df, y_levels = c(0, 1), yK = 0,
                  X_cols = character(), sigma2 = 1, counts = counts)$H_Y1
  stages <- c("uncalibrated_unit", "calibrated_unit",
              "uncalibrated_weighted", "calibrated_weighted")
  rows <- vector("list", 4L)
  weights <- matrix(NA_real_, nrow(df), 4L, dimnames = list(NULL, stages))
  calibrations <- list()
  variance <- NULL
  variance_error <- tryCatch({
    variance <- if ("pred_sigma2" %in% names(df)) {
      .variance_from_predictions(df, sigma2.quantile, sigma2.lower)
    } else compute_sigma2(df, theta_init, c(0, 1), sigma2.lower, sigma2.quantile)
    if (any(!is.finite(variance)) || any(variance <= 0)) stop("Variance weights must be finite and positive.")
    NULL
  }, error = function(e) conditionMessage(e))
  for (j in seq_along(stages)) {
    rows[[j]] <- tryCatch({
      weighted <- j > 2L
      if (weighted && !is.null(variance_error)) stop(variance_error)
      v <- if (weighted) variance else 1
      H <- H0 / v
      correction <- NULL
      if (j %% 2L == 0L) {
        correction <- calibrate_secondary(q, g, H, v)
        H <- correction$H
        calibrations[[stages[j]]] <- correction
      }
      weights[, j] <- H
      .secondary_score_row(stages[j], H, deltas, df$S_o, correction)
    }, error = function(e) {
      row <- .secondary_score_row(stages[j], H0, deltas, df$S_o)
      row[setdiff(names(row), c("stage", "status"))] <- NA_real_
      row$status <- conditionMessage(e)
      row
    })
  }
  list(results = do.call(rbind, rows), weights = as.data.frame(weights),
       q = q, state = state, counts = counts, deltas = deltas,
       calibrations = calibrations, sigma2 = variance)
}

#' Cross-fitted RSV with and without secondary-outcome calibration.
#'
#' Binary Y (0/1), random forests, no baseline X. Uses cv.rsv() for the
#' unchanged primary nuisance/variance fits. Plugin is the default; regression
#' uses cv.rsv()'s training-only preliminary effects, regressions and floors.
#' Returns paired unit and variance-weighted stages on shared predictions.
#' Pooled calibration solves one moment system, not an average of fold ratios.
#' Training-centered g and pooled analysis-centered q are stored separately.
cv.rsv_secondary <- function(Y, D, S_e, S_o, R, secondary, models,
                             secondary_basis = NULL, q_model = models$Y,
                             nfolds = 5L, clusters = NULL,
                             sigma2.method = c("plugin", "regression"),
                             sigma2_model = NULL, sigma2.quantile = .01,
                             sigma2.lower = NULL, seed = 42L,
                             num.threads = 1L, keep_fits = FALSE, X = NULL) {
  cl <- match.call()
  validate_required_inputs(Y, D, S_e, S_o)
  roles <- .secondary_target_roles(Y, S_o, S_e, c(0, 1), X)
  S_e <- roles$S_e; S_o <- roles$S_o
  if (!(is.numeric(D) || is.logical(D)) || anyNA(D[S_e]) || any(!D[S_e] %in% 0:1)) {
    stop("Experimental D must be observed and binary.")
  }
  Y[!S_o] <- NA_real_; D[!S_e] <- NA_real_
  R <- validate_R(R, length(Y))
  if (!is.numeric(R) || any(!is.finite(R))) stop("R must contain finite numeric predictors.")
  if (is.atomic(secondary) && is.null(dim(secondary))) secondary <- data.frame(secondary = secondary)
  secondary <- as.data.frame(secondary)
  if (nrow(secondary) != length(Y)) stop("secondary must have one row per observation.")
  secondary[!S_o, ] <- NA
  specs <- resolve_models(models)
  validate_models(specs)
  if (any(vapply(specs, function(x) x$model != "rf", logical(1))) ||
      !identical(q_model$model, "rf") ||
      (!is.null(sigma2_model) && !identical(sigma2_model$model, "rf"))) {
    stop("cv.rsv_secondary() currently supports random forests only.")
  }
  sigma2.method <- match.arg(sigma2.method)
  .variance_prediction_floor(1, sigma2.quantile, sigma2.lower)
  state <- fit_secondary_targets(Y, secondary, S_o, S_e, basis = secondary_basis)
  target_names <- state$target_names
  base <- cv.rsv(Y, D, S_e, S_o, R, models = models, nfolds = nfolds,
    clusters = clusters, y_levels = c(0, 1), yK = 0, seed = seed,
    num.threads = num.threads, sigma2.method = sigma2.method,
    sigma2_model = sigma2_model, sigma2.quantile = sigma2.quantile,
    sigma2.lower = sigma2.lower)
  g <- matrix(0, length(Y), length(target_names), dimnames = list(NULL, target_names))
  training_states <- vector("list", nfolds)
  training_errors <- rep(NA_character_, nfolds)
  fitted_g <- if (keep_fits) vector("list", nfolds) else NULL
  for (k in seq_len(nfolds)) {
    train <- base$fold_ids != k; test <- base$fold_ids == k
    error <- tryCatch({
      train_state <- fit_secondary_targets(Y[train], secondary[train, , drop = FALSE],
        S_o[train], S_e[train], basis = secondary_basis)
      if (!identical(train_state$target_names, target_names)) {
        stop("Training support changes calibration targets; use adequate cell support.")
      }
      q_train <- apply_secondary_targets(train_state, Y[train], S_o[train],
        secondary[train, , drop = FALSE], S_e[train])
      fits <- vector("list", ncol(q_train))
      for (j in seq_len(ncol(q_train))) {
        fitted <- fit_secondary_regression(R[train, , drop = FALSE], q_train[, j],
          q_model, if (is.null(seed)) NULL else seed + 3L + j, num.threads,
          R_predict = R[test, , drop = FALSE])
        g[test, j] <- fitted$predictions
        if (keep_fits) fits[[j]] <- fitted$model
      }
      training_states[[k]] <- train_state
      if (keep_fits) fitted_g[[k]] <- fits
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(error)) {
      g[test, ] <- NA_real_
      training_errors[k] <- error
    }
  }
  df <- .ensure_numeric_indicators(cbind(data.frame(Y, D, S_e, S_o), base$pooled_predictions))
  theta_init <- compute_theta_init(df, c(0, 1), yK = 0, X_cols = character())
  evaluated <- .cv_secondary_evaluate(df, secondary, g, secondary_basis,
    theta_init, sigma2.quantile, sigma2.lower)
  if (any(!is.na(training_errors))) {
    evaluated$results$status[c(2, 4)] <- paste("Secondary training failed:", paste(
      paste0("fold ", which(!is.na(training_errors)), ": ", training_errors[!is.na(training_errors)]),
      collapse = "; "))
  }
  final <- evaluated$results[evaluated$results$stage == "calibrated_weighted", ]
  result <- c(evaluated, list(coefficients = final$tau, relevance = final$denominator,
    data = df, secondary = secondary, g = g, secondary_basis = secondary_basis,
    theta_init = theta_init, training_states = training_states, training_errors = training_errors,
    fold_ids = base$fold_ids,
    nfolds = nfolds, clusters = clusters, pooled_predictions = base$pooled_predictions,
    models = specs, q_model = q_model, sigma2.method = sigma2.method,
    sigma2_model = base$sigma2_model, sigma2.quantile = sigma2.quantile,
    sigma2.lower = sigma2.lower, variance = base$variance,
    seed = seed, n.obs = sum(S_o), n.exp = sum(S_e), n.both = sum(S_e & S_o),
    call = cl, split = "cv"))
  if (keep_fits) result$fitted_g <- fitted_g
  class(result) <- "cv.rsv_secondary"
  result
}

# Fixed out-of-fold predictions; rebuild analysis centering and beta per draw.
# The plugin preliminary theta stays fixed, matching ordinary CV score bootstrap.
# Regression predictions and their training-sample floors remain fixed too.
add_se.cv.rsv_secondary <- function(object, method = "score_bootstrap", B = 1000L,
                                    clusters = object$clusters, strata = NULL,
                                    level = .95, seed = 42L, num.threads = 1L,
                                    progress = FALSE) {
  if (!identical(method, "score_bootstrap")) stop("Use method = 'score_bootstrap'.")
  if (length(B) != 1L || !is.numeric(B) || !is.finite(B) || B < 2 || B != as.integer(B)) {
    stop("B must be an integer of at least two.")
  }
  if (length(num.threads) != 1L || !is.finite(num.threads) || num.threads < 1 ||
      num.threads != as.integer(num.threads)) stop("Invalid num.threads.")
  validate_bootstrap_params(B, clusters, nrow(object$data))
  if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) stop("Invalid CI level.")
  if (length(seed) != 1L || !is.finite(seed) || seed < 0 || seed != as.integer(seed) ||
      seed + B > .Machine$integer.max) stop("Invalid bootstrap seed.")
  plan <- .bootstrap_resampling_plan(nrow(object$data), clusters, strata)
  run_one <- function(b) {
    set.seed(seed + b)
    rows <- .draw_bootstrap_rows(plan)
    draw <- tryCatch(.cv_secondary_evaluate(object$data[rows, , drop = FALSE],
      object$secondary[rows, , drop = FALSE], object$g[rows, , drop = FALSE],
      object$secondary_basis, object$theta_init, object$sigma2.quantile,
      object$sigma2.lower, colnames(object$q))$results, error = function(e) {
        out <- object$results
        out[setdiff(names(out), c("stage", "status"))] <- NA_real_
        out$status <- conditionMessage(e)
        out
      })
    draw$draw <- b
    if (progress && (b %% 200L == 0L || b == B)) cat("  Bootstrap", b, "/", B, "\n")
    draw
  }
  draws <- do.call(rbind, parallel::mclapply(seq_len(B), run_one, mc.cores = num.threads))
  z <- stats::qnorm(1 - (1 - level) / 2)
  inference <- lapply(seq_len(nrow(object$results)), function(j) {
    point <- object$results[j, ]; d <- draws[draws$stage == point$stage, ]
    failed <- d$status != "ok" | !is.finite(d$tau) |
      !is.finite(d$numerator) | !is.finite(d$denominator)
    se <- if (point$status == "ok" && !any(failed)) stats::sd(d$tau) else NA_real_
    data.frame(stage = point$stage, se = se, ci_lower = point$tau - z * se,
      ci_upper = point$tau + z * se, failed_draws = sum(failed),
      status = if (is.finite(se)) "Score-bootstrap SE and normal confidence interval." else {
        "Point/draw failures retained; inference unavailable."
      })
  })
  object$inference <- do.call(rbind, inference)
  object$bootstrap <- list(B = B, seed = seed, level = level, draws = draws,
    method = method, clusters = clusters, strata = strata,
    tau_draws = draws$tau[draws$stage == "calibrated_weighted"],
    n_failed = object$inference$failed_draws[object$inference$stage == "calibrated_weighted"],
    seed_draws = seed + seq_len(B),
    limitation = "Out-of-fold predictions fixed; prediction-learning uncertainty excluded.")
  object$se <- object$inference$se[object$inference$stage == "calibrated_weighted"]
  object$vcov <- matrix(object$se^2, 1, 1, dimnames = list("D", "D"))
  object$se.method <- method
  object
}

coef.cv.rsv_secondary <- function(object, ...) object$coefficients

confint.cv.rsv_secondary <- function(object, parm, level = .95, ...) {
  if (is.null(object$inference)) stop("Call add_se() first.")
  if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) stop("Invalid CI level.")
  stages <- if (missing(parm)) object$results$stage else parm
  if (any(!stages %in% object$results$stage)) stop("Unknown stage.")
  j <- match(stages, object$results$stage)
  z <- stats::qnorm(1 - (1 - level) / 2)
  data.frame(stage = stages, lower = object$results$tau[j] - z * object$inference$se[j],
             upper = object$results$tau[j] + z * object$inference$se[j])
}

print.cv.rsv_secondary <- function(x, ...) {
  cat("Cross-fitted secondary RSV:", x$nfolds, "folds;", x$sigma2.method, "variance\n")
  print(x$results, row.names = FALSE, ...)
  if (!is.null(x$inference)) print(x$inference, row.names = FALSE)
  invisible(x)
}

# Full-sample RSV with secondary-outcome calibration (binary Y, no X).
# Prediction models use R only. q is centered within observational Y groups.
# Finite bases impose selected moments; population calibration also needs the
# manuscript's specification/support assumptions. No whitening or cell scaling.

calibrate_secondary <- function(q, g, H0, sigma2 = 1, verbose = FALSE) {
  q <- as.matrix(q)
  g <- as.matrix(g)
  n <- length(H0)
  if (!identical(dim(q), dim(g)) || nrow(q) != n ||
      !length(sigma2) || !length(sigma2) %in% c(1L, n) ||
      any(!is.finite(c(q, g, H0, sigma2))) || any(sigma2 <= 0)) {
    stop("Check dimensions, finite inputs and positive variance weights.")
  }
  if (!ncol(q)) return(list(H = H0, beta = numeric(), M = matrix(numeric(), 0, 0),
                           b = numeric(), singular_values = numeric(),
                           condition = NA_real_, balance = 0))
  adjustment <- g / sigma2
  M <- crossprod(q, adjustment) / n
  b <- crossprod(q, H0) / n
  singular_values <- svd(M, nu = 0, nv = 0)$d
  if (verbose) cat("  M singular values:", format(singular_values, digits = 5), "\n")
  if (min(singular_values) <= .Machine$double.eps * max(singular_values) * max(dim(M))) {
    stop("Calibration matrix is numerically singular; no regularization applied.")
  }
  beta <- solve(M, b)
  H <- as.numeric(H0 - adjustment %*% beta)
  list(H = H, beta = beta, M = M, b = b, singular_values = singular_values,
       condition = max(singular_values) / min(singular_values),
       balance = max(abs(colMeans(q * H))))
}

compute_secondary_effect <- function(H, deltas, S_o) {
  numerator <- mean(H * deltas$delta_e)
  denominator <- mean(H * deltas$delta_o_Y1)
  sd_o <- stats::sd(H[as.logical(S_o)])
  zero <- abs(denominator) <= .Machine$double.eps * max(1, sd_o) * 16
  list(numerator = numerator, denominator = denominator,
       tau = if (zero) NA_real_ else numerator / denominator,
       standardized_relevance = if (is.finite(sd_o) && sd_o > 0) abs(denominator) / sd_o else NA_real_)
}

# Compatibility names for the shared numeric-response/variance helpers.
fit_secondary_regression <- function(R, response, rf_spec, seed, num_threads,
                                     R_predict = R) {
  fit_numeric_regression(R, response, rf_spec, seed, num_threads, R_predict)
}

.secondary_variance_floor <- function(raw, sigma2.quantile, sigma2.lower = NULL) {
  .variance_prediction_floor(raw, sigma2.quantile, sigma2.lower)
}

fit_secondary_variance <- function(R, squared_residual, rf_spec, seed, num_threads,
                                   R_predict = R, sigma2.quantile = 0.01,
                                   sigma2.lower = NULL) {
  fit_variance_regression(R, squared_residual, rf_spec, seed, num_threads,
                          R_predict, sigma2.quantile, sigma2.lower)
}

.secondary_stage_names <- function() {
  c("uncalibrated_unit", "calibrated_unit", "calibrated_updated")
}

.secondary_score_row <- function(stage, H, deltas, S_o, calibration = NULL,
                                 lower = NA_real_, floored_share = NA_real_) {
  effect <- compute_secondary_effect(H, deltas, S_o)
  data.frame(stage = stage, tau = effect$tau, numerator = effect$numerator,
             denominator = effect$denominator, separation = effect$standardized_relevance,
             M_condition = if (is.null(calibration)) NA_real_ else calibration$condition,
             balance = if (is.null(calibration)) NA_real_ else calibration$balance,
             H_min = min(H), H_max = max(H), variance_floor = lower,
             floored_share = floored_share,
             status = if (all(is.finite(c(effect$tau, effect$standardized_relevance)))) "ok"
                      else "Undefined effect or separation")
}

# Rebuild empirical quantities for given, fixed row-aligned predictions.
# Used both by point estimation and the fixed-prediction mandal bootstrap.
.secondary_evaluate_predictions <- function(df, secondary, g, secondary_basis,
    raw_sigma = NULL, sigma2.quantile = 0.01, sigma2.lower = NULL,
    target_names = colnames(g)) {
  state <- fit_secondary_targets(df$Y, secondary, df$S_o, df$S_e,
                                 basis = secondary_basis)
  fitted_q <- apply_secondary_targets(state, df$Y, df$S_o, secondary, df$S_e)
  # A constant bootstrap group retains its original zero target columns. Do not
  # silently remove calibration restrictions or change their coordinate order.
  if (any(!colnames(fitted_q) %in% target_names) ||
      !identical(colnames(g), target_names)) stop("Calibration target coordinates changed.")
  q <- matrix(0, nrow(df), length(target_names), dimnames = list(NULL, target_names))
  q[, colnames(fitted_q)] <- fitted_q
  counts <- compute_counts(df, c(0, 1), character(0))
  deltas <- compute_deltas(df, c(0, 1), yK = 0, use_pred = FALSE)
  if (any(!is.finite(as.matrix(deltas)))) stop("Absent outcome/treatment cell or nonfinite deltas.")
  H0 <- compute_H(df, y_levels = c(0, 1), yK = 0, X_cols = character(0),
                   sigma2 = 1, counts = counts)$H_Y1
  stages <- .secondary_stage_names()
  weights <- matrix(NA_real_, nrow(df), 3L, dimnames = list(NULL, stages))
  weights[, 1] <- H0
  rows <- list(.secondary_score_row(stages[1], H0, deltas, df$S_o))
  calibrations <- list()
  sigma2 <- rep(NA_real_, nrow(df))
  for (j in 2:3) {
    error <- tryCatch({
      lower <- floored <- NA_real_
      if (j == 2L) {
        variance <- 1
        initial <- H0
      } else {
        if (is.null(raw_sigma)) stop("Variance model not fitted.")
        lower <- .secondary_variance_floor(raw_sigma, sigma2.quantile, sigma2.lower)
        sigma2 <- variance <- pmax(raw_sigma, lower)
        floored <- mean(raw_sigma < lower)
        initial <- H0 / sigma2
      }
      correction <- calibrate_secondary(q, g, initial, variance)
      weights[, j] <- correction$H
      calibrations[[stages[j]]] <- correction
      rows[[j]] <- .secondary_score_row(stages[j], correction$H, deltas, df$S_o,
                                        correction, lower, floored)
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(error)) {
      row <- rows[[1]]
      row[setdiff(names(row), c("stage", "status"))] <- NA_real_
      row$stage <- stages[j]
      row$status <- error
      rows[[j]] <- row
    }
  }
  list(results = do.call(rbind, rows), weights = as.data.frame(weights), state = state,
       q = q, deltas = deltas, counts = counts, calibrations = calibrations, sigma2 = sigma2)
}

#' Full-sample secondary-calibrated RSV, using random forests.
#'
#' Binary Y coded 0/1 and no baseline X. secondary_basis is a fixed numeric
#' transformation or a specification accepted by fit_secondary_targets().
#' All forests fit and predict on the same full sample (resubstitution).
#' add_se() provides a separate, explicitly fixed-prediction bootstrap method.
rsv_secondary <- function(Y, D, S_e, S_o, R, secondary, models,
                          secondary_basis = NULL, q_model = models$Y,
                          sigma2_model = q_model, sigma2.quantile = 0.01,
                          sigma2.lower = NULL, seed = 42L, num.threads = 1L,
                          keep_fits = FALSE, X = NULL) {
  cl <- match.call()
  validate_required_inputs(Y, D, S_e, S_o)
  roles <- .secondary_target_roles(Y, S_o, S_e, c(0, 1), X)
  S_e <- roles$S_e
  S_o <- roles$S_o
  if (!is.numeric(D) && !is.logical(D)) stop("D must be numeric or logical.")
  if (anyNA(D[S_e]) || any(!D[S_e] %in% c(0, 1))) stop("Experimental D must be observed and binary.")
  Y[!S_o] <- NA_real_
  D[!S_e] <- NA_real_
  R <- validate_R(R, length(Y))
  if (!is.numeric(R) || any(!is.finite(R))) stop("R must contain finite numeric predictors.")
  .secondary_variance_floor(1, sigma2.quantile, sigma2.lower)
  specs <- resolve_models(models)
  validate_models(specs)
  if (any(vapply(specs, function(x) x$model != "rf", logical(1))) ||
      !identical(q_model$model, "rf") || !identical(sigma2_model$model, "rf")) {
    stop("rsv_secondary() currently supports random forests only.")
  }
  if (is.atomic(secondary) && is.null(dim(secondary))) secondary <- data.frame(secondary = secondary)
  secondary <- as.data.frame(secondary)
  if (nrow(secondary) != length(Y)) stop("secondary must have one row per observation.")
  secondary[!S_o, ] <- NA
  state <- fit_secondary_targets(Y, secondary, S_o, S_e, basis = secondary_basis)
  q <- apply_secondary_targets(state, Y, S_o, secondary, S_e)
  initial <- fit_all_models(R_train = R, Y = Y, D = D, S_e = S_e, S_o = S_o,
                            R_test = R, X_cols = character(0), y_levels = c(0, 1),
                            yK = 0, models = specs, seed = seed,
                            num.threads = num.threads, keep_fits = TRUE)
  df <- initial$training_data
  g <- matrix(0, nrow(R), ncol(q), dimnames = list(NULL, colnames(q)))
  fits <- list()
  for (j in seq_len(ncol(q))) {
    if (length(unique(q[, j])) < 2L) stop("Target has no variation: ", colnames(q)[j])
    fit <- fit_secondary_regression(R, q[, j], q_model,
                                    if (is.null(seed)) NULL else seed + 3L + j, num.threads)
    g[, j] <- fit$predictions
    if (keep_fits) fits[[colnames(q)[j]]] <- fit$model
  }
  evaluated <- .secondary_evaluate_predictions(df, secondary, g, secondary_basis)
  variance_error <- evaluated$results$status[2]
  raw_sigma <- NULL
  variance_fit <- NULL
  if (variance_error == "ok") {
    variance_error <- tryCatch({
      tau0 <- evaluated$results$tau[2]
      residual2 <- (evaluated$deltas$delta_e - tau0 * evaluated$deltas$delta_o_Y1)^2
      variance_fit <- fit_secondary_variance(R, residual2, sigma2_model,
        if (is.null(seed)) NULL else seed + 5L, num.threads,
        sigma2.quantile = sigma2.quantile, sigma2.lower = sigma2.lower)
      raw_sigma <- variance_fit$raw
      NULL
    }, error = function(e) conditionMessage(e))
  }
  if (!is.null(raw_sigma)) {
    evaluated <- .secondary_evaluate_predictions(df, secondary, g, secondary_basis,
                                                 raw_sigma, sigma2.quantile, sigma2.lower)
  } else {
    evaluated$results$status[3] <- paste("Variance update unavailable:", variance_error)
  }
  final <- evaluated$results[3, ]
  result <- c(evaluated, list(
    coefficients = final$tau, relevance = final$denominator, data = df,
    secondary = secondary, g = g, raw_sigma = raw_sigma,
    secondary_basis = secondary_basis, models = c(specs, list(q = q_model, sigma2 = sigma2_model)),
    sigma2.quantile = sigma2.quantile, sigma2.lower = sigma2.lower,
    seed = seed, num.threads = num.threads, split = "none",
    n.obs = sum(S_o), n.exp = sum(S_e), n.both = sum(S_e & S_o), call = cl))
  if (keep_fits) result$fitted_models <- c(initial$fitted_models, list(q = fits,
    sigma2 = if (is.null(variance_fit)) NULL else variance_fit$model))
  class(result) <- "rsv_secondary"
  result
}

# Change the variance floor while reusing the SAME prediction functions,
# including the variance forest. Inference is cleared because the score changed.
recalibrate_secondary <- function(object, sigma2.quantile = object$sigma2.quantile,
                                  sigma2.lower = object$sigma2.lower) {
  if (!inherits(object, "rsv_secondary")) stop("Expected an rsv_secondary object.")
  .secondary_variance_floor(1, sigma2.quantile, sigma2.lower)
  evaluated <- .secondary_evaluate_predictions(object$data, object$secondary,
    object$g, object$secondary_basis, object$raw_sigma,
    sigma2.quantile, sigma2.lower, colnames(object$q))
  object[names(evaluated)] <- evaluated
  object$sigma2.quantile <- sigma2.quantile
  object$sigma2.lower <- sigma2.lower
  object$coefficients <- object$results$tau[3]
  object$relevance <- object$results$denominator[3]
  for (name in c("inference", "bootstrap", "confidence_sets", "confidence_intervals",
                 "benchmark", "se", "relevance_se", "vcov", "se.method")) object[[name]] <- NULL
  object
}

print.rsv_secondary <- function(x, ...) {
  cat("Full-sample secondary-calibrated RSV (RF; selected-moment calibration)\n")
  print(x$results, row.names = FALSE, ...)
  if (!is.null(x$inference)) print(x$inference, row.names = FALSE)
  invisible(x)
}

coef.rsv_secondary <- function(object, ...) object$coefficients

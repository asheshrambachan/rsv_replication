# =============================================================================
# Model Fitting Functions
# Functions for fitting prediction models (random forest, logistic regression)
# =============================================================================

#' Fit all four prediction models
#'
#' Fits models for P(Y|X,R,S_o=1), P(D|X,R,S_e=1), P(S_e|X,R), P(S_o|X,R)
#'
#' @param R_train Training remote sensing variables
#' @param X_train Training discrete covariates (optional)
#' @param Y Outcome vector (can be multi-category)
#' @param D Treatment vector (binary)
#' @param S_e Experimental sample indicator (binary)
#' @param S_o Observational sample indicator (binary)
#' @param R_test Test remote sensing variables for predictions
#' @param X_test Test discrete covariates (optional)
#' @param X_cols Column names for X covariates
#' @param y_levels Primary-outcome categories
#' @param yK Reference category for Y
#' @param models List of model specifications for Y, D, S_e, S_o
#' @param seed Random seed
#' @param num.threads Number of threads for parallel processing
#' @param keep_fits Return training data with predictions and fitted models
#'   for reuse without refitting (default FALSE).
#'
#' @return List with components:
#'   \item{predictions}{Data frame with predicted Y (all levels), D, S_e, S_o}
#'   \item{theta_init}{Initial treatment effect estimate}
#'   \item{training_data}{When keep_fits=TRUE, training observations, X and
#'     predictions, with input sample-indicator types preserved}
#'   \item{fitted_models}{When keep_fits=TRUE, fitted Y, D, S_e and S_o models}
#'
#' @keywords internal
fit_all_models <- function(R_train, X_train = NULL, Y, D, S_e, S_o,
                          R_test, X_test = NULL, X_cols = NULL, y_levels, yK = NULL,
                          models, seed = NULL, num.threads = 1, keep_fits = FALSE) {
  if (!is.logical(keep_fits) || length(keep_fits) != 1L || is.na(keep_fits)) {
    stop("keep_fits must be TRUE or FALSE.")
  }
  if (keep_fits) training_roles <- list(S_e = S_e, S_o = S_o)
  
  if (is.null(y_levels)) {
    yK <- y_levels[1]
  }
  
  # Convert to numeric if logical
  if (is.logical(S_e)) S_e <- as.numeric(S_e)
  if (is.logical(S_o)) S_o <- as.numeric(S_o)

  # Combine X and R into single design matrix
  if (!is.null(X_train) && !is.null(X_cols) && length(X_cols) > 0) {
    # Convert character columns to factors for ranger
    X_train <- as.data.frame(X_train)
    X_test <- as.data.frame(X_test)
    for (col in names(X_train)) {
      if (is.character(X_train[[col]])) {
        X_train[[col]] <- factor(X_train[[col]])
        X_test[[col]] <- factor(X_test[[col]], levels = levels(X_train[[col]]))
      }
    }
    XR_train <- cbind(X_train, R_train)
    XR_test <- cbind(X_test, R_test)
  } else {
    XR_train <- R_train
    XR_test <- R_test
  }

  # Fit Y model: P(Y | X, R, S_o=1) - multi-category
  obs_idx <- (S_o == 1)
  if (sum(obs_idx) == 0) stop("No observations in observational sample")
  
  fit_Y <- fit_one_model_multicat(
    XR_train = XR_train[obs_idx, , drop = FALSE],
    y = Y[obs_idx],
    XR_test = XR_test,
    y_levels = y_levels,
    spec = models$Y,
    seed = seed,
    num.threads = num.threads
  )

  # Fit D model: P(D | X, R, S_e=1) - binary
  exp_idx <- (S_e == 1)
  if (sum(exp_idx) == 0) stop("No observations in experimental sample")

  fit_D <- fit_one_model(
    XR_train = XR_train[exp_idx, , drop = FALSE],
    y = D[exp_idx],
    XR_test = XR_test,
    spec = models$D,
    seed = seed,
    num.threads = num.threads
  )

  # Fit S_e model: P(S_e | X, R) - binary
  fit_S_e <- fit_one_model(
    XR_train = XR_train,
    y = S_e,
    XR_test = XR_test,
    spec = models$S_e,
    seed = seed,
    num.threads = num.threads
  )

  # Fit S_o model: P(S_o | X, R) - binary
  fit_S_o <- fit_one_model(
    XR_train = XR_train,
    y = S_o,
    XR_test = XR_test,
    spec = models$S_o,
    seed = seed,
    num.threads = num.threads
  )

  # Combine predictions
  # Y predictions: pred_Y0, pred_Y1, pred_Y2, ...
  # D, S_e, S_o predictions: pred_D, pred_S_e, pred_S_o
  predictions <- cbind(
    fit_Y$predictions,  # data.frame with pred_Y0, pred_Y1, ...
    data.frame(
      pred_D = fit_D$predictions,
      pred_S_e = fit_S_e$predictions,
      pred_S_o = fit_S_o$predictions
    )
  )

  # Add X columns to predictions if present
  if (!is.null(X_test) && !is.null(X_cols) && length(X_cols) > 0) {
    predictions <- cbind(X_test, predictions)
  }

  # Compute initial theta on training data
  train_pred_Y <- .predict_model_multicat(fit_Y$model, XR_train, y_levels, models$Y)
  train_pred_D <- .predict_model(fit_D$model, XR_train, models$D)
  train_pred_S_e <- .predict_model(fit_S_e$model, XR_train, models$S_e)
  train_pred_S_o <- .predict_model(fit_S_o$model, XR_train, models$S_o)

  # Combine with observations and X (if present)
  train_df <- data.frame(
    Y = Y, D = D, S_e = S_e, S_o = S_o, 
    train_pred_Y, # data.frame with pred_Y0, pred_Y1, ...
    pred_D = train_pred_D,
    pred_S_e = train_pred_S_e,
    pred_S_o = train_pred_S_o
    )
  
  if (!is.null(X_train) && !is.null(X_cols) && length(X_cols) > 0) {
    train_df <- cbind(X_train, train_df)
  }

  # Compute theta_init using the new interface
  theta_init <- compute_theta_init(train_df, y_levels = y_levels, yK = yK, X_cols = X_cols)

  result <- list(
    predictions = predictions,
    theta_init = theta_init
  )
  if (keep_fits) {
    # Model fitting uses numeric role indicators; exposed data retain the
    # caller's original indicators, masks and row order.
    train_df[c("S_e", "S_o")] <- training_roles
    result$training_data <- train_df
    result$fitted_models <- list(
      Y = fit_Y$model, D = fit_D$model,
      S_e = fit_S_e$model, S_o = fit_S_o$model
    )
  }
  result
}


# Numeric-response RF shared by ordinary and secondary-outcome RSV.
fit_numeric_regression <- function(R, response, rf_spec, seed, num_threads,
                                   R_predict = R) {
  if (length(response) != nrow(R) || any(!is.finite(response))) {
    stop("A finite regression response is required for every training record.")
  }
  spec <- rf_spec
  if (!is.null(spec$model) && spec$model != "rf") stop("Variance/correction regression currently supports RF only.")
  spec$model <- NULL
  if (any(names(spec) %in% c("x", "y", "probability", "seed", "num.threads"))) {
    stop("Set response, seed and threads through the function arguments.")
  }
  model <- do.call(ranger::ranger, c(
    list(x = as.data.frame(R), y = response, probability = FALSE,
         seed = seed, num.threads = num_threads), spec))
  list(model = model,
       predictions = as.numeric(stats::predict(model, data = as.data.frame(R_predict))$predictions))
}

.variance_prediction_floor <- function(raw, sigma2.quantile, sigma2.lower = NULL) {
  if (length(sigma2.quantile) != 1L || !is.numeric(sigma2.quantile) ||
      !is.finite(sigma2.quantile) || sigma2.quantile < 0 || sigma2.quantile > 1) {
    stop("sigma2.quantile must be a finite number between zero and one.")
  }
  if (!is.null(sigma2.lower) && (length(sigma2.lower) != 1L ||
      !is.numeric(sigma2.lower) || !is.finite(sigma2.lower) || sigma2.lower <= 0)) {
    stop("sigma2.lower must be NULL or a positive finite scalar.")
  }
  if (!is.numeric(raw) || !is.null(dim(raw)) || !length(raw) || any(!is.finite(raw))) {
    stop("Nonfinite variance predictions.")
  }
  if (!is.null(sigma2.lower)) return(sigma2.lower)
  max(as.numeric(stats::quantile(raw, probs = sigma2.quantile)), .Machine$double.eps)
}

fit_variance_regression <- function(R, squared_residual, rf_spec, seed, num_threads,
                                    R_predict = R, sigma2.quantile = 0.01,
                                    sigma2.lower = NULL) {
  .variance_prediction_floor(1, sigma2.quantile, sigma2.lower)
  if (any(squared_residual < 0)) stop("Squared residuals must be nonnegative.")
  fit <- fit_numeric_regression(R, squared_residual, rf_spec, seed, num_threads, R_predict)
  raw_train <- as.numeric(stats::predict(fit$model, data = as.data.frame(R))$predictions)
  lower <- .variance_prediction_floor(raw_train, sigma2.quantile, sigma2.lower)
  list(model = fit$model, sigma2 = pmax(fit$predictions, lower),
       sigma2_train = pmax(raw_train, lower), lower = lower,
       n_floored = sum(fit$predictions < lower), raw_train = raw_train,
       raw = fit$predictions)
}

.resolve_variance_model <- function(sigma2.method, sigma2_model, models) {
  if (sigma2.method != "regression") return(NULL)
  spec <- sigma2_model %||% models$Y
  if (!is.list(spec) || !identical(spec$model, "rf")) {
    stop("Set sigma2_model to an RF specification for sigma2.method = 'regression'.")
  }
  # Outcome-class weights are not numeric-response regression parameters.
  spec$class.weights <- NULL
  spec
}

.variance_from_predictions <- function(df, sigma2.quantile, sigma2.lower = NULL) {
  raw <- df$pred_sigma2
  lower <- .variance_prediction_floor(raw, sigma2.quantile, sigma2.lower)
  if (any(raw < 0)) stop("Variance predictions must be nonnegative.")
  if ("pred_sigma2_floor" %in% names(df)) lower <- df$pred_sigma2_floor
  if (any(!is.finite(lower)) || any(lower <= 0)) stop("Invalid stored variance floor.")
  pmax(raw, lower)
}

# The observed full residual includes the experimental-observational cross term
# for overlapping roles. theta contains every non-reference outcome contrast.
.squared_full_residual <- function(df, theta, y_levels, yK, X_cols) {
  delta <- compute_deltas(df, y_levels, yK = yK, X_cols = X_cols, use_pred = FALSE)
  theta_cols <- paste0("theta_Y", setdiff(y_levels, yK))
  parameters <- if (length(X_cols)) {
    dplyr::left_join(df[X_cols], theta, by = X_cols)
  } else theta[rep(1L, nrow(df)), , drop = FALSE]
  if (nrow(parameters) != nrow(df) || any(!is.finite(as.matrix(parameters[theta_cols])))) {
    stop("Preliminary theta must cover every training stratum exactly once.")
  }
  residual <- delta$delta_e - rowSums(
    as.matrix(delta[paste0("delta_o_Y", setdiff(y_levels, yK))]) *
      as.matrix(parameters[theta_cols]))
  if (any(!is.finite(residual))) stop("Nonfinite training residuals; check normalization cell support.")
  as.numeric(residual^2)
}

# Add a variance prediction to a fit_all_models result. Everything used to fit
# the residual regression, including normalizers and preliminary theta, is
# training-only. Held-out X/R are used solely for prediction.
.add_regression_variance <- function(fit, R_train, R_predict, X_train, X_predict,
                                     y_levels, yK, sigma2_model, sigma2.quantile,
                                     sigma2.lower, seed, num.threads, fixed_floor = TRUE,
                                     preliminary_theta = NULL) {
  df <- fit$training_data
  X_cols <- names(fit$theta_init)[!startsWith(names(fit$theta_init), "theta_Y")]
  if (is.null(yK)) yK <- y_levels[1]
  preliminary <- if (is.null(preliminary_theta)) {
    compute_estimate(df, fit$theta_init, y_levels, sigma2 = 1)
  } else list(theta = preliminary_theta, coefficients = compute_theta0(
    compute_theta_ate(df, preliminary_theta), y_levels, yK))
  response <- .squared_full_residual(df, preliminary$theta, y_levels, yK, X_cols)
  XR_train <- if (length(X_cols)) cbind(as.data.frame(X_train), R_train) else R_train
  XR_predict <- if (length(X_cols)) cbind(as.data.frame(X_predict), R_predict) else R_predict
  for (name in X_cols) if (is.character(XR_train[[name]])) {
    XR_train[[name]] <- factor(XR_train[[name]])
    XR_predict[[name]] <- factor(XR_predict[[name]], levels = levels(XR_train[[name]]))
  }
  variance <- fit_variance_regression(XR_train, response, sigma2_model,
    if (is.null(seed)) NULL else seed + 5L, num.threads, XR_predict,
    sigma2.quantile, sigma2.lower)
  fit$predictions$pred_sigma2 <- variance$raw
  if (fixed_floor) fit$predictions$pred_sigma2_floor <- variance$lower
  fit$theta_init <- preliminary$theta
  fit$variance <- list(method = "regression", model_spec = sigma2_model,
    preliminary_theta = preliminary$theta, preliminary_effect = preliminary$coefficients,
    raw_predictions = variance$raw, raw_training_predictions = variance$raw_train,
    floor = variance$lower, floored_share = mean(variance$raw < variance$lower),
    training_n = nrow(df), floor_source = if (fixed_floor) "training" else "estimation")
  fit
}

#' Fit a single prediction model (binary outcome)
#'
#' @param XR_train Training covariates (X and R combined)
#' @param y Response variable (binary)
#' @param XR_test Test covariates
#' @param spec Model specification (list with model type and parameters)
#' @param seed Random seed
#' @param num.threads Number of threads
#'
#' @return List with predictions and fitted model
#'
#' @keywords internal
fit_one_model <- function(XR_train, y, XR_test, spec, seed = NULL, num.threads = 1) {

  if (spec$model == "rf") {
    return(fit_randomforest(XR_train, y, XR_test, spec, seed, num.threads))
  } else if (spec$model == "logit") {
    return(fit_logistic(XR_train, y, XR_test))
  } else if (spec$model == "xgb") {
    return(fit_xgboost(XR_train, y, XR_test, spec, seed))
  } else {
    stop("Unknown model type: ", spec$model)
  }
}


#' Fit a single prediction model for multi-category outcome
#'
#' @param XR_train Training covariates (X and R combined)
#' @param y Response variable (multi-category)
#' @param XR_test Test covariates
#' @param y_levels Levels of y
#' @param spec Model specification (list with model type and parameters)
#' @param seed Random seed
#' @param num.threads Number of threads
#'
#' @return List with predictions (data.frame with pred_Y0, pred_Y1, ...) and fitted model
#'
#' @keywords internal
fit_one_model_multicat <- function(XR_train, y, XR_test, y_levels, spec, seed = NULL, num.threads = 1) {
  if (spec$model == "rf") {
      return(fit_randomforest_multicat(XR_train, y, XR_test, y_levels, spec, seed, num.threads))
  } else if (spec$model == "logit") {
      return(fit_logistic_multicat(XR_train, y, XR_test, y_levels))
  } else if (spec$model == "xgb") {
      return(fit_xgboost_multicat(XR_train, y, XR_test, y_levels, spec, seed))
  } else {
    stop("Unknown model type: ", spec$model)
  }
}


#' Fit random forest model (binary outcome)
#'
#' @param XR_train Training covariates
#' @param y Response variable (binary)
#' @param XR_test Test covariates
#' @param spec Model specification
#' @param seed Random seed
#' @param num.threads Number of threads
#'
#' @return List with predictions and model
#'
#' @keywords internal
fit_randomforest <- function(XR_train, y, XR_test, spec, seed = NULL, num.threads = 1) {

  # Keep as data.frame if it has factor columns, otherwise convert to matrix
  if (is.data.frame(XR_train) && any(sapply(XR_train, is.factor))) {
    XR_train <- as.data.frame(XR_train)
    XR_test <- as.data.frame(XR_test)
  } else {
    XR_train <- as.matrix(XR_train)
    XR_test <- as.matrix(XR_test)
  }

  # Build parameter list for ranger
  params <- list(
    x = XR_train,
    y = factor(y, levels = c(0, 1)),
    num.trees = spec$num.trees %||% 500,
    probability = TRUE,
    num.threads = num.threads
  )

  # Add mtry if specified
  if (!is.null(spec$mtry)) {
    params$mtry <- spec$mtry
  }

  # Add class weights if specified
  if (!is.null(spec$class.weights)) {
    params$class.weights <- spec$class.weights
  }

  # Add seed if specified
  if (!is.null(seed)) {
    params$seed <- seed
  }

  # Add any other ranger-specific parameters from spec
  extra_params <- setdiff(
    names(spec),
    c("model", "num.trees", "mtry", "class.weights")
  )
  for (param in extra_params) {
    params[[param]] <- spec[[param]]
  }

  # Fit model
  model <- do.call(ranger::ranger, params)

  # Predict on test set - P(Y=1)
  pred <- stats::predict(model, XR_test)$predictions[, "1"]

  list(predictions = pred, model = model)
}


#' Fit random forest model (multi-category outcome)
#'
#' @param XR_train Training covariates
#' @param y Response variable (multi-category)
#' @param XR_test Test covariates
#' @param y_levels Levels of y
#' @param spec Model specification
#' @param seed Random seed
#' @param num.threads Number of threads
#'
#' @return List with predictions (data.frame with pred_Y0, pred_Y1, ...) and model
#'
#' @keywords internal
fit_randomforest_multicat <- function(XR_train, y, XR_test, y_levels, spec, seed = NULL, num.threads = 1) {

  # Keep as data.frame if it has factor columns, otherwise convert to matrix
  if (is.data.frame(XR_train) && any(sapply(XR_train, is.factor))) {
    XR_train <- as.data.frame(XR_train)
    XR_test <- as.data.frame(XR_test)
  } else {
    XR_train <- as.matrix(XR_train)
    XR_test <- as.matrix(XR_test)
  }
  
  if (!is.factor(y)) {
    y = factor(y, levels = y_levels)
  }
  
  # Build parameter list for ranger
  # Use all y_levels to maintain consistent output structure
  params <- list(
    x = XR_train,
    y = y,
    num.trees = spec$num.trees %||% 500,
    probability = TRUE,
    num.threads = num.threads
  )

  # Add mtry if specified
  if (!is.null(spec$mtry)) {
    params$mtry <- spec$mtry
  }

  # Add class weights if specified
  if (!is.null(spec$class.weights)) {
    params$class.weights <- spec$class.weights
  }

  # Add seed if specified
  if (!is.null(seed)) {
    params$seed <- seed
  }

  # Add any other ranger-specific parameters from spec
  extra_params <- setdiff(
    names(spec),
    c("model", "num.trees", "mtry", "class.weights")
  )
  for (param in extra_params) {
    params[[param]] <- spec[[param]]
  }

  # Fit model
  model <- do.call(ranger::ranger, params)

  # Predict on test set - get probabilities for all levels
  pred_mat <- stats::predict(model, XR_test)$predictions

  # Convert to data.frame with column names pred_Y0, pred_Y1, ...
  pred_df <- as.data.frame(pred_mat)
  colnames(pred_df) <- paste0("pred_Y", y_levels)

  list(predictions = pred_df, model = model)
}


#' Fit logistic regression model (binary outcome)
#'
#' @param XR_train Training covariates
#' @param y Response variable (binary)
#' @param XR_test Test covariates
#'
#' @return List with predictions and model
#'
#' @keywords internal
fit_logistic <- function(XR_train, y, XR_test) {

  # Convert to data.frame and handle factors using model.matrix
  XR_train <- as.data.frame(XR_train)
  XR_test <- as.data.frame(XR_test)

  # Create formula for model.matrix to handle factors properly
  # Use all columns, model.matrix will create dummy variables for factors
  formula_rhs <- paste(names(XR_train), collapse = " + ")
  formula_obj <- as.formula(paste("~", formula_rhs))

  # Create design matrices (model.matrix includes intercept by default)
  X_train <- model.matrix(formula_obj, data = XR_train)
  X_test <- model.matrix(formula_obj, data = XR_test)

  # Fit logistic regression (X_train already has intercept from model.matrix)
  model <- fastglm::fastglm(
    x = X_train,
    y = y,
    family = stats::binomial(),
    method = 0  # Use Cholesky decomposition
  )

  # Predict on test set - P(Y=1)
  eta <- as.numeric(X_test %*% model$coefficients)
  pred <- 1 / (1 + exp(-eta))

  list(predictions = pred, model = model)
}


#' Fit multinomial logistic regression (multi-category outcome)
#'
#' @param XR_train Training covariates
#' @param y Response variable (multi-category)
#' @param XR_test Test covariates
#' @param y_levels Levels of y
#'
#' @return List with predictions (data.frame with pred_Y0, pred_Y1, ...) and model
#'
#' @keywords internal
fit_logistic_multicat <- function(XR_train, y, XR_test, y_levels) {

  # Keep as data.frame to handle factors properly
  y <- factor(match(y, y_levels), levels = seq_along(y_levels))  # class index 1..K
  XR_train <- as.data.frame(XR_train)
  XR_test <- as.data.frame(XR_test)

  # Fit multinomial logistic regression
  model <- nnet::multinom(y ~ ., data = XR_train, trace = FALSE, MaxNWts = 1052)

  # Predict on test set
  pred_mat_present <- stats::predict(model, newdata = XR_test, type = "probs")
  
  # Handle binary case for present levels: multinom returns vector P(Y=1) instead of matrix
  if (is.null(dim(pred_mat_present))) {
    pred_mat_present <- cbind(`0` = 1 - pred_mat_present, `1` = pred_mat_present)
  }
  
  # Map predictions from present levels to all levels
  pred_mat <- matrix(NA, nrow = nrow(XR_test), ncol = length(y_levels))
  y_levels_present <- y_levels[as.numeric(as.character(model$lev))]
  for (i in seq_along(y_levels_present)) {
    y_level <- y_levels_present[i]
    idx_in_all <- which(y_levels == y_level)
    pred_mat[, idx_in_all] <- pred_mat_present[, i]
  }
  
  # Convert to data.frame with column names pred_Y0, pred_Y1, ...
  pred_df <- as.data.frame(pred_mat)
  colnames(pred_df) <- paste0("pred_Y", y_levels)
  list(predictions = pred_df, model = model)
}


# =============================================================================
# XGBoost Model Functions
# =============================================================================

#' Fit XGBoost model (binary outcome)
#'
#' @param XR_train Training covariates
#' @param y Response variable (binary 0/1)
#' @param XR_test Test covariates
#' @param spec Model specification (list with xgb parameters)
#' @param seed Random seed
#'
#' @return List with predictions and model
#'
#' @keywords internal
fit_xgboost <- function(XR_train, y, XR_test, spec, seed = NULL) {

  XR_train <- as.matrix(XR_train)
  XR_test <- as.matrix(XR_test)

  dtrain <- xgboost::xgb.DMatrix(data = XR_train, label = as.numeric(y))
  dtest <- xgboost::xgb.DMatrix(data = XR_test)

  # Build parameter list
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = spec$max_depth %||% 6,
    eta = spec$eta %||% 0.3,
    subsample = spec$subsample %||% 0.8,
    colsample_bytree = spec$colsample_bytree %||% 0.8
  )

  # GPU support: device = "cuda" for xgboost >= 2.0
  if (!is.null(spec$device)) {
    params$device <- spec$device
    params$tree_method <- "hist"
  }

  if (!is.null(seed)) {
    params$seed <- seed
  }

  nrounds <- spec$nrounds %||% 100

  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )

  pred <- stats::predict(model, dtest)

  list(predictions = pred, model = model)
}


#' Fit XGBoost model (multi-category outcome)
#'
#' @param XR_train Training covariates
#' @param y Response variable (multi-category)
#' @param XR_test Test covariates
#' @param y_levels Levels of y
#' @param spec Model specification
#' @param seed Random seed
#'
#' @return List with predictions (data.frame with pred_Y0, pred_Y1, ...) and model
#'
#' @keywords internal
fit_xgboost_multicat <- function(XR_train, y, XR_test, y_levels, spec, seed = NULL) {

  XR_train <- as.matrix(XR_train)
  XR_test <- as.matrix(XR_test)

  # XGBoost needs 0-indexed integer labels
  y_int <- match(as.character(y), as.character(y_levels)) - 1L
  num_class <- length(y_levels)

  dtrain <- xgboost::xgb.DMatrix(data = XR_train, label = y_int)
  dtest <- xgboost::xgb.DMatrix(data = XR_test)

  if (num_class == 2) {
    # Binary case: use logistic, then construct 2-column probabilities
    params <- list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = spec$max_depth %||% 6,
      eta = spec$eta %||% 0.3,
      subsample = spec$subsample %||% 0.8,
      colsample_bytree = spec$colsample_bytree %||% 0.8
    )
  } else {
    # Multi-class: use softprob
    params <- list(
      objective = "multi:softprob",
      num_class = num_class,
      eval_metric = "mlogloss",
      max_depth = spec$max_depth %||% 6,
      eta = spec$eta %||% 0.3,
      subsample = spec$subsample %||% 0.8,
      colsample_bytree = spec$colsample_bytree %||% 0.8
    )
  }

  # GPU support
  if (!is.null(spec$device)) {
    params$device <- spec$device
    params$tree_method <- "hist"
  }

  if (!is.null(seed)) {
    params$seed <- seed
  }

  nrounds <- spec$nrounds %||% 100

  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )

  raw_pred <- stats::predict(model, dtest)

  if (num_class == 2) {
    # raw_pred is P(class=1); construct both columns
    pred_mat <- cbind(1 - raw_pred, raw_pred)
  } else {
    # raw_pred is a flat vector: reshape to n_test x num_class
    pred_mat <- matrix(raw_pred, ncol = num_class, byrow = TRUE)
  }

  pred_df <- as.data.frame(pred_mat)
  colnames(pred_df) <- paste0("pred_Y", y_levels)

  # Store y_levels and num_class on model for prediction
  attr(model, "y_levels") <- y_levels
  attr(model, "num_class") <- num_class

  list(predictions = pred_df, model = model)
}


#' Predict from fitted model (binary outcome - internal)
#'
#' Internal function for predicting from different model types
#'
#' @param model Fitted model (ranger or fastglm)
#' @param XR_new New covariates
#' @param spec Model specification (for determining model type)
#'
#' @return Predicted probabilities
#'
#' @keywords internal
.predict_model <- function(model, XR_new, spec) {
  if (inherits(model, "ranger")) {
    # Preserve factor covariates, as in the RF fitting and multicategory paths.
    if (!(is.data.frame(XR_new) && any(vapply(XR_new, is.factor, logical(1))))) {
      XR_new <- as.matrix(XR_new)
    }
    return(stats::predict(model, XR_new)$predictions[, "1"])
  }
  XR_new <- as.matrix(XR_new)
  if (inherits(model, "fastglm")) {
    X_new <- cbind(1, XR_new)
    eta <- as.numeric(X_new %*% model$coefficients)
    return(1 / (1 + exp(-eta)))
  } else if (inherits(model, "xgb.Booster")) {
    dmat <- xgboost::xgb.DMatrix(data = as.matrix(XR_new))
    return(stats::predict(model, dmat))
  } else {
    stop("Unknown model class: ", class(model)[1])
  }
}


#' Predict from fitted model (multi-category outcome - internal)
#'
#' Internal function for predicting from different model types for multi-category outcomes
#'
#' @param model Fitted model (ranger or multinom)
#' @param XR_new New covariates
#' @param y_levels Levels of y
#' @param spec Model specification (for determining model type)
#'
#' @return Data frame with predicted probabilities (pred_Y0, pred_Y1, ...)
#'
#' @keywords internal
.predict_model_multicat <- function(model, XR_new, y_levels, spec) {
  if (inherits(model, "ranger")) {
    # Keep as data.frame if it has factors, otherwise convert to matrix
    if (is.data.frame(XR_new) && any(sapply(XR_new, is.factor))) {
      XR_new <- as.data.frame(XR_new)
    } else {
      XR_new <- as.matrix(XR_new)
    }
    pred_mat <- stats::predict(model, XR_new)$predictions

    # Ranger should return predictions for all levels specified during training
    # But verify the column count matches y_levels
    if (ncol(pred_mat) != length(y_levels)) {
      warning("Ranger prediction matrix has ", ncol(pred_mat), " columns but y_levels has ", length(y_levels), " levels")
    }

    pred_df <- as.data.frame(pred_mat)
    colnames(pred_df) <- paste0("pred_Y", y_levels)
    return(pred_df)
    
  } else if (inherits(model, "multinom")) {
    XR_new <- as.data.frame(XR_new)
    pred_mat_present <- stats::predict(model, newdata = XR_new, type = "probs")

    if (is.null(dim(pred_mat_present))) {
      pred_mat_present <- cbind(`0` = 1 - pred_mat_present, `1` = pred_mat_present)
    }

    # Map predictions from present levels to all levels
    pred_mat <- matrix(0, nrow = nrow(XR_new), ncol = length(y_levels))
    y_levels_present <- y_levels[as.numeric(as.character(model$lev))]
    for (i in seq_along(y_levels_present)) {
      y_level <- y_levels_present[i]
      idx_in_all <- which(y_levels == y_level)
      pred_mat[, idx_in_all] <- pred_mat_present[, i]
    }

    pred_df <- as.data.frame(pred_mat)
    colnames(pred_df) <- paste0("pred_Y", y_levels)
    return(pred_df)

  } else if (inherits(model, "xgb.Booster")) {
    dmat <- xgboost::xgb.DMatrix(data = as.matrix(XR_new))
    raw_pred <- stats::predict(model, dmat)
    num_class <- attr(model, "num_class") %||% length(y_levels)

    if (num_class == 2) {
      pred_mat <- cbind(1 - raw_pred, raw_pred)
    } else {
      pred_mat <- matrix(raw_pred, ncol = num_class, byrow = TRUE)
    }

    pred_df <- as.data.frame(pred_mat)
    colnames(pred_df) <- paste0("pred_Y", y_levels)
    return(pred_df)

  } else {
    stop("Unknown model class: ", class(model)[1])
  }
}

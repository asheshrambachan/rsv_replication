# =============================================================================
# RSV Helper Functions
# =============================================================================

## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(boot)
})
source("code/poverty/utils/common.R")


#' Compute joint probabilities
get_joint <- function(x) {
  with(x, list(
      D1Se =      D_Se  * Se, # P(D=1 | S=e, X) × P(S=e | X)
      D0Se = (1 - D_Se) * Se, # P(D=0 | S=e, X) × P(S=e | X)
      Y1So =      Y_So  * So, # P(Y=1 | S=o, X) × P(S=o | X)
      Y0So = (1 - Y_So) * So # P(Y=0 | S=o, X) × P(S=o | X)
  ))
}


#' Sum marginal probabilities
count_marginals <- function(x) {
  j <- get_joint(x)
  lapply(j, mean) # ∑P(D, S | X)
}


#' Compute deltas/variations
get_Delta <- function(x, count) {
  j <- get_joint(x)
  list(
    e = j$D1Se / count$D1Se - j$D0Se / count$D0Se, # treatment variations from the Se
    o = j$Y1So / count$Y1So - j$Y0So / count$Y0So # outcome variations from the So
  )
}


#' Compute sigma^2
get_sigma2 <- function(x, count, coef, eps=1e-2) {
  j <- get_joint(x)
  sigma2 <- (j$D1Se / count$D1Se^2 + j$D0Se / count$D0Se^2) + 
    coef^2 * (j$Y1So / count$Y1So^2 + j$Y0So / count$Y0So^2)
  
  # Lower bound on sigma for numerical stability
  sigma2 <- pmax(sigma2, eps) 
  
  return(sigma2)
}


#' Algorithm 1, Step 2: Train predictors and compute initial coefficient
rsv_fit_train <- function(X, D, Y, Se, So, X_test, classwt = c(10,1), ntree = 100){
  Se <- to_logical(Se)
  So <- to_logical(So)
  train <- data.frame(
    D_Se = as.integer((D == 1) & Se), 
    Y_So = as.integer((Y == 1) & So), 
    Se = Se,
    So = So
  )
  
  # Step 2a
  count <- count_marginals(train)
  
  # Step 2b
  model_Y_So <- randomForest(x = X[So, , drop = FALSE], y = factor(Y[So], levels = c(0, 1)), classwt = classwt, ntree = ntree)
  model_D_Se <- randomForest(x = X[Se, , drop = FALSE], y = factor(D[Se], levels = c(0, 1)), ntree = ntree)
  model_So <- randomForest(x = X, y = factor(So, levels = c(FALSE, TRUE)), ntree = ntree)
  
  train_pred <- data.frame(
    D_Se = predict(model_D_Se, X, type = "prob")[, 2],
    Y_So = predict(model_Y_So, X, type = "prob")[, 2],
    Se   = Se,
    So   = So
  )
  test_pred <- data.frame(
    D_Se = predict(model_D_Se, X_test, type = "prob")[, 2], 
    Y_So = predict(model_Y_So, X_test, type = "prob")[, 2],
    Se   = rep(TRUE, nrow(X_test)),
    So   = predict(model_So,   X_test, type = "prob")[, 2]
  )
  
  Delta <- get_Delta(train_pred, count)
  coef_init <- mean(Delta$e * Delta$o) / mean(Delta$o^2)
  list(coef_init = coef_init, test_pred = test_pred)
}


#' Algorithm 1, Step 3: Compute RSV Estimator
rsv_fit_test <- function(D, Y, Se, So, pred, coef_init, eps){
  Se <- to_logical(Se)
  So <- to_logical(So)
  test <- data.frame(
    D_Se = as.integer((D == 1) & Se), # D * 1{S = e}
    Y_So = as.integer((Y == 1) & So), # Y * 1{S = o}
    Se = Se, So = So
  )
  
  # Step 3a
  count <- count_marginals(test)
  
  # Step 2d
  sigma2 <- get_sigma2(pred, count = count, coef = coef_init, eps = eps)
  Delta_pred <- get_Delta(pred, count = count)
  H <- Delta_pred$o / sigma2
  
  # Step 3b
  Delta <- get_Delta(test, count = count)
  numerator <- mean(Delta$e * H)
  denominator <- mean(Delta$o * H)
  coef <- numerator / denominator
  
  list(coef = coef, denominator = denominator, sigma2 = sigma2, H = H, Delta = Delta)
}


#' Algorithm 1, Step 3c: RSV Bootstrapping Function
rsv_boot_fun <- function(D, Y, Se, So, pred, clusters, coef_init, eps, B, cores) {
  
  # Combine all relevant variables into a single data.frame
  boot_data <- data.frame(D, Y, Se_true=Se, So_true=So, pred, clusters)

  # Run cluster-level bootstrap
  boot_out <- boot(
    statistic = function(d, coef_init, eps){
      pred <- select(d, names(pred))
      out <- rsv_fit_test(
        D=d$D, Y=d$Y, Se=d$Se_true, So=d$So_true, pred=pred,
        coef_init=coef_init, eps=eps
      )
      
      c(coef = out$coef, denominator = out$denominator)
    },
    ran.gen = function(d, mle) cluster_sample(d, cluster_var="clusters"),
    data = boot_data,
    coef_init = coef_init,
    eps = eps,
    R = B,
    sim = "parametric",
    parallel = "multicore",
    ncpus = cores
  )
  
  coef_names <- names(boot_out$t0)
  coef_id <- which(coef_names == "coef")
  denominator_id <- which(coef_names == "denominator")
  
  # Return standard errors
  list(
    se = sd(boot_out$t[, coef_id]),
    denominator_se = sd(boot_out$t[, denominator_id])
  )
}
# =============================================================================
# Surrogate Estimator & Bootstrapping
# =============================================================================

## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(randomForest)
})
source("code/poverty/utils/surrogate_helpers.R")


#' Generic surrogate function
surrogate_fun <- function(...){
  UseMethod("surrogate_fun")
}


#' Default surrogate: trains RF
surrogate_fun.default <- function(
    X_train, Y_train, So_train, X_test, D_test, 
    classwt = c(50,1), ntree = 100,
    se.boot = FALSE, clusters_test = NULL, B = 1000, cores = 1
  ){
  if ((se.boot) && is.null(clusters_test)) stop("clusters_test required for SEs")
  
  So_train <- to_logical(So_train)
  
  model_Y_So <- randomForest(X_train[So_train, ], factor(Y_train[So_train], levels = c(0, 1)), classwt = classwt, ntree = ntree)
  
  Y_test_pred <- predict(model_Y_So, X_test, type = "prob")[, 2]
  
  # Estimate surrogate coef
  results <- list(
    coef = surrogate_coef(D_test, Y_test_pred),
    D_test = D_test,
    Y_test_pred = unname(Y_test_pred)
  )
  
  # Optional bootstrap for SEs
  if (se.boot){
    boot_out <- surrogate_boot_fun(D_test, Y_test_pred, clusters_test, B, cores)
    results <- c(results, boot_out)
  }
  
  return(results)
}


#' RSV-based surrogate
surrogate_fun.rsv <- function(rsv, se.boot = FALSE, B = 1000, cores = 1){
  D_test <- rsv$D_test
  Y_test_pred <- rsv$Y_test_pred
  clusters_test <- rsv$clusters_test
  
  # Estimate surrogate coef
  results <- list(
    coef = surrogate_coef(D_test, Y_test_pred),
    D_test = D_test,
    Y_test_pred = unname(Y_test_pred)
  )
  
  # Optional bootstrap for SEs
  if (se.boot){
    boot_out <- surrogate_boot_fun(D_test, Y_test_pred, clusters_test, B, cores)
    results <- c(results, boot_out)
  }
  
  class(results) <- "surrogate"
  return(results)
}

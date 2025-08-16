# =============================================================================
# RSV Estimator & Bootstrapping
# =============================================================================

## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(randomForest)
})
source("code/poverty/utils/rsv_helpers.R")


#' RSV estimation function
rsv_fun <- function(
    # first step fun arg
    X_train, D_train, Y_train, Se_train, So_train,
    X_test, D_test, Y_test, Se_test, So_test,
    classwt = c(10, 1), ntree = 100, eps = 1e-2, 
    se.boot = FALSE, clusters_test = NULL, B = 1000, cores = 1 # boot arg
    ) {
  
  if ((se.boot) && is.null(clusters_test)) stop("clusters_test required for SEs")
  
  # Algorithm 1, Step 2: Train random forests and get initial coefficient + test predictions
  step1 <- rsv_fit_train(X_train, D_train, Y_train, Se_train, So_train, X_test, classwt, ntree)
  
  # Algorithm 1, Step 3: Estimate RSV coefficient on test data
  step2 <- rsv_fit_test(D_test, Y_test, Se_test, So_test, step1$test_pred, step1$coef_init, eps)

  # Assemble outputs
  results <- c(
    step2,
    list(
      coef_init = step1$coef_init, 
      D_test = D_test,
      Y_test_pred = step1$test_pred$Y_So,
      clusters_test = clusters_test
      )
  )

  # Step 3c: Optional bootstrap for SEs
  if (se.boot) {
    boot_se <- rsv_boot_fun(
      D_test, Y_test, Se_test, So_test,
      step1$test_pred, clusters_test,
      step1$coef_init, eps, B, cores
    )
    results <- c(results, boot_se)
  }
  
  class(results) <- "rsv"
  return(results)
}

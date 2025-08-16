# =============================================================================
# Surrogate Helper Functions
# =============================================================================

## Load libraries
suppressPackageStartupMessages({
  library(boot)
})
source("code/poverty/utils/common.R")


#' Compute Surrogate Estimator
surrogate_coef <- function(D, Y_pred){
  Y_D1 = mean(Y_pred * D, na.rm = T) / mean(D, na.rm = T)
  Y_D0 = mean(Y_pred * (1-D), na.rm = T) / mean(1-D, na.rm = T)
  coef = Y_D1 - Y_D0
  return(coef)
}


#' Surrogate Bootstrapping Function
surrogate_boot_fun <- function(D, Y_pred, clusters, B, cores) {
  # Combine all relevant variables into a single data.frame
  boot_data <- data.frame(D, Y_pred, clusters)

  # Run cluster-level bootstrap
  boot_out <- boot(
    statistic = function(d) surrogate_coef(D = d$D, Y_pred = d$Y_pred),
    ran.gen = function(d, mle) cluster_sample(d, cluster_var="clusters"),
    data = boot_data,
    R = B,
    sim = "parametric",
    parallel = "multicore",
    ncpus = cores
  )
  
  # Return standard error
  list(se = sd(boot_out$t))
}

suppressPackageStartupMessages({
  library(boot)
})
source("code/antipoverty/utils/first_step_fun.R")
source("code/antipoverty/utils/second_step_fun.R")
source("code/antipoverty/utils/cluster_sample.R")

rsv_fun <- function(
    # first step fun arg
    X_train = NULL, D_train = NULL, Y_train = NULL, 
    X_test = NULL, D_test = NULL, Y_test = NULL, 
    classwt = c(10, 1), ntree = 100,
    delta = 0.01, # second step fun arg
    se.boot = TRUE, clusters_test = NULL, B=1000 # boot arg
    ) {
  
  if ((se.boot) & is.null(clusters_test))
    stop("se.boot is true but no clusters_test was passed.")
  
  first_step <- first_step_fun(X_train, D_train, Y_train, X_test, classwt, ntree) 
  Y_test_pred <- first_step$pred_test$Y
  
  second_step <- second_step_fun(
    D_test = D_test, Y_test = Y_test, pred_test = first_step$pred_test, 
    theta_1st = first_step$theta_1st, delta = delta)
  
  out <- list(
    coef = second_step$theta_2nd, # same as theta_2nd
    theta_1st = first_step$theta_1st,
    theta_2nd = second_step$theta_2nd,
    denominator = second_step$denominator,
    H = second_step$H,
    Y_test_pred = Y_test_pred
  )
  
  # Compute standard errors via bootstrap 
  if (se.boot) {
    second_step_boot <- boot(
      statistic = second_step_fun,
      ran.gen = cluster_sample,
      data = data.frame(
        D_test, 
        Y_test, 
        first_step$pred_test, 
        clusters_test
        ),
      theta_1st = first_step$theta_1st,   
      delta = delta, 
      mle = list(cluster_var="clusters_test"),
      R = B,
      parallel = "multicore",
      sim = "parametric"
    ) 
    
    # Identify coefficients index
    coef_names <- names(second_step_boot$t0)
    theta_2nd_index <- which(coef_names == "theta_2nd")
    denominator_index <- which(coef_names == "denominator")
    
    out$se <- sd(second_step_boot$t[, theta_2nd_index]) # same as theta_2nd_se
    out$theta_2nd_se <- sd(second_step_boot$t[, theta_2nd_index])
    out$denominator_se <- sd(second_step_boot$t[, denominator_index])
  }
  
  return(out)
}
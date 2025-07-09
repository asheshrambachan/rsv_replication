suppressPackageStartupMessages({
  library(boot)
})

surrogate_coef <- function(D, Y){
  Y_given_D1 = mean(Y * D, na.rm = T) / mean(D, na.rm = T)
  Y_given_D0 = mean(Y * (1-D), na.rm = T) / mean(1-D, na.rm = T)
  theta = Y_given_D1 - Y_given_D0
  return(theta)
}

surrogate_fun <- function(D, Y, clusters, se.boot = FALSE, B = 1000){
  
  if ((se.boot) & is.null(clusters))
    stop("se.boot is true but no clusters was passed.")
  
  out <- list(coef = surrogate_coef(D = D, Y = Y))
  
  if (se.boot){
    coef_boot <- boot(
      statistic = surrogate_coef,
      ran.gen = cluster_sample,
      data = data.frame(
        D = D, 
        Y = Y, 
        clusters = clusters
      ),
      mle = list(cluster_var="clusters"),
      R = B,
      parallel = "multicore",
      sim = "parametric"
    ) 
    out$se <- sd(coef_boot$t)
  }
  
  return(out)
} 
suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
  
  library(readr)
  library(doParallel)  
})
source("code/poverty/utils/cv_rsv_surrogate.R")

cores <- min(100, detectCores() - 1) # Use all but one core

data <- read_csv("data/processed/poverty/data.csv", show_col_types = F)

Y <- data$Ycons

D <- data$D

X <- data %>% 
  select(
    starts_with("viirs_annual_"),
    paste0("feature_", 1:1000)
  )

for(n in c(1000, 2000, 3000)){
  for(tau in seq(from = 0, to = 0.5, by = 0.1)){
    
    # Set up parallel backend
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    
    B <- 500
    out <- foreach(
      b = c(1:B), 
      .packages = c("boot", "randomForest")
    ) %dopar% {
      set.seed(b)
      draw <- DGP(n = n, tau = tau, X = X, D = D, Y = Y) 

      set.seed(123)
      rsv_surrogate_cv(X = draw$X, D = draw$D, Y = draw$Y, nfold = 5, se.boot = F)
    }
    
    # Stop the parallel cluster
    stopCluster(cl)
    
    TE = mean(Y[D == 1], na.rm = T) + tau - mean(Y[D == 0], na.rm = T)
    
    rsv <- list(
      TE = TE,
      coef_cv_boot = sapply(out, function(x) x$rsv$coef_cv),
      out = lapply(out, function(x) x$rsv)
    )
    
    output_path <- sprintf("data/raw/antipoverty/simulations_correct/rsv_nfeat%s_nfold5_B%s_n%s_tau%.1f.rds", ncol(X), B, n, tau)
    dir.create(dirname(output_path), recursive = T, showWarnings = F)
    saveRDS(rsv, output_path)
    cat(sprintf("Saved results to: %s\n", output_path))
    
    surrogate <- list(
      TE = TE,
      coef_cv_boot = sapply(out, function(x) x$surrogate$coef_cv),
      out = lapply(out, function(x) x$surrogate)
    )
    
    output_path <- sprintf("data/raw/antipoverty/rsv_vs_surrogate/surrogate_nfeat%s_nfold5_B%s_n%s_tau%.1f.rds", ncol(X), B, n, tau)
    dir.create(dirname(output_path), recursive = T, showWarnings = F)
    saveRDS(surrogate, output_path)
    cat(sprintf("Saved results to: %s\n", output_path))
  }
}
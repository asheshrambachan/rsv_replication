suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
  library(doParallel)  
})
source("code/antipoverty/utils/rsv_surrogate_cv.R")
source("code/antipoverty/utils/DGP.R")

cores <- 100 #detectCores() - 1 # Use all but one core

data <- read.csv("data/clean/antipoverty/data.csv") %>%
  filter(
    tot_p >= 100,
    !(is.na(Y05k) | is.na(Y10k) | is.na(Ycons))
    )

D <- data$D

nfeatures <- 1000 # 1001
X <- data %>% 
  select(starts_with("feature_")) %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
X <- X[, 1:nfeatures] 

Y <- data$Ycons
for(n in c(1000, 2000, 3000)){
  for(tau in seq(from = 0, to = 0.5, by = 0.1)){
    
    # Set up parallel backend
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    
    B <- 500
    out <- foreach(
      b = c(1:B), 
      # .export = c(
      #   "DGP", "rsv_surrogate_cv", 
      #   "rsv_fun", "first_step_fun", "second_step_fun",
      #   "surrogate_fun", "surrogate_coef",
      #   "cluster_sample",
      #   "n", "tau", "X", "D", "Y"
      #   ),
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
    
    output_path <- sprintf("data/raw/antipoverty/simulations_correct_sameDGP/rsv_nfold5_B%s_n%s_tau%.1f.rds", B, n, tau)
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(rsv, output_path)
    cat(sprintf("Saved results to: %s\n", output_path))
    
    surrogate <- list(
      TE = TE,
      coef_cv_boot = sapply(out, function(x) x$surrogate$coef_cv),
      out = lapply(out, function(x) x$surrogate)
    )
    
    output_path <- sprintf("data/raw/antipoverty/simulations_correct_sameDGP/surrogate_nfold5_B%s_n%s_tau%.1f.rds", B, n, tau)
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(surrogate, output_path)
    cat(sprintf("Saved results to: %s\n", output_path))
  }
}
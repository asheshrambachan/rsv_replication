source("code/poverty/utils/rsv_fun.R")
source("code/poverty/utils/surrogate_fun.R")

# calibrated, synthetic data generating process
DGP <- function(n, X, D, Y, tau = 0) {
  prob_Y1_D1 <- mean(Y[D == 1], na.rm = T) + tau
  prob_Y1_D0 <- mean(Y[D == 0], na.rm = T)
  Y_D1 <- rbinom(n, 1, prob = prob_Y1_D1)
  Y_D0 <- rbinom(n, 1, prob = prob_Y1_D0)
  
  Y1_id_draw <- sample(which(Y == 1), size = n, replace = T)
  Y0_id_draw <- sample(which(Y == 0), size = n, replace = T)
  X_Y1 <- X[Y1_id_draw, ] 
  X_Y0 <- X[Y0_id_draw, ]
  
  D <- rbinom(n, 1, prob = 0.5)
  Y <- D * Y_D1 + (1 - D) * Y_D0
  X <- Y * X_Y1 + (1 - Y) * X_Y0
  
  # mimic a setting with missing outcomes, we delete Y if D=1
  S <- ifelse(D == 1, "e", "o")
  Y <- ifelse(S == "e", NA, Y)
  
  return(list(
    X = X,
    D = D,
    Y = Y
  ))
}

rsv_surrogate_cv <- function(
    X, D, Y, nfold = 5, 
    clusters = NULL, se.boot = FALSE, ...
  ){
  
  folds <- sample(1:nfold, size = length(D), replace = T)
  
  rsv <- list()
  surrogate <- list()
  for (k in 1:nfold){
    train_ids <- folds != k
    test_ids <- folds == k
    
    X_train <- X[train_ids, ]
    X_test <- X[test_ids, ]
    
    D_train <- D[train_ids] 
    D_test <- D[test_ids]
    
    Y_train <- Y[train_ids] 
    Y_test <- Y[test_ids]  
    
    if (!is.null(clusters))
      clusters_test <- clusters[test_ids]
    else
      clusters_test <- NULL
    
    rsv[[length(rsv) + 1]] <- rsv_fun(
      X_train = X_train, D_train = D_train, Y_train = Y_train,
      X_test = X_test, D_test = D_test, Y_test = Y_test, 
      clusters_test = clusters_test, se.boot = se.boot, ...
    )
    
    Y_test_pred <- rsv[[length(rsv)]]$Y_test_pred
    
    surrogate[[length(surrogate) + 1]] <- surrogate_fun(
      D = D_test, Y = Y_test_pred, 
      clusters = clusters_test, se.boot = se.boot, ...
    )
    
  }
  
  rsv <- list(
    coef_cv = mean(sapply(rsv, function(x) x$coef)),
    out = rsv
  )
  
  surrogate <- list(
    coef_cv = mean(sapply(surrogate, function(x) x$coef)),
    out = surrogate
  )
  
  return(list(rsv = rsv, surrogate = surrogate))
}
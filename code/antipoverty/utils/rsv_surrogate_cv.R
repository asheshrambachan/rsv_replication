source("code/antipoverty/utils/rsv_fun.R")
source("code/antipoverty/utils/surrogate_fun.R")

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
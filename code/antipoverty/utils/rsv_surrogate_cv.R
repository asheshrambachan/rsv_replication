rsv_surrogate_cv <- function(
    X, D, Y, nfolds = 5, 
    clusters = NULL, se.boot = FALSE, ...
  ){
  
  folds <- sample(c(1:nfolds), size = length(D), replace = T)
  
  out <- foreach(
    k = c(1:nfolds), .combine = append
  ) %do% {
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
    
    rsv <- rsv_fun(
      X_train = X_train, D_train = D_train, Y_train = Y_train,
      X_test = X_test, D_test = D_test, Y_test = Y_test, 
      clusters_test = clusters_test, se.boot = se.boot, ...
    )
    
    surrogate <- surrogate_fun(
      D = D_test, Y = rsv$Y_test_pred, 
      clusters = clusters_test, se.boot = se.boot, ...
    )
    
    list(
      rsv = rsv, 
      surrogate = surrogate
    )
  }
  
  coef_rsv_folds <- sapply(out, function(x) x$rsv$coef)
  coef_surrogate_folds <- sapply(out, function(x) x$surrogate$coef)
  
  coef_rsv_cv <- mean(coef_rsv_folds)
  coef_surrogate_cv <- mean(coef_surrogate_folds)
  return(list(
    coef_rsv_cv = coef_rsv_cv,
    coef_surrogate_cv = coef_surrogate_cv,
    out = out
  ))
}
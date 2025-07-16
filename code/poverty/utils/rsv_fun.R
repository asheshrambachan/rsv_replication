suppressPackageStartupMessages({
  library(dplyr)
  library(randomForest)
  library(boot)
})
source("code/poverty/utils/rsv_helpers.R")
source("code/poverty/utils/cluster_sample.R")

first_step_fun <- function(X_train, D_train, Y_train, X_test, classwt=c(50,1), ntree=100){
  train <- create_outcomes(D_train, Y_train)
  
  
  ## Algorithm 1, Step 2 (b) 
  
  ## PRED_Y(R) ~ P(Y = 1 | S = o, R)
  model_Y_So <- randomForest(
    x = X_train[train$So, ], 
    y = as.factor(train$Y_So[train$So]), 
    classwt = classwt,
    ntree = ntree, 
  )
  
  ## PRED_D(R) ~ P(D = 1 | S = e, R)
  model_D_Se <- randomForest(
    x = X_train[train$Se, ], 
    y = as.factor(train$D_Se[train$Se]), 
    ntree = ntree
  )
  
  ## PRED_S(R) ~ P(S = o | R)
  model_So <- randomForest(
    x = X_train, 
    y = as.factor(train$So), 
    ntree = ntree
  )
  
  # Store train predictions
  pred_train <- data.frame(
    D_Se = predict(model_D_Se, X_train, type = "prob")[, 2],
    Y_So = predict(model_Y_So, X_train, type = "prob")[, 2],
    Se = train$Se, # TODO: this isn't train predictions. is this correct?
    So = train$So # TODO: this isn't train predictions. is this correct?
  )
  
  emp_p_train <- get_emp_prob(train)
  Delta_pred <- get_Delta(pred_train, emp_p_train)
  # Algorithm 1, Step 2 (c)
  coef_init <- mean(Delta_pred$e * Delta_pred$o) / mean(Delta_pred$o**2)
  
  # Store test predictions
  pred_test <- data.frame(
    D_Se = predict(model_D_Se, X_test, type = "prob")[, 2], 
    Y_So = predict(model_Y_So, X_test, type = "prob")[, 2], 
    Se = rep(1, nrow(X_test)), # predict(model_Se, X_test, type = "prob")[, 2]
    So = predict(model_So, X_test, type = "prob")[, 2]
  )
  
  return(list(
    coef_init = coef_init,
    pred_test = pred_test
  ))
}

second_step_fun <- function(
    boot_data, D_test = NULL, Y_test = NULL, pred_test = NULL, 
    coef_init, delta
){
  
  is.boot <- !missing(boot_data)
  if (is.boot){
    D_test <- boot_data$D_test
    Y_test <- boot_data$Y_test
    pred_test <- select(boot_data, !c("D_test", "Y_test"))
  }
  
  test <- create_outcomes(D_test, Y_test)
  
  emp_p_test <- get_emp_prob(test)
  Delta_pred <- get_Delta(pred_test, emp_p_test)
  
  sigma2 <- get_sigma2(emp_p_test, pred_test, coef_init)
  Delta <- get_Delta(test, emp_p_test)
  H <- Delta_pred$o / pmax(sigma2, delta)
  
  numerator <- mean(Delta$e * H)
  denominator <- mean(Delta$o * H)
  
  coef <- numerator / denominator
  
  if (is.boot)
    return(c(coef = coef, denominator = denominator))
  else
    return(list(
      coef = coef,
      denominator = denominator,
      Delta = Delta,
      H = H
    ))
}

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
  Y_test_pred <- first_step$pred_test$Y_So
  
  second_step <- second_step_fun(
    D_test = D_test, Y_test = Y_test, pred_test = first_step$pred_test, 
    coef_init = first_step$coef_init, delta = delta)
  
  out <- list(
    coef = second_step$coef,
    coef_init = first_step$coef_init,
    denominator = second_step$denominator,
    H = second_step$H,
    Y_test_pred = Y_test_pred,
    Delta = second_step$Delta
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
      coef_init = first_step$coef_init,   
      delta = delta, 
      mle = list(cluster_var="clusters_test"),
      R = B,
      parallel = "multicore",
      sim = "parametric"
    ) 
    
    # Identify coefficients index
    coef_names <- names(second_step_boot$t0)
    coef_index <- which(coef_names == "coef")
    denominator_index <- which(coef_names == "denominator")
    
    out$se <- sd(second_step_boot$t[, coef_index]) 
    out$denominator_se <- sd(second_step_boot$t[, denominator_index])
  }

  return(out)
}
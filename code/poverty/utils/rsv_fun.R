suppressPackageStartupMessages({
  library(dplyr)
  library(randomForest)
  library(boot)
})

isobs <- function(x) !is.na(x)
isexp <- function(x) is.na(x)

create_outcomes <- function(D, Y){
  isobsD <- isobs(D)
  isobsY <- isobs(Y)
  D[isexp(D)] <- 0
  Y[isexp(Y)] <- 0
  return(data.frame(D, Y, isobsD, isobsY))
}

get_emp_prob <- function(x) list(
  D1 = mean(x$D * x$isobsD),
  D0 = mean((1 - x$D) * x$isobsD),
  Y1 = mean(x$Y * x$isobsY),
  Y0 = mean((1 - x$Y) * x$isobsY)
)

get_Q <- function(emp_p, pred) list(
  D = pred$isobsD * (pred$D/emp_p$D1 - (1 - pred$D)/emp_p$D0),
  Y = pred$isobsY * (pred$Y/emp_p$Y1 - (1 - pred$Y)/emp_p$Y0)
)

get_sigma2 <- function(emp_p, pred, theta_1st) {
  term1 <- pred$isobsD * (pred$D/emp_p$D1**2 + (1 - pred$D)/emp_p$D0**2)
  term2 <- pred$isobsY * theta_1st**2 * (pred$Y/emp_p$Y1**2 + (1 - pred$Y)/emp_p$Y0**2)
  sigma2 = term1 + term2
  return(sigma2)
}

get_Delta <- function(true_p, emp_p) list(
  e = true_p$isobsD * (true_p$D/emp_p$D1 - (1 - true_p$D)/emp_p$D0),
  o = true_p$isobsY * (true_p$Y/emp_p$Y1 - (1 - true_p$Y)/emp_p$Y0)
)

cluster_sample <- function(data, mle=list(cluster_var="clusters_test")) {
  # Extract unique cluster IDs from the specified cluster variable
  cluster_var <- mle$cluster_var 
  cluster_ids <- unique(data[[cluster_var]])
  
  # Sample cluster IDs with replacement. The number of sampled clusters is equal to the total number of unique clusters.
  sampled_clusters <- sample(x = cluster_ids, size = length(cluster_ids), replace = TRUE)
  
  # For each sampled cluster, extract all observations and combine them into a single data frame
  data[data[[cluster_var]] %in% sampled_clusters, ]
}

first_step_fun <- function(X_train, D_train, Y_train, X_test, classwt=c(50,1), ntree=100){
  
  train <- create_outcomes(D_train, Y_train)
  
  # Train treatment model and predict probabilities
  model_D <- randomForest(
    x = X_train[train$isobsD, ], 
    y = as.factor(train$D[train$isobsD]), 
    ntree = ntree
  )
  
  # Train outcome model with class weights and predict probabilities
  model_Y <- randomForest(
    x = X_train[train$isobsY, ], 
    y = as.factor(train$Y[train$isobsY]), 
    classwt = classwt,
    ntree = ntree, 
  )
  
  # Train model for D if needed and predict probabilities
  # if (any(isexp(train$D))) { # this is always false since D[isexp(D)] <- 0
  #   model_isobsD <- randomForest(
  #     x = X_train, 
  #     y = as.factor(train$isobsD), 
  #     ntree = ntree
  #   )
  # }
  
  # Train model for Y and predict probabilities
  model_isobsY <- randomForest(
    x = X_train, 
    y = as.factor(train$isobsY), 
    ntree = ntree
  )
  
  # Store train predictions
  pred_train <- data.frame(
    D = predict(model_D, X_train, type = "prob")[, 2],
    Y = predict(model_Y, X_train, type = "prob")[, 2],
    isobsD = train$isobsD, # TODO: this isn't train predictions. is this correct?
    isobsY = train$isobsY # TODO: this isn't train predictions. is this correct?
  )
  
  # Store test predictions
  pred_test <- data.frame(
    D = predict(model_D, X_test, type = "prob")[, 2], 
    Y = predict(model_Y, X_test, type = "prob")[, 2], 
    isobsD = rep(1, nrow(X_test)), # predict(model_isobsD, X_test, type = "prob")[, 2]
    isobsY = predict(model_isobsY, X_test, type = "prob")[, 2]
  )
  
  emp_p_train <- get_emp_prob(train)
  Q_train <- get_Q(emp_p_train, pred_train)
  
  theta_1st <- mean(Q_train$D * Q_train$Y)/mean(Q_train$Y**2)
  
  return(list(
    theta_1st = theta_1st,
    pred_test = pred_test
  ))
}

second_step_fun <- function(
    boot_data, D_test = NULL, Y_test = NULL, pred_test = NULL, 
    theta_1st, delta
){
  
  is.boot <- !missing(boot_data)
  if (is.boot){
    D_test <- boot_data$D_test
    Y_test <- boot_data$Y_test
    pred_test <- select(boot_data, !c("D_test", "Y_test"))
  }
  
  test <- create_outcomes(D_test, Y_test)
  
  emp_p_test <- get_emp_prob(test)
  Q_test <- get_Q(emp_p_test, pred_test)
  
  sigma2 <- get_sigma2(emp_p_test, pred_test, theta_1st)
  Delta <- get_Delta(test, emp_p_test)
  H <- Q_test$Y / pmax(sigma2, delta)
  
  numerator <- mean(Delta$e * H)
  denominator <- mean(Delta$o * H)
  theta_2nd <- numerator / denominator
  
  if (is.boot)
    return(c(theta_2nd = theta_2nd, denominator = denominator))
  else
    return(list(
      theta_2nd = theta_2nd,
      denominator = denominator,
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
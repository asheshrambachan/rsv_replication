library(randomForest)
source("code/antipoverty/utils/helper_fun.R")

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

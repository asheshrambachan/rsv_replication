
# calibrated, synthetic data generating process
DGP <- function(n, X, D, Y, tau = 0) {
  # added to reproduce the results in the paper but not used
  sample(which(Y == 0), size = n, replace = T)
  sample(which(Y == 1), size = n, replace = T)
  
  Y1_id_draw <- sample(which(Y == 1), size = n, replace = T)
  Y0_id_draw <- sample(which(Y == 0), size = n, replace = T)
  
  X_Y1 <- X[Y1_id_draw, ] 
  X_Y0 <- X[Y0_id_draw, ]
  
  prob_Y1_D1 <- mean(Y[D == 1], na.rm = T) + tau
  prob_Y1_D0 <- mean(Y[D == 0], na.rm = T)
  
  # From here, we no longer need X, D, Y
  Y_D1 <- rbinom(n, 1, prob = prob_Y1_D1)
  Y_D0 <- rbinom(n, 1, prob = prob_Y1_D0)
  
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

# calibrated, synthetic data generating process
DGP <- function(n, X, D, Y, tau = 0) {
  Y1_id_draw <- sample(which(Y == 1), size = n, replace = T)
  Y0_id_draw <- sample(which(Y == 0), size = n, replace = T)
  
  X_Y1 <- X[Y1_id_draw, ] 
  X_Y0 <- X[Y0_id_draw, ]
  
  prob_Y1_D1 <- mean(Y[D == 1], na.rm = T) + tau
  prob_Y1_D0 <- mean(Y[D == 0], na.rm = T)
  Y_D1 <- rbinom(n, 1, prob = prob_Y1_D1)
  Y_D0 <- rbinom(n, 1, prob = prob_Y1_D0)
  
  D_draw <- rbinom(n, 1, prob = 0.5)
  Y_draw <- D_draw * Y_D1 + (1 - D_draw) * Y_D0
  X_draw <- Y_draw * X_Y1 + (1 - Y_draw) * X_Y0
  
  # mimic a setting with missing outcomes, we delete Y if D=1
  S_draw <- ifelse(D_draw == 1, "e", "o")
  Y_draw <- ifelse(S_draw == "e", NA, Y_draw)
  
  return(list(
    X = X_draw,
    D = D_draw,
    Y = Y_draw
  ))
}
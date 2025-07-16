# =============================================================================
# Title: RSV Helper Functions
# Purpose: Internal functions used for computing empirical probabilities,
#          Q-functions, ... etc.
# =============================================================================

# Construct unified outcomes object with missingness indicators
create_outcomes <- function(D, Y){
  Se <- !is.na(D) # 1{S = e}
  So <- !is.na(Y) # 1{S = o}
  
  # Replace missing values with 0 for modeling purposes
  D_Se <- D
  D_Se[!Se] <- 0 # D_Se is D * 1{S = e} = [D | S = e]
  
  Y_So <- Y
  Y_So[!So] <- 0 # Y_So is Y * 1{S = o} = [Y | S = o]
  return(data.frame(D_Se, Y_So, Se, So))
}

# Compute empirical probabilities 
get_emp_prob <- function(x) {
  list(
    D1Se = mean(x$D_Se * x$Se), # P(D = 1 | S = e, X = x) P(S = e | X = x) = P(D = 1, S = e | X = x)
    D0Se = mean((1 - x$D_Se) * x$Se), # P(D = 0 | S = e, X = x) P(S = e | X = x) = P(D = 0, S = e | X = x)
    Y1So = mean(x$Y_So * x$So), # P(Y = 1 | S = o, X = x) P(S = o | X = x) = P(Y = 1, S = o | X = x)
    Y0So = mean((1 - x$Y_So) * x$So) # P(Y = 0 | S = o, X = x) P(S = o | X = x) = P(Y = 0, S = o | X = x)
  )
}


get_sigma2 <- function(emp_p, pred, coef_init) {
  D1Se <- pred$D_Se * pred$Se 
  D0Se <- (1 - pred$D_Se) * pred$Se
  Y1So <- pred$Y_So * pred$So
  Y0So <- (1 - pred$Y_So) * pred$So
  
  term1 <-                D1Se / emp_p$D1Se^2 + D0Se / emp_p$D0Se^2
  term2 <- coef_init^2 * (Y1So / emp_p$Y1So^2 + Y0So / emp_p$Y0So^2)
  sigma2 <- term1 + term2
  return(sigma2)
}

get_Delta <- function(x, emp_p) {
  D1Se <- x$D_Se * x$Se 
  D0Se <- (1 - x$D_Se) * x$Se
  Y1So <- x$Y_So * x$So
  Y0So <- (1 - x$Y_So) * x$So
  
  list(
    e = D1Se / emp_p$D1Se - D0Se / emp_p$D0Se,
    o = Y1So / emp_p$Y1So - Y0So / emp_p$Y0So
  )
}

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

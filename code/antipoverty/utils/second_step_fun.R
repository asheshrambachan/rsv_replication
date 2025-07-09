source("code/antipoverty/utils/helper_fun.R")

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


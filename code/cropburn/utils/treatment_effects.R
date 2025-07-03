# Load required packages
suppressPackageStartupMessages({
  library(fixest)
  library(dplyr)
})

# Helper: Build formula as string from RHS variables
build_formula <- function(lhs_var, rhs_vars) {
  rhs_vars <- paste0("i(", rhs_vars, ")", collapse = " + ")
  as.formula(paste(lhs_var, "~", rhs_vars))
}

# ----------------------------------------------------------
# Function to compute treatment effect decomposition:
#   - beta        = effect of Y on R (observational sample)
#   - theta_tilde = effect of D on R (experimental sample)
#   - theta_star  = scaled effect: theta_tilde / beta
# ----------------------------------------------------------
treatment_effects <- function(data, R_var, Y_var, S_var, D_var, FE_vars) {
  
  # Fit model: R (RSV) ~ D (AnyPES) + FE (on experimental sample)
  model_R_D <- feols(
    fml = build_formula(R_var, c(D_var, FE_vars)),
    data = data %>% filter(!!sym(S_var) == "e"), 
    notes = FALSE
  )
  
  # Fit model: R (RSV) ~ Y (GT) + FE (on observational sample)
  model_R_Y <- feols(
    fml = build_formula(R_var, c(Y_var, FE_vars)),
    data = data %>% filter(!!sym(S_var) == "o"), 
    notes = FALSE
  )
  
  # Extract coefficients of interest
  theta_tilde  <- unname(coef(model_R_D)[paste0(D_var, "::1")])
  beta         <- unname(coef(model_R_Y)[paste0(Y_var, "::1")])
  theta_star   <- theta_tilde / beta

  # Return results as named numeric vector with attributes
  coefs <- c("theta_tilde" = theta_tilde, "beta" = beta, "theta_star" = theta_star)
  attr(coefs, "sample") <- c("Experimental", "Observational", "Observational and Experimental")
  attr(coefs, "nobs")   <- c(model_R_D$nobs, model_R_Y$nobs, model_R_D$nobs)

  return(coefs)
}



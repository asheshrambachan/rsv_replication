# Load required packages
install.packages("boot.pval")

suppressPackageStartupMessages({
  library(boot)
  library(boot.pval)
  library(dplyr)
  library(modelsummary)
})

# Summarize a single coefficient from a boot object in modelsummary format
boot_summary <- function(boot_data, coef_name){
  # Identify coefficient index
  coef_names <- names(boot_data$t0)
  i <- which(coef_names == coef_name)
  
  if (length(i) != 1) {
    stop("Coefficient name not found or not unique.")
  }
  
  # Extract components
  term <- names(boot_data$t0[i])
  estimate <- unname(boot_data$t0[i])
  std.error <- sd(boot_data$t[,i])
  conf.int <- boot.ci(boot_data, conf = 0.95, type = "perc", index = i)$percent[4:5]
  p.value <- boot.pval(boot_data, theta_null = 0, type = "perc", index = i)
  
  # Extract metadata
  sample <- attr(boot_data$t0,"sample")[i]
  is_exp <- grepl("Experimental", sample, ignore.case = T)
  is_obs <- grepl("Observational", sample, ignore.case = T)
  nobs <- attr(boot_data$t0,"nobs")[i]
  
  # Construct tidy-style output (term-level summary)
  ti <- data.frame(
    term = term,
    estimate = estimate,
    p.value = p.value,
    std.error = std.error,
    conf.low = conf.int[1],
    conf.high = conf.int[2]
  )
  
  # Construct glance-style output (model-level summary)
  gl <- data.frame(
    "nobs" = nobs,
    "Experimental" = ifelse(is_exp, "$\\times$", " "),
    "Observational" = ifelse(is_obs, "$\\times$", " ")
  )
  
  # Combine into a modelsummary-compatible object
  mod <- list(tidy = ti, glance = gl)
  class(mod) <- "modelsummary_list"
  return(mod)
}
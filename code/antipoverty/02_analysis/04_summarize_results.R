rm(list = ls())

library(dplyr)
# Set significance level
alpha <- 0.05

# Initialize list to store rows
results <- list()

outcomes <- c("cons", "05k", "10k")
samples <- c("real", "synthetic")

for (Y in outcomes){
  model <- readRDS(sprintf("data/clean/antipoverty/simulations/benchmark_%s.rds", Y))
  
  coef <- unname(coef(model)[2])
  se <- unname(model$se[2])
  lci <- coef - qnorm(1 - alpha) * se
  uci <- coef + qnorm(1 - alpha) * se
  
  # Append row to results list
  results[[length(results) + 1]] <- data.frame(
    estimator = "benchmark",
    S = NA,
    Y = Y,
    coef = coef,
    lci = lci,
    uci = uci,
    denominator_coef = NA,
    denominator_lci = NA,
    denominator_uci = NA
  )
}

for (Y in outcomes){
  for (S in samples){
    model <- readRDS(sprintf("data/clean/antipoverty/simulations/rsv_%s_%s.rds", Y, S))
  
    coef <- model$theta_2nd
    se <- model$theta_2nd_se
    lci <- coef - qnorm(1 - alpha) * se
    uci <- coef + qnorm(1 - alpha) * se
    
    denominator_coef <- model$denominator
    denominator_se <- model$denominator_se
    denominator_lci <- denominator_coef - qnorm(1 - alpha) * denominator_se
    denominator_uci <- denominator_coef + qnorm(1 - alpha) * denominator_se
  
    # Append row to results list
    results[[length(results) + 1]] <- data.frame(
      estimator = "rsv",
      S = S,
      Y = Y,
      coef = coef,
      lci = lci,
      uci = uci,
      denominator_coef = denominator_coef,
      denominator_lci = denominator_lci,
      denominator_uci = denominator_uci
    )
  }
}

# Combine all rows into a single data frame
summary_df <- do.call(rbind, results)

write.csv(summary_df, "data/clean/antipoverty/estimates.csv", row.names = F)


y_cons_raw <- read.csv("data/clean/antipoverty/data_wo_features.csv") %>%
  filter(
    tot_p >= 100, # Identify and remove small shrid based on population size i.e filter out all shrids with pop size of 100 or lower
    !(is.na(y_05k) | is.na(y_10k) | is.na(y_cons)) # Remove rows with missing values in target variable
  ) %>% .$y_cons_raw

# Initialize list to store rows
results <- list()

for (S in c("synthetic", "real")){
  filename <- sprintf("data/clean/antipoverty/simulations/rsv_cons_%s.rds", S)
  rds <- readRDS(filename)
  Y <- rds$Y
  H <- scale(rds$H)
  
  results[[length(results) + 1]] <- data.frame(
    S = S,
    Y = Y,
    H = H,
    consumption = log(y_cons_raw)
  ) 
}

# Combine all rows into a single data frame
summary_df <- do.call(rbind, results)

write.csv(summary_df, "data/clean/antipoverty/representations.csv", row.names = F)

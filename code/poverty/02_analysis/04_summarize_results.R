rm(list = ls())
library(tidyr)
library(dplyr)
# Set significance level
alpha <- 0.05

# Initialize list to store rows
results <- list()

for (Y in c("Ycons", "Ylow", "Ymid")){
  model <- readRDS(sprintf("data/interim/poverty/rsv_vs_benchmark/benchmark_%s.rds", Y))
  
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

for (Y in c("Ycons", "Ylow", "Ymid")){
  for (S in c("Sreal", "Ssynth")){
    model <- readRDS(sprintf("data/interim/poverty/rsv_vs_benchmark/rsv_%s_%s.rds", Y, S))
  
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

write.csv(summary_df, "data/processed/poverty/rsv_vs_benchmark.csv", row.names = F)


###
# Initialize list to store rows
results <- list()

for (S in c("Ssynth", "Sreal")){
  filename <- sprintf("data/interim/poverty/rsv_vs_benchmark/rsv_Ycons_%s.rds", S)
  rds <- readRDS(filename)
  Y <- rds$Y
  H <- scale(rds$H)
  
  results[[length(results) + 1]] <- data.frame(
    S = S,
    Y = Y,
    H = H
  ) 
}

# Combine all rows into a single data frame
summary_df <- do.call(rbind, results)

write.csv(summary_df, "data/interim/poverty/representations.csv", row.names = F)


####


rmse <- function(x) sqrt(mean(x**2))


results <- list()
for(n in c(1000, 2000, 3000)){
  for(tau in seq(from = 0, to = 0.5, by = 0.1)){
    
    rsv <- readRDS(sprintf("data/interim/poverty/rsv_vs_surrogate/rsv_nfeat1050_nfold5_B500_n%s_tau%.1f.rds", n, tau))
    surrogate <- readRDS(sprintf("data/interim/poverty/rsv_vs_surrogate/surrogate_nfeat1050_nfold5_B500_n%s_tau%.1f.rds", n, tau))
    
    rsv_boot <- rsv$coef_cv_boot
    surrogate_boot <- surrogate$coef_cv_boot
    
    TE <- rsv$TE
    
    results[[length(results) + 1]] <- data.frame(
      sample_size = n, # sample_size
      tau = tau,
      theta = TE,
      
      rmse_rsv = rmse(rsv_boot - TE),
      rmse_surrogate = rmse(surrogate_boot - TE),
      
      bias_rsv = mean(rsv_boot) - TE,
      bias_surrogate = mean(surrogate_boot) - TE
    )
  }
}

# Combine all rows into a single data frame
summary_df <- results %>% 
  bind_rows() %>%
  pivot_longer(
  cols = c(starts_with("coef_"), starts_with("rmse_"), starts_with("bias_")),
  names_to = c(".value", "estimator"),
  names_sep = "_"
)
summary_df$Y <- "Ycons"

output_path <- "data/processed/poverty/rsv_vs_surrogate.csv"
write.csv(summary_df, output_path, row.names = F)
# cat(sprintf("Saved results to: %s\n", output_path))



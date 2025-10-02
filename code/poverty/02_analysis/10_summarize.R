# =============================================================================
# Summary for Generated RDS Files
# =============================================================================

rm(list = ls())

## Load libraries
suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
})


# Significance level for CIs
alpha <- 0.05

# -----------------------------------------------------------------------------
# 1. Benchmark & RSV RealD Results Table
# -----------------------------------------------------------------------------
results <- list()


for (spillover in c("", "_wo_spillover")){
  for (Y in c("Ycons", "Ylowinc", "Ymidinc")){
    
    ## 1.1 Benchmark: Estimate and CI for each Y outcome
    model <- readRDS(sprintf("data/poverty/interim/benchmark_realD_%s%s.rds", Y, spillover))
    
    coef <- unname(coef(model)[2])
    se <- unname(model$se[2])
    lci <- coef - qnorm(1 - alpha) * se
    uci <- coef + qnorm(1 - alpha) * se
    
    # Append row to results list
    results[[length(results) + 1]] <- data.frame(
      estimator = "benchmark",
      spillover = if_else(spillover=="_wo_spillover", "without", "with"),
      S = NA,
      Y = Y,
      coef = coef,
      se = se,
      lci = lci,
      uci = uci
    )
    
    
    ## 1.2 RSV: Estimate and denominator for each (Y, S) combination
    for (S in c("realS", "synthS")){
      model <- readRDS(sprintf("data/poverty/interim/rsv_realD_%s_%s%s.rds", S, Y, spillover))
      
      coef <- model$coef
      se <- model$se
      lci <- coef - qnorm(1 - alpha) * se
      uci <- coef + qnorm(1 - alpha) * se
      
      # Append row to results list
      results[[length(results) + 1]] <- data.frame(
        estimator = "rsv",
        spillover = if_else(spillover=="_wo_spillover", "without", "with"),
        S = S,
        Y = Y,
        coef = coef,
        se = se,
        lci = lci,
        uci = uci
      )
    }
  }
}



## Aggregate Results
summary_df <- do.call(rbind, results)
print(summary_df)


## Export Results
output_path <-  "data/poverty/processed/realD_coefs.csv"
write.csv(summary_df,output_path, row.names = F)
cat(sprintf("Saved to: %s\n", output_path))


# -----------------------------------------------------------------------------
# 2. Export Relavance/Denominators for RSV RealD
# -----------------------------------------------------------------------------
results <- list()
for (spillover in c("", "_wo_spillover")){
  for (Y in c("Ycons", "Ylowinc", "Ymidinc")){
    for (S in c("realS", "synthS")){
      model <- readRDS(sprintf("data/poverty/interim/rsv_realD_%s_%s%s.rds", S, Y, spillover))
      
      coef <- model$denominator
      se <- model$denominator_se
      lci <- coef - qnorm(1 - alpha) * se
      uci <- coef + qnorm(1 - alpha) * se
      
      # Append row to results list
      results[[length(results) + 1]] <- data.frame(
        estimator = "rsv",
        spillover = if_else(spillover=="_wo_spillover", "without", "with"),
        S = S,
        Y = Y,
        coef = coef,
        se = se,
        lci = lci,
        uci = uci
      )
    }
  }
}


## Aggregate Results
summary_df <- do.call(rbind, results)
print(summary_df)


## Export Results
output_path <-  "data/poverty/processed/realD_denominators.csv"
write.csv(summary_df,output_path, row.names = F)
cat(sprintf("Saved to: %s\n", output_path))


# -----------------------------------------------------------------------------
# 3. Export Scaled Representations H (for "Ycons" only, by S)
# -----------------------------------------------------------------------------
results <- list()
for (S in c("realS", "synthS")){
  filename <- sprintf("data/poverty/interim/rsv_realD_%s_Ycons.rds", S)
  rds <- readRDS(filename)
  
  results[[length(results) + 1]] <- data.frame(
    S = S,
    Y = rds$Y,
    H = as.numeric(scale(rds$H))
  )
}


## Aggregate Results
summary_df <- do.call(rbind, results)
print(head(summary_df))


## Export Results
output_path <-  "data/poverty/processed/realD_representations.csv"
write.csv(summary_df,output_path, row.names = F)
cat(sprintf("Saved to: %s\n", output_path))



# -----------------------------------------------------------------------------
# 4. RSV & Surrogate Synthetic Simulation Result
# -----------------------------------------------------------------------------
results <- list()
n_list <- c(1000, 2000, 3000)
tau_list <- seq(0, 0.5, by = 0.1)
Y_var <- "Ycons"

for(n in n_list){
  for(tau in tau_list){

    rsv <- readRDS(sprintf("data/poverty/interim/rsv_synthD_synthS_%s_n%s_tau%.1f.rds", Y_var, n, tau))
    surrogate <- readRDS(sprintf("data/poverty/interim/surrogate_synthD_synthS_%s_n%s_tau%.1f.rds", Y_var, n, tau))
    
    synth_te <- surrogate$synth_te # or rsv$synth_te
    
    
    rsv_boot <- rsv$coef_boot
    surrogate_boot <- surrogate$coef_boot
    
    rsv$rmse = sqrt(mean((rsv_boot - synth_te)^2))
    surrogate$rmse = sqrt(mean((surrogate_boot - synth_te)^2))
    
    rsv$bias =  mean(rsv_boot) - synth_te
    surrogate$bias = mean(surrogate_boot) - synth_te
    
    results[[length(results) + 1]] <- data.frame(
      Y = Y_var, 
      sample_size = n, 
      tau = tau,

      theta = synth_te,
      rmse_rsv       = rsv$rmse,
      rmse_surrogate = surrogate$rmse,
      bias_rsv       = rsv$bias,
      bias_surrogate = surrogate$bias 
    )
  }
}


## Aggregate Results
summary_df <- results %>%
  bind_rows() %>%
  pivot_longer(
    cols = c(starts_with("rmse_"), starts_with("bias_")),
    names_to = c(".value", "estimator"),
    names_sep = "_"
  )
print(summary_df)


## Export Results
output_path <- "data/poverty/processed/synthD_synthS_coefs.csv"
write.csv(summary_df, output_path, row.names = F)
cat(sprintf("Saved to: %s\n", output_path))

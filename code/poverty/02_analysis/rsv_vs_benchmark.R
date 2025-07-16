rm(list = ls())

# === Load libraries ===
suppressPackageStartupMessages({
  library(fixest)
  library(dplyr)
  library(readr)
  library(doParallel)
})
options(readr.show_col_types = F)
source("code/poverty/utils/rsv_fun.R")


# === Load Data ===
data <- read_csv("data/processed/poverty/data.csv") 

# Define synthetic and real sample splits
obs_mandals <- unique(data$clusters)[1:floor(length(unique(data$clusters)) / 2)]

data <- data %>%
  mutate(
    Sreal = if_else(wave %in% c("Holdout", "Buffer"), "o", "e"),  
    Ssynth = ifelse(clusters %in% obs_mandals, "o", "e") 
  )


# === Benchmark Regressions ===
for (Y_var in c("Ycons", "Ylowinc", "Ymidinc")){
  model <- feols(
    fml = as.formula(paste0(Y_var, " ~ D")), 
    data = data, 
    cluster = ~clusters,
    notes = F
  )
  
  output_path <- sprintf("data/interim/poverty/rsv_vs_benchmark/benchmark_%s.rds", Y_var)
  dir.create(dirname(output_path), recursive = T, showWarnings = F)
  saveRDS(model, output_path)
  cat(sprintf("[BENCHMARK] %-5s → coef = %.3f; se = %.3f → saved to %s\n", 
              Y_var, coef(model)[2], model$se[2], output_path))
}


# === RSV Estimation with Parallelization ===
sim_grid <- expand.grid(
  Y_var = c("Ycons", "Ylowinc", "Ymidinc"), 
  S_var = c("Sreal", "Ssynth"), 
  stringsAsFactors = F
)

# Set up parallel backend
n_cores <- min(detectCores() - 1, nrow(sim_grid))
cl <- makeCluster(n_cores)
registerDoParallel(cl)
cat(sprintf("[INFO] Running on %d cores\n", n_cores))

results <- foreach(
  i = 1:nrow(sim_grid), 
  .packages = c("boot", "randomForest", "dplyr")
) %dopar% {
  row <- sim_grid[i, ]
  Y_var <- row$Y_var
  S_var <- row$S_var
  
  d <- data %>% 
    mutate(Y = !!sym(Y_var), S = !!sym(S_var)) %>%
    filter(!is.na(Y)) %>%
    mutate(Y = if_else(S == "e", NA, Y))
  
  X <- d %>% 
    select(
      starts_with("viirs_annual_"),
      starts_with("feature_")
      )
  
  set.seed(42)
  out <- rsv_fun(
    X_train = X, D_train = d$D, Y_train = d$Y,
    X_test  = X, D_test  = d$D, Y_test  = d$Y, 
    classwt = c(10, 1), ntree = 100,
    delta = 0.01,
    se.boot = T, 
    clusters_test = d$clusters, 
    B = 1000
  )
  
  output_path <- sprintf("data/interim/poverty/rsv_vs_benchmark/rsv_%s_%s.rds", Y_var, S_var)
  dir.create(dirname(output_path), recursive = T, showWarnings = F)
  saveRDS(out, output_path)
  
  list(
    coef = out$coef,
    se = out$se,
    Y_var = Y_var,
    S_var = S_var,
    output_path = output_path
  )
}

stopCluster(cl)

for (i in 1:length(results)){
  out <- results[[i]]
  cat(sprintf("[RSV] %-5s %-6s → coef = %.3f; se = %.3f → saved to %s\n", 
              out$Y_var, out$S_var, out$coef, out$se, out$output_path))
}


####


# Set significance level
alpha <- 0.05

# Initialize list to store rows
results <- list()

for (Y in c("Ycons", "Ylowinc", "Ymidinc")){
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
    se = se,
    lci = lci,
    uci = uci,
    denominator_coef = NA,
    denominator_lci = NA,
    denominator_uci = NA
  )
  
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
      se = se,
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

write.csv(summary_df, "data/processed/poverty/representations.csv", row.names = F)


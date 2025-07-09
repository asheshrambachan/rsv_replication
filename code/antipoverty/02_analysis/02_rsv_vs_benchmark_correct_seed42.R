# Clear environment
rm(list = ls())

# Load required libraries
suppressPackageStartupMessages({
  library(fixest)
  library(dplyr)
  library(readr)
  library(doParallel)
})
source("code/antipoverty/utils/rsv_fun.R")

# Load data
data <- read_csv("data/clean/antipoverty/data.csv", show_col_types = F) 
obs_mandals <- unique(data$clusters)[1:floor(length(unique(data$clusters)) / 2)]
data <- data %>%
  mutate(
    Sreal = if_else(wave %in% c("Holdout", "Buffer"), "o", "e"), # RSV: Buffer & Holdout 
    Ssynth = ifelse(clusters %in% obs_mandals, "o", "e"), # RSV: random subset (some control and treated units into the same)
    Y05k_Sreal = ifelse(Sreal == "e", NA, Y05k),
    Y10k_Sreal = ifelse(Sreal == "e", NA, Y10k),
    Ycons_Sreal = ifelse(Sreal == "e", NA, Ycons),
    Y05k_Ssynth = ifelse(Ssynth == "e", NA, Y05k),
    Y10k_Ssynth = ifelse(Ssynth == "e", NA, Y10k),
    Ycons_Ssynth = ifelse(Ssynth == "e", NA, Ycons)
  )

# Estimate and save benchmark regressions
Y_list <- c("Ycons", "Y05k", "Y10k")
for (Y in Y_list){
  out <- feols(
    fml = as.formula(paste0(Y, " ~ D")), 
    data = data, 
    cluster = ~clusters,
    notes = FALSE
  )
  cat(sprintf("coef = %.3f; se = %.3f; ", coef(out)[2], out$se[2]))
  output_path <- sprintf("data/raw/antipoverty/simulations_correct_seed42_sample/benchmark_%s.rds", Y)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, output_path)
  cat(sprintf("Saved results to: %s\n", output_path))
}

# Estimate and save rsv regressions
D <- data$D
clusters <- data$clusters
X <- data %>% 
  select(starts_with("feature_")) %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

Y_list <- grep(pattern = "^Y(05k|10k|cons)_S(real|synth)$", colnames(data), value = TRUE)


# Set up parallel backend
cores <- min(detectCores() - 1, length(Y_list))
cl <- makeCluster(cores)
registerDoParallel(cl)
cat(sprintf("n cores = %s", cores))

list <- foreach(
  Y = Y_list, 
  # .export = c(
  #   "X", "D", "clusters", "data", 
  #   "rsv_fun", "first_step_fun", "second_step_fun", "cluster_sample"
  #   ),
  .packages = c("boot", "randomForest", "dplyr")
) %dopar% {
  set.seed(42)
  out <- rsv_fun(
    X_train = X, D_train = D, Y_train = data[[Y]],
    X_test = X, D_test = D, Y_test = data[[Y]], 
    classwt = c(10, 1), ntree = 100,
    delta = 0.01,
    se.boot = T, clusters_test = clusters, B = 1000
  )
  
  cat(sprintf("coef = %.3f; se = %.3f; ", out$coef, out$se))
  output_path <- sprintf("data/raw/antipoverty/simulations_correct_seed42_sample/rsv_%s.rds", Y)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, output_path)
  cat(sprintf("Saved results to: %s\n", output_path))
}

# Stop the parallel cluster
stopCluster(cl)
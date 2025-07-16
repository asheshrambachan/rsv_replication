suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
  library(readr)
  library(doParallel)
  library(argparse)
})
source("code/poverty/utils/cv_rsv_surrogate.R")

# Create a parser object
parser <- ArgumentParser(description = "Process some integers and a message.")
parser$add_argument("-n", "--n", type = "integer", default = c(1000, 2000, 3000))
parser$add_argument("-t", "--tau", type = "double", default = seq(from = 0, to = 0.5, by = 0.1))
parser$add_argument("-c", "--cores", type = "integer", default = 120)
parser$add_argument("-b", "--B", type = "integer", default = 500)
args <- parser$parse_args()

# Check if arguments are provided
n_list <- c(args$n)
tau_list <- c(args$tau)
B <- args$B
cores <- min(args$cores, detectCores() - 1) # Use all but one core
cat("    n =", n_list, "\n")
cat("  tau =", tau_list, "\n")
cat("    B =", B, "\n")
cat("cores =", cores, "\n")

data <- read_csv("data/processed/poverty/data.csv", show_col_types = F)
Y <- data$Ycons
D <- data$D
X <- data %>% 
  select(
    starts_with("viirs_annual_"),
    paste0("feature_", 1:1000)
  )

for(n in n_list){
  for(tau in tau_list){
    output_path <- sprintf("data/interim/poverty/rsv_vs_surrogate/rsv_nfeat%s_nfold5_B%s_n%s_tau%.1f.rds", ncol(X), B, n, tau)
    if (file.exists(output_path))
      next
    
    # Set up parallel backend
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    
    out <- foreach(
      b = c(1:B), 
      .packages = c("boot", "randomForest")
    ) %dopar% {
      set.seed(b)
      draw <- DGP(n = n, tau = tau, X = X, D = D, Y = Y) 
      cv_rsv_surrogate(X = draw$X, D = draw$D, Y = draw$Y, nfold = 5)
    }
    
    # Stop the parallel cluster
    stopCluster(cl)
    
    TE <- mean(Y[D == 1], na.rm = T) + tau - mean(Y[D == 0], na.rm = T)
    rsv_coef_cv_boot = sapply(out, function(x) x$rsv$coef_cv)
    surrogate_coef_boot = sapply(out, function(x) x$surrogate$coef_cv)
    
    rsv <- list(
      TE = TE,
      coef = mean(rsv_coef_cv_boot, na.rm=T),
      coef_boot = rsv_coef_cv_boot,
      out = lapply(out, function(x) x$rsv)
    )

    dir.create(dirname(output_path), recursive = T, showWarnings = F)
    saveRDS(rsv, output_path)
    cat(sprintf("[RSV]       True = %3.3f; Estimate = %3.3f → saved to %s\n", 
                TE, rsv$coef, output_path))
    
    surrogate <- list(
      TE = TE,
      coef = mean(surrogate_coef_boot, na.rm=T),
      coef_boot = surrogate_coef_boot,
      out = lapply(out, function(x) x$surrogate)
    )
    
    output_path <- sprintf("data/interim/poverty/rsv_vs_surrogate/surrogate_nfeat%s_nfold5_B%s_n%s_tau%.1f.rds", ncol(X), B, n, tau)
    dir.create(dirname(output_path), recursive = T, showWarnings = F)
    saveRDS(surrogate, output_path)
    cat(sprintf("[Surrogate] True = %3.3f; Estimate = %3.3f → saved to %s\n", 
                TE, surrogate$coef, output_path))
  }
}



#####



results <- list()
for(n in c(1000, 2000, 3000)){
  for(tau in seq(from = 0, to = 0.5, by = 0.1)){
    
    rsv <- readRDS(sprintf("data/interim/poverty/rsv_vs_surrogate/rsv_nfeat1050_nfold5_B500_n%s_tau%.1f.rds", n, tau))
    surrogate <- readRDS(sprintf("data/interim/poverty/rsv_vs_surrogate/surrogate_nfeat1050_nfold5_B500_n%s_tau%.1f.rds", n, tau))
    
    rsv_boot <- rsv$coef_
    surrogate_boot <- surrogate$coef_
    
    TE <- rsv$TE
    
    results[[length(results) + 1]] <- data.frame(
      sample_size = n, # sample_size
      tau = tau,
      theta = TE,
      
      rmse_rsv = sqrt(mean((rsv_boot - TE)^2)),
      rmse_surrogate = sqrt(mean((surrogate_boot - TE)^2)),
      
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
cat(sprintf("Saved results to: %s\n", output_path))

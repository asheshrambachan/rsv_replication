# =============================================================================
# Estimate Surrogate: Synthetic Treatment Effects & Synthetic Sample Definitions
# =============================================================================

rm(list = ls())


## Load libraries
suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
  library(readr)
  library(parallel)
  library(argparse)
})
source("code/poverty/utils/surrogate_fun.R")
source("code/poverty/utils/common.R")


## Parse Command-Line Arguments
parser <- ArgumentParser(description = "Estimate Surrogate: Synthetic Treatment Effects & Synthetic Sample Definitions")
parser$add_argument("-n", "--n", type = "integer", default = c(1000, 2000, 3000))
parser$add_argument("-t", "--tau", type = "double", default = seq(0, 0.5, by = 0.1))
parser$add_argument("-c", "--cores", type = "integer", default = 245)
parser$add_argument("-b", "--B", type = "integer", default = 500)
parser$add_argument("-r", "--use_rsv", type = "logical", default = TRUE)
parser$add_argument("-y", "--Y_var", type = "character", default = "Ycons")
args <- parser$parse_args()

n_list   <- c(args$n)
tau_list <- c(args$tau)
B        <- args$B
use_rsv  <- args$use_rsv
Y_var    <- args$Y_var
cores    <- min(B, args$cores, detectCores() - 1)

cat(sprintf(
  "[INFO] n = %s; tau = %s; B = %d; cores = %d; use_rsv = %s; Y_var = %s\n",
  paste(n_list, collapse = ","), paste(tau_list, collapse = ","), B, cores, use_rsv, Y_var
))


## Load Data (if needed)
if (!use_rsv) {
  data <- read_csv("data/poverty/processed/poverty_data.csv", show_col_types = F)
  Y <- data[[Y_var]]
  D <- data$D
  X <- data %>% 
    select(
      starts_with("viirs_annual_"),
      paste0("feature_", 1:1000)
    )
  real_te <- mean(Y[D == 1], na.rm = T) - mean(Y[D == 0], na.rm = T)
}


## Main Estimation Loop
for(n in n_list){
  for(tau in tau_list){
    if (use_rsv) {
      # If RSV object is provided, use precomputed RSV outputs for synthetic D/S assignment
      rsv_path <- sprintf("data/poverty/interim/rsv_synthD_synthS_%s_n%s_tau%.1f.rds", Y_var, n, tau)
      rsv <- readRDS(rsv_path)
      real_te <- rsv$synth_te - tau
      run_one <- function(b) {
        rsv_b <- rsv$out[[b]]
        out   <- lapply(rsv_b, surrogate_fun.rsv)
        coef_cv <- mean(sapply(out, function(x) x$coef), na.rm = TRUE)
        list(coef_cv = coef_cv, out = out)
      }
    } else {
      # Generate new synthetic datasets and estimate from scratch
      run_one <- function(b) {
        set.seed(b)
        draw <- gen_synth_data(n = n, tau = tau, X = X, D = D, Y = Y)
        cv_fun(
          fun   = surrogate_fun.default,
          nfold = 5,
          X     = draw$X,
          D     = draw$D,
          Y     = draw$Y,
          So    = draw$So
        )
      }
    }

    # Parallel Estimation Across B
    out_boot <- mclapply(1:B, run_one, mc.cores = cores)

    # Aggregate Results
    coef_cv_boot <- sapply(out_boot, function(x) x$coef_cv)
    out <- list(
      synth_te   = real_te + tau,
      coef       = mean(coef_cv_boot, na.rm=TRUE),
      coef_boot  = coef_cv_boot,
      out        = lapply(out_boot, function(x) x$out)
    )
    
    # Export Results
    output_path <- sprintf("data/poverty/interim/surrogate_synthD_synthS_%s_n%s_tau%.1f.rds", Y_var, n, tau)
    dir.create(dirname(output_path), recursive = T, showWarnings = F)
    saveRDS(out, output_path)

    # Print Summary
    cat(sprintf("[Surrogate] synthD synthS %s n = %d tau = %.2f synth_te = %.3f; coef = %.3f → Saved to %s\n", Y_var, n, tau, out$synth_te, out$coef, output_path))
  }
}
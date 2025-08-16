# =============================================================================
# Estimate Benchmark
# =============================================================================

rm(list = ls())

## Load libraries
suppressPackageStartupMessages({
  library(fixest)
  library(dplyr)
  library(readr)
})
options(readr.show_col_types = F)


## Load data
data <- read_csv("data/poverty/processed/poverty_data.csv")
Y_vars <- c("Ycons", "Ylowinc", "Ymidinc")


## Benchmark Regressions
for (Y_var in Y_vars){
  model <- feols(
    fml = as.formula(paste0(Y_var, " ~ D")), 
    data = data, 
    cluster = ~clusters,
    notes = F
  )
  
  # Export Results
  output_path <- sprintf("data/poverty/interim/benchmark_realD_%s.rds", Y_var)
  dir.create(dirname(output_path), recursive = T, showWarnings = F)
  saveRDS(model, output_path)
  cat(sprintf("[BENCHMARK] realD %s; coef = %.3f (%.3f) → saved to %s\n", 
              Y_var, coef(model)[2], model$se[2], output_path))
}
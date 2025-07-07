# Clear environment
rm(list = ls())

# Load required libraries
suppressPackageStartupMessages({
  library(fixest)
  library(dplyr)
  library(readr)
})

# Load data
data <- read_csv(
  "data/clean/antipoverty/data.csv",
  col_select = c(Y05k, Y10k, Ycons, clusters, D)
  )

# Regression settings
outcomes <- c("Ycons", "Y05k", "Y10k")
out_dir <- "data/clean/antipoverty/simulations"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Estimate and save regressions
for (Y_name in outcomes){
  out <- feols(
    fml = as.formula(paste0(Y_name, " ~ D")), 
    data = data, 
    cluster = ~clusters,
    notes = FALSE
  )
  cat(sprintf("coef = %.3f; se = %.3f; ", coef(out)[2], out$se[2]))
  output_path <- file.path(out_dir, sprintf("benchmark_%s.rds", Y_name))
  saveRDS(out, output_path)
  cat(sprintf("Saved results to: %s\n", output_path))
}

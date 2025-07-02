# Clear environment
rm(list=ls())

# Load required packages
library(boot)
library(purrr)
source("code/common/cluster_grouped_sample.R")
source("code/common/treatment_effects.R")

# Load cleaned dataset
data <- read.csv("data/clean/cropburn/data.csv")

# Set seed for reproducibility
set.seed(0) 

# Run Bootstrap Simulation
out <- boot(
  statistic = treatment_effects,
  ran.gen = cluster_grouped_sample,
  data = data,
  R_var = "R_bal",  # RSV Outcome
  Y_var = "Y",      # GT Outcome
  S_var = "S",      # Sample indicator
  D_var = "D",      # Treatment indicator
  # FE_vars = c("rabovemed", "district", "baseline_complete", "listing_not_complete", "vill_added_back"),
  # cluster = "village_id",
  mle = list(S_var="S", cluster_var="village_id"),
  R = 5000, # Number of bootstrap replications
  parallel = "multicore",
  sim = "parametric",
) 

# Save Output
output_path <- sprintf("data/clean/cropburn/te_bootstrap_%s.rds", Sys.Date())
saveRDS(out, file = output_path)
cat(sprintf("Saved bootstrap simulation to: %s\n", output_path))

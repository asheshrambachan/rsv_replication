# Clear environment
rm(list=ls())

# Load required packages
library(boot)
library(purrr)
source("code/cropburn/utils/cluster_grouped_sample.R")
source("code/cropburn/utils/treatment_effects.R")

# Load cleaned dataset
data <- read.csv("data/clean/cropburn/data.csv")

for (R_var in c("R_max", "R_bal")){
  # Set seed for reproducibility
  set.seed(0) 
  
  # Run Bootstrap Simulation
  cat("Started bootstrap simulations...\n")
  out <- boot(
    statistic = treatment_effects,
    ran.gen = cluster_grouped_sample,
    data = data,
    R_var = R_var,    # RSV Outcome
    Y_var = "Y",      # GT Outcome
    S_var = "S",      # Sample indicator
    D_var = "D",      # Treatment indicator
    FE_vars = c("rabovemed", "district", "baseline_complete", "listing_not_complete", "vill_added_back"),
    mle = list(S_var="S", cluster_var="village_id"),
    R = 5000, # Number of bootstrap replications
    parallel = "multicore",
    sim = "parametric",
  ) 
  
  print(out)
  
  # Save Output
  output_path <- sprintf("data/clean/cropburn/te_bootstrap_%s.rds", gsub("_", "", R_var))
  saveRDS(out, file = output_path)
  cat(sprintf("Saved bootstrap simulations to: %s\n", output_path))
}

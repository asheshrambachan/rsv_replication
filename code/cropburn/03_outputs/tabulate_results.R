# Generate LaTeX Table of Bootstrapped Coefficients

# Clear environment
rm(list=ls())

# Load required packages
suppressPackageStartupMessages({
  library(kableExtra)
  library(dplyr)
})
source("code/cropburn/utils/boot_summary.r")

for (R_var in c("R_max", "R_bal")){
  # Load bootstrapped estimates
  coef_boot <- readRDS(sprintf("data/processed/cropburn/te_bootstrap_%s.rds", gsub("_", "", R_var)))
  
  # Create a modelsummary-formatted list of boot coefficient summaries
  tab <- modelsummary(
      list(
        "$\\tilde{\\theta}$" = boot_summary(coef_boot, "theta_tilde"), 
        "$\\beta$" = boot_summary(coef_boot, "beta"),
        "$\\theta^\\star$" = boot_summary(coef_boot, "theta_star") 
      ),
      statistic = "std.error",
      coef_map = c("beta" = "Estimate", "theta_tilde" = "Estimate", "theta_star" = "Estimate"),
      fmt = fmt_sprintf("%.3f"),
      gof_omit = ".*", # Omit glance-level stats
      stars = c('*' = .1, '**' = .05, '***' = .01),
      escape = FALSE,
      output = "data.frame"
    ) %>%
    mutate(Estimand = c("Estimate", ""), .before = 1) %>%
    select(-part, -term, -statistic)
  
  # Convert to LaTeX table with grouped header
  latex_tab <- tab %>%
    kbl(
      format = "latex",
      booktabs = TRUE,
      align = "lccc",
      escape = FALSE
    ) %>%
    add_header_above(c(" ", "Common practice", "Bias", "Causal parameter"), line = FALSE)
  
  # Save LaTeX table
  output_path <- sprintf("output/tables/cropburn_te_%s.tex", gsub("_", "", R_var))
  save_kable(latex_tab, output_path)
  cat(sprintf("Saved LaTeX table to: %s\n", output_path))
}
# Clear environment
rm(list = ls())

# Load required libraries
suppressPackageStartupMessages({
  library(fixest)
  library(dplyr)
  library(readr)
})
source("code/antipoverty/utils/rsv_fun.R")

# Load data
data_benchmark <- read_csv(
  "data/clean/antipoverty/data.csv",
  col_select = c(shrid2, Y05k, Y10k, Ycons, clusters, D),
  show_col_types = F
)

data_rsv <- read.csv("data/clean/antipoverty/data.csv") %>%
  filter(
    tot_p >= 100, # Identify and remove small shrid based on population size i.e filter out all shrids with pop size of 100 or lower
    !(is.na(Y05k) | is.na(Y10k) | is.na(Ycons)) # Remove rows with missing values in target variable
  )
obs_mandals <- unique(data_rsv$clusters)[1:floor(length(unique(data_rsv$clusters)) / 2)]
data_rsv <- data_rsv %>%
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
    data = data_benchmark, 
    cluster = ~clusters,
    notes = FALSE
  )
  cat(sprintf("coef = %.3f; se = %.3f; ", coef(out)[2], out$se[2]))
  output_path <- sprintf("data/raw/antipoverty/simulations/benchmark_%s_v2.rds", Y)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, output_path)
  cat(sprintf("Saved results to: %s\n", output_path))
}


# Estimate and save rsv regressions
D <- data_rsv$D
clusters <- data_rsv$clusters
X <- data_rsv %>% 
  select(
    starts_with("viirs_annual_"),
    "lat", "lon",
    starts_with("feature_")
    ) %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

Y_list <- grep(pattern = "^Y(05k|10k|cons)_S(real|synth)$", colnames(data_rsv), value = TRUE)
for (Y in Y_list) {
  set.seed(123)
  out <- rsv_fun(
    X_train = X, D_train = D, Y_train = data_rsv[[Y]],
    X_test = X, D_test = D, Y_test = data_rsv[[Y]], 
    classwt = c(10, 1), ntree = 100,
    delta = 0.01,
    se.boot = T, clusters_test = clusters, B = 1000
  )
  
  cat(sprintf("coef = %.3f; se = %.3f; ", out$coef, out$se))
  output_path <- sprintf("data/raw/antipoverty/simulations/rsv_%s.rds", Y)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, output_path)
  cat(sprintf("Saved results to: %s\n", output_path))
}

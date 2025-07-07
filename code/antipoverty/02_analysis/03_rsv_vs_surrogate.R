library(dplyr)
library(doParallel)  
library(tidyr)
source("code/antipoverty/utils/rsv_surrogate_cv.R")
source("code/antipoverty/utils/DGP.R")

library(doParallel)
# cl<-makePSOCKcluster(5)
# registerDoParallel(cl)
cores <- 7
registerDoParallel(cores = cores)

rmse <- function(x) sqrt(mean(x**2))

data_wo_features <- read.csv("data/clean/antipoverty/data_wo_features.csv") %>%
  rename(c(
    Y05k = y_05k,
    Y10k = y_10k,
    Ycons = y_cons,
  )) %>%
  filter(
    tot_p >= 100, # Identify and remove small shrid based on population size i.e filter out all shrids with pop size of 100 or lower
    # !(is.na(Y05k) | is.na(Y10k) | is.na(Ycons)) # Remove rows with missing values in target variable
  ) %>%
  select(shrid2, lat, lon, Y05k, Y10k, Ycons, D)

viirs_annual <- read.csv("data/raw/shrug/shrug-viirs-annual-csv/viirs_annual_shrid.csv") %>%
  filter(
    shrid2 %in% unique(data_wo_features$shrid2),
    year %in% 2012:2021,
    category == "median-masked",
  ) %>%
  select(-category) %>%
  reshape(dir="w", idvar="shrid2", timevar="year")

features_parts <- list.files("data/clean/antipoverty/features", full.names = T, pattern = ".csv") 
features <- do.call(rbind, lapply(features_parts, read.csv))

data <- data_wo_features %>%
  left_join(viirs_annual, by = "shrid2") %>%
  left_join(features, by=c("lat", "lon"))

D <- data$D

X <- data %>% 
  select(
    starts_with("viirs_annual_"),
    "lat", "lon",
    starts_with("feature_")
  ) %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

nfeatures <- 1001 # 1001
X <- X[, 1:nfeatures] 

Y <- data$Ycons

K <- 5
B <- 500
for(n in c(1000, 2000, 3000)){
  for(tau in seq(from = 0, to = 0.5, by = 0.1)){
    
    out <- foreach(
      b = c(1:B), .combine = append
    ) %do% {
      set.seed(b)
      draw <- DGP(n = n, X = X, D = D, Y = Y, tau = tau)

      set.seed(123)
      rsv_surrogate_cv(X = draw$X, D = draw$D, Y = draw$Y, nfolds = K, se.boot = F)
    }
    
    out$TE <- mean(Y[D == 1], na.rm = T) + tau - mean(Y[D == 0], na.rm = T)
    
    output_path <- sprintf("data/clean/antipoverty/simulations/rsv_surrogate_n%s_tau%.1f.rds", n, tau)
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, output_path)
    cat(sprintf("Saved results to: %s\n", output_path))
  }
}


results <- list()
for(n in c(1000, 2000, 3000)){
  for(tau in seq(from = 0, to = 0.5, by = 0.1)){
    data <- readRDS(sprintf("data/clean/antipoverty/simulations/rsv_surrogate_n%s_tau%.1f.rds", n, tau))
    coef_rsv_cv_boot <- sapply(data, function(x) x$coef_rsv_cv)
    coef_surrogate_cv_boot <- sapply(data, function(x) x$coef_surrogate_cv)
    
    TE <- data$TE
    
    results[[length(results) + 1]] <- data.frame(
      n = n, # sample_size
      tau = tau,
      theta = 0.07 + tau,
      TE = TE,
      
      rmse_rsv = rmse(coef_rsv_cv_boot - TE),
      rmse_surrogate = rmse(coef_surrogate_cv_boot - TE),
      
      bias_rsv = mean(coef_rsv_cv_boot) - TE,
      bias_surrogate = mean(coef_surrogate_cv_boot) - TE
    )
  }
}

# Combine all rows into a single data frame
summary_df <- do.call(rbind, results) %>% pivot_longer(
  cols = c(starts_with("coef_"), starts_with("rmse_"), starts_with("bias_")),
  names_to = c(".value", "estimator"),
  names_sep = "_"
)
summary_df$Y <- "Ycons"

output_path <- "data/clean/antipoverty/rsv_vs_surrogate.csv"
write.csv(summary_df, output_path, row.names = F)
cat(sprintf("Saved results to: %s\n", output_path))
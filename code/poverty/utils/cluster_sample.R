suppressPackageStartupMessages({
  library(dplyr)
})

cluster_sample <- function(data, mle=list(cluster_var="clusters_test")) {
  # Extract unique cluster IDs from the specified cluster variable
  cluster_var <- mle$cluster_var 
  cluster_ids <- unique(data[[cluster_var]])
  
  # Sample cluster IDs with replacement. 
  # The number of sampled clusters is equal to the total number of unique clusters.
  sampled_clusters <- sample(
    x = cluster_ids, 
    size = length(cluster_ids), 
    replace = TRUE
    )
  
  # For each sampled cluster, extract all observations and combine them into a single data frame
  data[data[[cluster_var]] %in% sampled_clusters, ]
}
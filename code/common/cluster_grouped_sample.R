# Load required packages
library(dplyr)
library(purrr)

# Function: Clustered resampling stratified by a grouping variable
cluster_grouped_sample <- function(data, mle=list(S_var="S_var", cluster_var="village_id")) {
  # Extract variable names from the input list
  S_var <- mle$S_var 
  cluster_var <- mle$cluster_var 
  
  
  data %>%
    # Split the data by the grouping variable (e.g., study arm)
    split(.[[S_var]]) %>% 
    
    # Resample clusters within each group and combine results
    map_df(~{
      # Get all unique clusters within the group
      cluster_ids <- unique(.x[[cluster_var]])
      
      # Sample cluster IDs with replacement. The number of sampled clusters is equal to the total number of unique clusters.
      sampled_clusters <- sample(x = cluster_ids, size = length(cluster_ids), replace = TRUE)

      # For each sampled cluster, pull all rows corresponding to each sampled cluster,
      # Combine rows across all sampled clusters
      bind_rows(lapply(sampled_clusters, function(i) .x[.x[[cluster_var]] %in% i,]))
    })
}

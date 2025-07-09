library(readr)

# Load mosaik feature dataset
features <- read_csv(
  "data/raw/antipoverty/features/[RAW] Centroid_features.csv",
  col_names = c("lat", "lon", paste0("feature_", 1:4000)),
  skip = 1,
  show_col_types = F
)

# Estimate number of rows per 100MB
bytes_per_row <- as.numeric(object.size(features[1:1000, ])) / 1000 * 2.5
target_size_bytes <- 100 * 1024^2  # 100 MB
rows_per_chunk <- floor(target_size_bytes / bytes_per_row)

# Split and write
n_rows <- nrow(features)
num_chunks <- ceiling(n_rows / rows_per_chunk)

for (i in 1:num_chunks) {
  start_row <- (i - 1) * rows_per_chunk + 1
  end_row <- min(i * rows_per_chunk, n_rows)
  chunk <- features[start_row:end_row, ]
  write_csv(chunk, sprintf("data/raw/antipoverty/features/features_part%02d.csv", i))
}


rm(list = ls())

# Load necessary libraries
library(dplyr)
library(readr)
library(haven)
library(sf)
library(geosphere)
library(tidyr)
sf_use_s2(FALSE)

####
d_rename <- c(
  "sri potti sriramulu nellore" = "nellore",
  "ysr kadapa" = "kadapa"
)
sd_rename <- c("sirpur town" = "sirpur t")
shrug_data <- read_csv(
  "~/MIT Dropbox/Haya Alsharif/analysis_image_project/raw_data/shrug-shrid-keys-csv/shrid_loc_names.csv",
  col_select = c(shrid2, state_name, district_name, subdistrict_name),
  show_col_types = F
) %>%
  filter(state_name == "andhra pradesh") %>%
  mutate(
    district_name = recode(district_name, !!!d_rename),
    subdistrict_name = recode(subdistrict_name, !!!sd_rename),
  )

wave_levels <- c(
  "1" = "Treatment",
  "2" = "Buffer", 
  "3" = "Control",
  "D" = "Holdout" # non-study mandals
)

study_data <- read_dta(
  "data/raw/antipoverty/muralidharanetal_replication_package/20141346_data/data/balance-for-ap-mandal-comparison.dta",
  col_select = c(uniqueM, district_name, subdistrict_name, wave) #, mandalInStudy)
) %>%
  rename(clusters = uniqueM) %>%
  mutate(
    district_name = tolower(district_name),
    subdistrict_name =  gsub("[()]", "", gsub(".", " ", tolower(subdistrict_name), fixed = T)),
    wave = recode_factor(wave, !!!wave_levels),
    D = if_else(wave=="Treatment", 1, if_else(wave %in% c("Control", "Buffer"), 0, NA))
  )

shrug_study_data <- shrug_data %>% 
  inner_join(study_data, by = c("district_name", "subdistrict_name")) %>%
  select(-state_name, -district_name, -subdistrict_name)

####
secc_cons_urban <- read_csv(
  "data/raw/shrug/shrug-secc-cons-urban-csv/secc_cons_urban_shrid.csv", 
  col_select = c(shrid2, Ycons=secc_cons_pc_urban),
  show_col_types = F
)

secc_cons_rural <- read_csv(
  "data/raw/shrug/shrug-secc-cons-rural-csv/secc_cons_rural_shrid.csv", 
  col_select = c(shrid2, Ycons=secc_cons_pc_rural),
  show_col_types = F
)

secc_cons <- bind_rows(secc_cons_urban, secc_cons_rural) %>%
  distinct(shrid2, .keep_all = T) %>%
  mutate(Ycons = as.integer(Ycons<=19000))


####
secc_urban <- read_csv(
    "data/raw/shrug/shrug-secc-parsed-urban-csv/secc_urban_shrid.csv",
    col_select = c(shrid2, tot_p, tot_f),
    show_col_types = F
  ) %>%
  mutate(urban = 1)

secc_rural <- read_csv(
    "data/raw/shrug/shrug-secc-mord-rural-csv/secc_rural_shrid.csv",
    col_select = c(shrid2, tot_p, tot_f, Y05k=inc_5k_plus_share, Y10k=inc_10k_plus_share),
    show_col_types = F
  ) %>%
  mutate(urban = 0)

secc <- bind_rows(secc_urban, secc_rural) %>%
  distinct(shrid2, .keep_all = T) %>%
  mutate(
    Y05k = as.integer(Y05k==0),
    Y10k = as.integer(Y10k==0)
  )

###

viirs_annual <- read_csv(
  "data/raw/shrug/shrug-viirs-annual-csv/viirs_annual_shrid.csv",
  show_col_types = F
  ) %>%
  filter(
    year %in% 2012:2021,
    category == "median-masked",
  ) %>%
  select(-category) %>%
  pivot_wider(
    id_cols = shrid2,
    names_from = year,
    values_from = c(
      viirs_annual_min, viirs_annual_max, viirs_annual_mean,
      viirs_annual_sum, viirs_annual_num_cells
    ),
    names_sep = "."
  )

###

data <- shrug_study_data %>%
  left_join(secc, by = "shrid2") %>% 
  left_join(secc_cons, by = "shrid2") %>%
  left_join(viirs_annual, by = "shrid2")


####

# Load centroid-based coordinates
shrids <- st_read("data/raw/shrug/shrug-shrid-poly-shp/shrid2_open.shp", quiet=T) %>%
  filter(shrid2 %in% data$shrid2)

centroids <- st_centroid(shrids)
coords <- st_coordinates(centroids)
shrids$centroid_lon <- coords[, 1]
shrids$centroid_lat <- coords[, 2]

shrids_coor <- shrids %>%
  as.data.frame() %>%
  select(shrid2, centroid_lat, centroid_lon)

data <- data %>%
  left_join(shrids_coor, by = "shrid2")

####

# Load mosaik feature dataset
features_parts <- list.files(
  "data/raw/antipoverty/features", 
  pattern = "features_part\\d{2}\\.csv$",
  full.names = T
  ) 
features <- do.call(rbind, lapply(features_parts, read_csv, show_col_types = F))

# Convert coordinates to matrix (in degrees)
centroids_coor <- data %>%
  select(centroid_lon, centroid_lat) %>% 
  as.matrix()

features_coor <- features %>%
  select(lon, lat) %>% 
  as.matrix()

# Compute distance matrix (in meters)
dist_matrix <- distm(centroids_coor, features_coor, fun = distHaversine)

# Get the index of the nearest neighbor and its distance (in km)
min_indices <- max.col(-dist_matrix)  # Returns column index of the min value
min_distances <- dist_matrix[cbind(1:nrow(dist_matrix), min_indices)] / 1000  # Convert to km

# Assign nearest feature to shrids
data$mosiak_idx <- min_indices
data$distance_km <- min_distances

# Retrieve the lat/lon of the matched mosaik features
data$lat <- features$lat[data$mosiak_idx]
data$lon <- features$lon[data$mosiak_idx]

data <- data %>% 
  filter(distance_km <= 1.5) %>% # Filter by 1.5 km radius
  select(-mosiak_idx, -distance_km, -centroid_lon, -centroid_lat) %>%
  left_join(features, by=c("lat", "lon")) %>%
  filter(tot_p >= 100)

write.csv(data, "data/clean/antipoverty/data.csv", row.names = F)

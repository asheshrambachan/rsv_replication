rm(list = ls())

# === Load libraries ===
library(dplyr)
library(readr)
library(haven)
library(sf)
library(geosphere)
library(tidyr)
options(readr.show_col_types = F)
sf_use_s2(F)

# === Load and Clean SHRUG Location Data ===
d_rename <- c("sri potti sriramulu nellore" = "nellore", "ysr kadapa" = "kadapa")
sd_rename <- c("sirpur town" = "sirpur t")

shrug_loc <- read_csv(
  "data/raw/shrug-shrid-keys-csv/shrid_loc_names.csv",
  col_select = c(shrid2, state_name, district_name, subdistrict_name)
) %>%
  filter(state_name == "andhra pradesh") %>%
  mutate(
    district_name = recode(district_name, !!!d_rename),
    subdistrict_name = recode(subdistrict_name, !!!sd_rename)
  )

# === Load and Clean Muralidharan et al Data ===
wave_levels <- c(
  "1" = "Treatment",
  "2" = "Buffer", 
  "3" = "Control",
  "D" = "Holdout" # non-study mandals
)

study_data <- read_dta(
  "data/raw/muralidharanetal_replication_package/20141346_data/data/balance-for-ap-mandal-comparison.dta",
  col_select = c(uniqueM, district_name, subdistrict_name, wave) 
) %>%
  rename(clusters = uniqueM) %>%
  mutate(
    district_name = tolower(district_name),
    subdistrict_name =  gsub("[()]", "", gsub(".", " ", tolower(subdistrict_name), fixed = T)),
    wave = recode_factor(wave, !!!wave_levels),
    D = case_when(
      wave == "Treatment" ~ 1,
      wave %in% c("Control", "Buffer") ~ 0,
      TRUE ~ NA
    )
  )

shrug_study_data <- shrug_loc %>% 
  inner_join(study_data, by = c("district_name", "subdistrict_name")) #%>%
  # select(-state_name, -district_name, -subdistrict_name)


# === Load SECC Consumption ===
secc_cons_urban <- read_csv(
  "data/raw/shrug-secc-cons-urban-csv/secc_cons_urban_shrid.csv", 
  col_select = c(shrid2, Ycons=secc_cons_pc_urban)
)

secc_cons_rural <- read_csv(
  "data/raw/shrug-secc-cons-rural-csv/secc_cons_rural_shrid.csv", 
  col_select = c(shrid2, Ycons=secc_cons_pc_rural)
)

secc_cons <- bind_rows(secc_cons_urban, secc_cons_rural) %>%
  distinct(shrid2, .keep_all = T) %>%
  mutate(Ycons = as.integer(Ycons<=19000))


# === Load SECC Income ===
secc_income_urban <- read_csv(
    "data/raw/shrug-secc-parsed-urban-csv/secc_urban_shrid.csv",
    col_select = c(shrid2, tot_p, tot_f)
  ) %>%
  mutate(urban = 1)

secc_income_rural <- read_csv(
    "data/raw/shrug-secc-mord-rural-csv/secc_rural_shrid.csv",
    col_select = c(shrid2, tot_p, tot_f, Ylow=inc_5k_plus_share, Ymid=inc_10k_plus_share)
  ) %>%
  mutate(urban = 0)

secc_income <- bind_rows(secc_income_urban, secc_income_rural) %>%
  distinct(shrid2, .keep_all = T) %>%
  mutate(
    Ylow = as.integer(Ylow==0),
    Ymid = as.integer(Ymid==0)
  )


# === Load VIIRS Light Data ===
viirs_annual <- read_csv("data/raw/shrug-viirs-annual-csv/viirs_annual_shrid.csv") %>%
  filter(
    year %in% 2012:2021,
    category == "median-masked",
  ) %>%
  select(-category) %>%
  pivot_wider(
    id_cols = shrid2,
    names_from = year,
    values_from = c(viirs_annual_min, viirs_annual_max, viirs_annual_mean,
                    viirs_annual_sum, viirs_annual_num_cells),
    names_sep = "."
  )


# === Merge All Inputs ===
data <- shrug_study_data %>%
  left_join(secc_cons, by = "shrid2") %>%
  left_join(secc_income, by = "shrid2") %>% 
  left_join(viirs_annual, by = "shrid2")


# === Add Coordinates from SHRUG Polygons ===
shrids <- st_read("data/raw/shrug-shrid-poly-shp/shrid2_open.shp", quiet = T) %>%
  filter(shrid2 %in% data$shrid2)

centroids <- st_centroid(shrids)
coords <- st_coordinates(centroids)

shrids_coords <- shrids %>%
  mutate(
    centroid_lon = coords[, 1], 
    centroid_lat = coords[, 2]
  ) %>%
  as.data.frame() %>%
  select(shrid2, centroid_lat, centroid_lon)

data <- data %>%
  left_join(shrids_coords, by = "shrid2")

# === Assign Mosaic Features via Nearest Neighbor ===
# Load mosaik feature dataset
features <- list.files(
  "data/interim/poverty/features", 
  pattern = "features_part\\d{2}\\.csv$",
  full.names = T
) %>%
  lapply(read_csv) %>%
  bind_rows()

dist_matrix <- distm(
  data %>% select(centroid_lon, centroid_lat) %>% as.matrix(),
  features %>% select(lon, lat) %>% as.matrix(),
  fun = distHaversine
)

min_indices <- max.col(-dist_matrix)
min_distances_km <- dist_matrix[cbind(1:nrow(dist_matrix), min_indices)] / 1000

data <- data %>%
  mutate(
    mosaic_idx = min_indices,
    distance_km = min_distances_km,
    lat = features$lat[mosaic_idx],
    lon = features$lon[mosaic_idx]
  ) %>%
  filter(distance_km <= 1.5) %>%
  select(-mosaic_idx, -distance_km, -centroid_lat, -centroid_lon) %>%
  left_join(features, by = c("lat", "lon")) %>%
  filter(tot_p >= 100)

# === Export Final Merged Data ===
path <- "data/processed/poverty/data.csv"
write.csv(data, path, row.names = F)
cat(sprintf("Saved cleaned data to: %s\n", path))
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

shrug_loc <- read.table(
  "https://dataverse.harvard.edu/api/access/datafile/10742739",
  sep="\t", header=TRUE
  ) %>%
  select(shrid2, state_name, district_name, subdistrict_name) %>%
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


# === Load SECC 
# Consumption
secc_cons_urban <- read.table(
  "https://dataverse.harvard.edu/api/access/datafile/10742795",
  sep="\t", header=TRUE
  ) %>%
  select(shrid2, Ycons_raw=secc_cons_pc_urban) %>%
  mutate(urban = 1)

secc_cons_rural <- read.table(
    "https://dataverse.harvard.edu/api/access/datafile/10742743",
    sep="\t", header=TRUE
  ) %>%
  select(shrid2, Ycons_raw=secc_cons_pc_rural) %>%
  mutate(urban = 0)

secc_cons <- bind_rows(secc_cons_urban, secc_cons_rural) # 162 shrids appear in both urban and rural data

# Income
secc_income_urban <- read.table(
  "https://dataverse.harvard.edu/api/access/datafile/10742848", 
  sep="\t", header=TRUE
  ) %>% 
  select(shrid2, tot_p, tot_f) %>%
  mutate(
    urban = 1,
    tot_f = if_else(tot_p==0, 0, tot_f),
    Ylowinc_raw = NA,
    Ymidinc_raw = NA
    )

secc_income_rural <- read_dta(
  "https://dataverse.harvard.edu/api/access/datafile/10742876",
  col_select = c(shrid2, tot_p, tot_f, inc_5k_plus_share, inc_10k_plus_share)
  ) %>%
  rename(
    Ylowinc_raw = inc_5k_plus_share, 
    Ymidinc_raw = inc_10k_plus_share
  ) %>%
  mutate(
    urban = 0,
    tot_f = if_else(tot_p==0, 0, tot_f)
    )

secc_income <- bind_rows(secc_income_urban, secc_income_rural)

# Merge income and consumption data
secc <- full_join(secc_cons, secc_income, by = c("shrid2", "urban")) 


# === Load VIIRS Light Data ===
viirs_annual <- read_dta("https://dataverse.harvard.edu/api/access/datafile/10742856") %>%
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
  left_join(secc, by = "shrid2") %>%
  left_join(viirs_annual, by = "shrid2") %>%
  filter(tot_p >= 100) %>%
  mutate(
    Ylowinc = as.integer(Ylowinc_raw==0),
    Ymidinc = as.integer(Ymidinc_raw==0),
    Ycons = as.integer(Ycons_raw <= quantile(Ycons_raw, 0.25))
  )

# === Add Coordinates from SHRUG Polygons ===
shrids <- st_read("data/raw/shrug_v2.0/shrug-shrid-poly-gpkg/shrid2_open.gpkg", quiet = T) %>%
  filter(shrid2 %in% data$shrid2)
centroids <- st_centroid(shrids) # note that these centroids are slightly different from those computed via python and used in the code to query mosiak api "Centroids_coordinates.csv"
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
  left_join(features, by = c("lat", "lon")) 

# === Export Final Merged Data ===
path <- "data/processed/poverty/data.csv"
write.csv(data, path, row.names = F)
cat(sprintf("Saved cleaned data to: %s\n", path))

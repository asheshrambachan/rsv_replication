rm(list = ls())

# === Load libraries ===
library(readr)
library(sf)
options(readr.show_col_types = F)

# === Helper Function ===
save_shapefile <- function(sf_object, path) {
  dir.create(dirname(path), recursive = T, showWarnings = F)
  st_write(sf_object, path, append = F, quiet = T)
  cat(sprintf("Saved cleaned shapefile to: %s\n", path))
}


# === Clean and Save State Shapefile ===
state <- st_read("data/raw/shrug-pc11state-poly-shp/state.shp", quiet = T) %>%
  filter(s_name=="Andhra Pradesh")

save_shapefile(state, "data/processed/poverty/state/state.shp")


# === Clean and Save District Shapefile === 
d_rename <- c(
  "Sri Potti Sriramulu Nellore" =  "Nellore",
  "Y.S.R." = "Kadapa"
)
districts <- st_read("data/raw/shrug-pc11dist-poly-shp/district.shp", quiet = T) %>%
  filter(pc11_s_id %in% state$pc11_s_id) %>%
  mutate(d_name = recode(d_name, !!!d_rename))

save_shapefile(districts, "data/processed/poverty/districts/districts.shp")


# === Clean and Save SHRID2 Shapefile ===
shrid_ids <- read_csv("data/processed/poverty/data.csv", col_select = shrid2) %>% 
  pull(shrid2) %>%
  unique()

shrids <- st_read("data/raw/shrug-shrid-poly-shp/shrid2_open.shp", quiet = T) %>%
  filter(shrid2 %in% shrid_ids)

save_shapefile(shrids, "data/processed/poverty/shrids/shrids.shp")
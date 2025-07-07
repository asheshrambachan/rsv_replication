library(readr)

rm(list = ls())

# Clean and filter state shapefile
raw_shp <- "data/raw/shrug/shrug-pc11state-poly-shp/state.shp"
if (!file.exists(raw_shp)) {
  stop(
    paste(
      "File does not exist:", raw_shp, "\n",
      "Please follow these steps to download the required data:\n",
      "1) Visit https://www.devdatalab.org/shrug_download/\n",
      "2) Navigate to the 'Open Polygons and Spatial Statistics' tab\n",
      "3) For 'PC11 State Polygons', click on 'SHP' to download\n",
      "4) Unzip the downloaded file\n",
      "5) Place the unzipped folder named 'shrug-pc11state-poly-shp' into the directory: data/raw/shrug/"
    )
  )
}

state <- st_read(raw_shp, quiet=T) %>%
  filter(s_name=="Andhra Pradesh")

output_path <- "data/clean/antipoverty/state/state.shp"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
st_write(state, output_path, append=F, quiet=T)
cat(sprintf("Saved cleaned shapefiles to: %s\n", output_path))

# Clean and filter district shapefile 
raw_shp <- "data/raw/shrug/shrug-pc11dist-poly-shp/district.shp"
if (!file.exists(raw_shp)) {
  stop(
    paste(
      "File does not exist:", raw_shp, "\n",
      "Please follow these steps to download the required data:\n",
      "1) Visit https://www.devdatalab.org/shrug_download/\n",
      "2) Navigate to the 'Open Polygons and Spatial Statistics' tab\n",
      "3) For 'PC11 District Polygons', click on 'SHP' to download\n",
      "4) Unzip the downloaded file\n",
      "5) Place the unzipped folder named 'shrug-pc11dist-poly-shp' into the directory: data/raw/shrug/"
    )
  )
}

d_rename <- c(
  "Sri Potti Sriramulu Nellore" =  "Nellore",
  "Y.S.R." = "Kadapa"
)

districts <- st_read(raw_shp, quiet=T) %>%
  filter(pc11_s_id %in% state$pc11_s_id) %>%
  mutate(d_name = recode(d_name, !!!d_rename))

output_path <- "data/clean/antipoverty/districts/districts.shp"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
st_write(districts, output_path, append=F, quiet=T)
cat(sprintf("Saved cleaned shapefiles to: %s\n", output_path))

# Clean and filter shrid shapefile 
raw_shp <- "data/raw/shrug/shrug-shrid-poly-shp/shrid2_open.shp"
if (!file.exists(raw_shp)) {
  stop(
    paste(
      "File does not exist:", raw_shp, "\n",
      "Please follow these steps to download the required data:\n",
      "1) Visit https://www.devdatalab.org/shrug_download/\n",
      "2) Navigate to the 'Open Polygons and Spatial Statistics' tab\n",
      "3) For 'Shrid Polygons', click on 'SHP' to download\n",
      "4) Unzip the downloaded file\n",
      "5) Place the unzipped folder named 'shrug-shrid-poly-shp' into the directory: data/raw/shrug/"
    )
  )
}

shird_ids <- read_csv("data/clean/antipoverty/pca.csv", col_select = "shrid2", show_col_types = F) %>%
  distinct()
shrids <- st_read(raw_shp, quiet=T) %>%
  right_join(., shird_ids, by="shrid2")

output_path <- "data/clean/antipoverty/shrids/shrids.shp"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
st_write(shrids, output_path, append=F, quiet=T)
cat(sprintf("Saved cleaned shapefiles to: %s\n", output_path))





data_wo_features <- read.csv("data/clean/antipoverty/data_wo_features.csv") %>%
  rename(c(
    Y05k = y_05k,
    Y10k = y_10k,
    Ycons = y_cons,
  )) %>%
  select(shrid2, lat, lon, tot_p, tot_f, urban, wave, clusters, D, Y05k, Y10k, Ycons)

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

write.csv(data, "data/clean/antipoverty/data.csv", row.names = F)

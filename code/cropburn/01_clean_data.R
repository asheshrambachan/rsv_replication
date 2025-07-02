# Clear environment
rm(list = ls())

# Load required libraries
library(haven)    # For reading .dta files
library(dplyr)    # For data manipulation
library(sf)       # For spatial data
library(readr)    # Read csv files with specified column type

# Load Jack et al to SHRUG Village Mapping
shrug_jacketal_mapping <- read_csv(
  "data/raw/cropburn/shrug_jacketal_mapping.csv", 
  col_types = cols(pc11_tv_id = "c"),
  col_select = c("pc11_tv_id", "village_id")
  )

# Read and Clean Jack et al Plot-Level Data
fields <- read_dta("data/raw/cropburn/jacketal_replication_package/Data/Analysis/dataset_for_analysis_plot_level.dta") %>%
  
  # Remove incomplete or alternate IDs
  select(-village_id, -blp_plot_id, -sc_plot_id) %>%
  
  # Rename key variables for clarity
  rename(
    village_id = blp_village_id,
    R_prob     = rs_p_not_burned_cont,
    R_bal      = rs_p_not_burned_bin_min,
    R_max      = rs_p_not_burned_bin_max
  ) %>%
  
  # Create new variables and logical indicators
  mutate(
    # Convert district to a proper factor
    district = as_factor(district_fix, levels = "labels"),
    
    # Define treatment groups
    pesonly = pes800 | pes1600,
    pesuct  = pes800uct25 | pes800uct50,
    pesany  = pesonly | pesuct,
    D       = pesany,  # Use 'D' as binary treatment indicator
    
    # Define GT outcome
    monp_p_burnt_any = pmax(monp_pv_burnt_any1, monp_pv_burnt_any2, monp_pv_burnt_any3, na.rm = TRUE),
    Y_sc    = !sc_pv_burnt_any,
    Y_mon   = !monp_p_burnt_any,
    Y       = !rs_label,  # Original GT outcome 
    Y_recon = pmin(Y_sc, Y_mon, na.rm = TRUE),  # Reconstructed GT outcome
    
    # Sample indicator
    is_sc = !is.na(Y_sc),
    S     = if_else(is_sc, "o", "e")
  ) %>%
  
  # Filter out incomplete rows
  filter(
    !is.na(village_id),                        # Drop rows with missing village IDs
    !(is.na(R_bal) & is.na(Y_recon))           # Drop rows missing both R and Y
  ) %>%
  
  # Keep only needed variables
  select(
    village_id, district, R_bal, R_max, R_prob, Y, Y_recon, S, D,
    rabovemed, baseline_complete, listing_not_complete, vill_added_back
  ) %>%
  
  # Convert relevant columns to numeric (for modeling later)
  mutate(across(
    c(D, Y, Y_recon, starts_with("R_"), baseline_complete, 
      listing_not_complete, vill_added_back, rabovemed),
    as.numeric
  )) 

# Merge SHRUG Mapping into Fields Data
fields <- right_join(shrug_jacketal_mapping, fields, by = "village_id")

# Save cleaned dataset
write.csv(fields, "data/clean/cropburn/data.csv", row.names = FALSE)

# Clean and filter district shapefile (Bathinda & Faridkot only)
districts <- st_read("data/raw/shrug/shrug-pc11dist-poly-shp/district.shp", quiet=T) %>%
  filter(d_name %in% c("Bathinda", "Faridkot"))

st_write(districts, "data/clean/cropburn/districts/districts.shp", append=F)

# Clean and filter village shapefile (within selected districts)
villages <- st_read("data/raw/shrug/shrug-pc11-village-poly-shp/village_modified.shp", quiet=T) %>%
  filter(pc11_d_id %in% districts$pc11_d_id) %>%
  select(-mdds_og)

st_write(villages, "data/clean/cropburn/villages/villages.shp", append=F)


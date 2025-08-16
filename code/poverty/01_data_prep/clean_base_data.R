# =============================================================================
# Title: Clean and Merge Poverty-Related Data (SHRUG, SECC, VIIRS, Study Data)
# Purpose: Prepares base data by merging raw spatial and socioeconomic data
# Output: data/poverty/processed/base_data.csv
# =============================================================================

rm(list = ls())

## Load Libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(haven)
  library(tidyr)
})
options(readr.show_col_types = FALSE)


# -----------------------------------------------------------------------------
# 1. Load and Clean SHRUG Location Data
# -----------------------------------------------------------------------------

district_name_map <- c("sri potti sriramulu nellore" = "nellore", 
                       "ysr kadapa" = "kadapa")
subdistrict_name_map <- c("sirpur town" = "sirpur t")

shrug_loc <- read.table(
  "https://dataverse.harvard.edu/api/access/datafile/10742739",
  sep = "\t", header = TRUE
) %>%
  select(shrid2, state_name, district_name, subdistrict_name) %>%
  filter(state_name == "andhra pradesh") %>%
  mutate(
    district_name    = recode(district_name, !!!district_name_map),
    subdistrict_name = recode(subdistrict_name, !!!subdistrict_name_map)
  )


# -----------------------------------------------------------------------------
# 2. Load and Clean Muralidharan et al. Study Data
# -----------------------------------------------------------------------------

wave_levels <- c(
  "1" = "Treatment",
  "2" = "Buffer", 
  "3" = "Control",
  "D" = "Holdout"
)

study_data <- read_dta(
  "data/poverty/raw/muralidharanetal_replication_package/20141346_data/data/balance-for-ap-mandal-comparison.dta",
  col_select = c(uniqueM, district_name, subdistrict_name, wave) 
) %>%
  rename(clusters = uniqueM) %>%
  mutate(
    district_name    = tolower(district_name),
    subdistrict_name = gsub("[()]", "", gsub(".", " ", tolower(subdistrict_name), fixed = TRUE)),
    wave             = recode_factor(wave, !!!wave_levels),
    D                = case_when(
      wave == "Treatment"              ~ 1, # Experimental: Treated
      wave %in% c("Control", "Buffer") ~ 0, # Experimental: Untreated
      TRUE                             ~ NA # Holdout (Observational)
    )
  )

shrug_study_data <- shrug_loc %>%
  inner_join(study_data, by = c("district_name", "subdistrict_name"))


# -----------------------------------------------------------------------------
# 3. Load and Merge SECC Data
# -----------------------------------------------------------------------------

## 3.1 Consumption
secc_cons <- read.table(
  "https://dataverse.harvard.edu/api/access/datafile/10742743",
  sep = "\t", header = TRUE
) %>%
  select(shrid2, Ycons_raw = secc_cons_pc_rural) %>%
  mutate(urban = 0)

## 3.2 Income
secc_income <- read_dta(
  "https://dataverse.harvard.edu/api/access/datafile/10742876",
  col_select = c(shrid2, tot_p, tot_f, inc_5k_plus_share, inc_10k_plus_share)
) %>%
  rename(
    Ylowinc_raw = inc_5k_plus_share, 
    Ymidinc_raw = inc_10k_plus_share
  ) %>%
  mutate(
    urban = 0,
    tot_f = if_else(tot_p == 0, 0, tot_f)
  )

## 3.3 Merge Consumption and Income
secc <- full_join(secc_cons, secc_income, by = "shrid2")


# -----------------------------------------------------------------------------
# 4. Merge All Data Sources
# -----------------------------------------------------------------------------

data <- shrug_study_data %>%
  left_join(secc, by = "shrid2") %>%
  filter(tot_p >= 100) %>%
  mutate(
    Ylowinc = as.integer(Ylowinc_raw == 0),
    Ymidinc = as.integer(Ymidinc_raw == 0),
    Ycons   = as.integer(Ycons_raw <= quantile(Ycons_raw, 0.25, na.rm = TRUE))
  ) # 8312
print(quantile(data$Ycons_raw, 0.25, na.rm = TRUE)) # 18946.61 

# -----------------------------------------------------------------------------
# 5. Export Final Merged Dataset
# -----------------------------------------------------------------------------
path <- "data/poverty/processed/base_data.csv"
write_csv(data, path)
cat(sprintf("Saved cleaned base data to: %s\n", path))
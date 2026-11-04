# =============================================================================
# Smartcard — Study, Location, and Welfare Data
#
# Assembles the base village-level dataset by merging three sources:
#   - Muralidharan et al. experimental study data (treatment assignment)
#   - SHRUG subdistrict location table (shrid2 identifiers)
#   - SECC consumption and income data (poverty outcomes)
#
# Villages with fewer than 100 residents (tot_p < 100) are dropped. Poverty
# outcomes are binarised: Ycons flags the bottom consumption quartile;
# Ylowinc/Ymidinc flag zero income-share at each threshold.
#
# Output: data/interim/smartcards/base_data.Rds
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
})

# Additional measurement sources and original columns to retain (issue #6).
# Each selection includes shrid2. The SECC rural selection also includes
# the existing population and primary-outcome inputs.
secondary_source_urls <- c(
  secc_rural_shrid = "https://dataverse.harvard.edu/api/access/datafile/10742876",
  ec13_shrid = "https://dataverse.harvard.edu/api/access/datafile/10742666",
  pc11_pca_clean_shrid = "https://dataverse.harvard.edu/api/access/datafile/10742704",
  secc_cons_rural_shrid = "https://dataverse.harvard.edu/api/access/datafile/10742743",
  pc11_vd_clean_shrid = "https://dataverse.harvard.edu/api/access/datafile/10742721",
  antyodaya_shrid = "https://dataverse.harvard.edu/api/access/datafile/10742626"
)

secondary_source_vars <- list(
  secc_rural_shrid = c(
    "shrid2", "tot_p", "tot_f", "inc_5k_plus_share", "inc_10k_plus_share",
    "ent_own_share",
    "salary_any_share",
    "salary_priv_share",
    "inc_source_manlab_share",
    "inc_source_cultiv_share",
    "inc_source_enterpr_share",
    "house_type1", "house_type2", "house_type3",
    "house_own1", "house_own2", "house_own3",
    "house_tax0", "house_tax1",
    "kisan_cc",
    "mech_farm_equip_share",
    "irr_equip_share",
    "ag_equip_share",
    "veh_two_three_share", "veh_four_share",
    "land_own_share",
    "irr_share1",
    "ed_prim_share",
    "ed_sec_share",
    "sc_share", "st_share", "disab_share",
    "num_room_mean",
    "roof_mat_solid_share",
    "wall_mat_solid_share"
  ),
  ec13_shrid = c(
    "shrid2",
    "ec13_emp_all", "ec13_emp_f", "ec13_emp_hired",
    "ec13_count_all", "ec13_emp_manuf", "ec13_emp_services"
  ),
  pc11_pca_clean_shrid = c(
    "shrid2",
    "pc11_pca_tot_work_p", "pc11_pca_tot_p",
    "pc11_pca_p_lit", "pc11_pca_p_06"
  ),
  secc_cons_rural_shrid = c(
    "shrid2", "secc_pov_rate_rural"
  ),
  pc11_vd_clean_shrid = c(
    "shrid2", "pc11_vd_comm_bank"
  ),
  antyodaya_shrid = c(
    "shrid2",
    "total_hhd_engaged_in_non_farm_ac",
    "total_hhd_mobilized_into_shg",
    "total_hhd_availing_pmjdy_bank_ac",
    "total_hhd_having_piped_water_con",
    "total_hhd"
  )
)

# -----------------------------------------------------------------------------
# 1. Load Muralidharan et al. Study Data
#
# wave encodes experimental assignment: 1 = treated (2010), 2 = untreated
# (2011), 3 = untreated (2012), D = observational holdout. D is set to NA for
# observational units — treatment was never assigned there.
# -----------------------------------------------------------------------------

sample_smartcard_levels <- c(
  "1" = "Experimental: Treated (2010)",
  "2" = "Experimental: Untreated (2011)",
  "3" = "Experimental: Untreated (2012)",
  "D" = "Observational (N/A)"
)

study_data <- read_dta(
  "data/raw/smartcards/113012-V1/20141346_data/data/balance-for-ap-mandal-comparison.dta",
  col_select = c(uniqueM, district_name, subdistrict_name, wave)
) %>%
  rename(clusters = ) %>%
  mutate(
    district_name    = tolower(district_name),
    subdistrict_name = gsub("[()]", "", gsub(".", " ", tolower(subdistrict_name), fixed = TRUE)),
    wave = recode_factor(wave, !!!sample_smartcard_levels),
    D = if_else(wave == "Experimental: Treated (2010)", 1L, 0L),
    clusters = paste(subdistrict_name, district_name, sep = " ")
  ) %>%
  select(-uniqueM)

# -----------------------------------------------------------------------------
# 2. Load SHRUG Location Table
#
# Maps subdistrict names to shrid2 identifiers. A small set of name
# discrepancies between SHRUG and the study data are corrected manually.
# -----------------------------------------------------------------------------

district_name_map    <- c("sri potti sriramulu nellore" = "nellore",
                          "ysr kadapa"                  = "kadapa")
subdistrict_name_map <- c("sirpur town" = "sirpur t", 
                          "khammam" = "khammam urban")

shrug_loc <- read.table(
  "https://dataverse.harvard.edu/api/access/datafile/10742739",
  sep = "\t", header = TRUE
) %>%
  select(shrid2, state_name, district_name, subdistrict_name) %>%
  filter(state_name == "andhra pradesh") %>%
  mutate(
    district_name    = recode(district_name,    !!!district_name_map),
    subdistrict_name = recode(subdistrict_name, !!!subdistrict_name_map)
  ) %>%
  select(-state_name)

# -----------------------------------------------------------------------------
# 3. Load Welfare and Additional Measurements
#
# Retain the requested source columns without transforming them. The existing
# temporary outcome inputs and tot_f convention are preserved. New Stata
# imports request the original file format to retain source labels.
# -----------------------------------------------------------------------------

secc_cons <- read.table(
  secondary_source_urls[["secc_cons_rural_shrid"]],
  sep = "\t", header = TRUE
) %>%
  select(
    all_of(secondary_source_vars[["secc_cons_rural_shrid"]]),
    Ycons_raw = secc_cons_pc_rural
  )

secc_income <- read_dta(
  secondary_source_urls[["secc_rural_shrid"]],
  col_select = all_of(secondary_source_vars[["secc_rural_shrid"]])
) %>%
  rename(Ylowinc_raw = inc_5k_plus_share,
         Ymidinc_raw = inc_10k_plus_share) %>%
  mutate(tot_f = if_else(tot_p == 0, 0, tot_f))

ec13 <- read_dta(
  paste0(secondary_source_urls[["ec13_shrid"]], "?format=original"),
  col_select = all_of(secondary_source_vars[["ec13_shrid"]])
)

pc11_pca <- read_dta(
  paste0(secondary_source_urls[["pc11_pca_clean_shrid"]], "?format=original"),
  col_select = all_of(secondary_source_vars[["pc11_pca_clean_shrid"]])
)

pc11_vd <- read_dta(
  paste0(secondary_source_urls[["pc11_vd_clean_shrid"]], "?format=original"),
  col_select = all_of(secondary_source_vars[["pc11_vd_clean_shrid"]])
)

antyodaya <- read_dta(
  paste0(secondary_source_urls[["antyodaya_shrid"]], "?format=original"),
  col_select = all_of(secondary_source_vars[["antyodaya_shrid"]])
)

# Each source must contribute at most one row per village.
measurement_tables <- list(
  secc_rural_shrid = secc_income,
  secc_cons_rural_shrid = secc_cons,
  ec13_shrid = ec13,
  pc11_pca_clean_shrid = pc11_pca,
  pc11_vd_clean_shrid = pc11_vd,
  antyodaya_shrid = antyodaya
)

for (source_name in names(measurement_tables)) {
  source_key <- measurement_tables[[source_name]]$shrid2
  if (anyNA(source_key) || any(source_key == "") || anyDuplicated(source_key) > 0L) {
    stop(sprintf("Expected unique, nonmissing shrid2 values in %s.", source_name),
         call. = FALSE)
  }
}

secc <- full_join(secc_cons, secc_income, by = "shrid2")

# -----------------------------------------------------------------------------
# 4. Merge and Binarise Outcomes
#
# Inner-join on subdistrict names (SHRUG x study data), then left-join welfare.
# Villages with < 100 residents are too small for reliable welfare estimates.
# Ycons = 1 flags the bottom consumption quartile (poorest 25%).
# Ylowinc/Ymidinc = 1 flag zero income-share at their respective thresholds.
# -----------------------------------------------------------------------------

base_data <- shrug_loc %>%
  inner_join(study_data, by = c("district_name", "subdistrict_name")) %>%
  left_join(secc, by = "shrid2") %>%
  filter(tot_p >= 100) %>%
  mutate(
    Ycons   = as.integer(Ycons_raw   <= quantile(Ycons_raw, 0.25, na.rm = TRUE)),
    Ylowinc = as.integer(Ylowinc_raw == 0),
    Ymidinc = as.integer(Ymidinc_raw == 0)
  ) %>%
  select(-ends_with("_raw"))

# Add measurements without restricting the established analysis sample.
base_data <- base_data %>%
  left_join(ec13, by = "shrid2") %>%
  left_join(pc11_pca, by = "shrid2") %>%
  left_join(pc11_vd, by = "shrid2") %>%
  left_join(antyodaya, by = "shrid2")

cat(sprintf("Base data: %d villages\n", nrow(base_data)))
cat(sprintf("  Experimental: %d  |  Observational: %d\n",
            sum(!is.na(base_data$D)), sum(is.na(base_data$D))))

# -----------------------------------------------------------------------------
# 5. Save
# -----------------------------------------------------------------------------

out_path <- "data/interim/smartcards/base_data.Rds"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(base_data, out_path)
cat("Saved to", out_path, "\n")

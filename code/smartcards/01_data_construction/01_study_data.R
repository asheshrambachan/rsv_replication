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
  "data/raw/smartcards/muralidharanetal_replication_package/balance-for-ap-mandal-comparison.dta",
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
subdistrict_name_map <- c("sirpur town" = "sirpur t")

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
# 3. Load SECC Welfare Data
#
# Two separate files: per-capita rural consumption, and household income-share
# at two thresholds (5k and 10k rupees/year). tot_f is zeroed when tot_p == 0
# to avoid division issues downstream.
# -----------------------------------------------------------------------------

secc_cons <- read.table(
  "https://dataverse.harvard.edu/api/access/datafile/10742743",
  sep = "\t", header = TRUE
) %>%
  select(shrid2, Ycons_raw = secc_cons_pc_rural)

secc_income <- read_dta(
  "https://dataverse.harvard.edu/api/access/datafile/10742876",
  col_select = c(shrid2, tot_p, tot_f, inc_5k_plus_share, inc_10k_plus_share)
) %>%
  rename(Ylowinc_raw = inc_5k_plus_share,
         Ymidinc_raw = inc_10k_plus_share) %>%
  mutate(tot_f = if_else(tot_p == 0, 0, tot_f))

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

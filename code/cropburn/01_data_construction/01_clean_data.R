# =============================================================================
# Crop Burning — Clean Tabular Data
#
# Cleans the Jack et al. (2022) plot-level experimental data and merges it
# with SHRUG village identifiers needed to link to satellite features.
#
# Inputs:
#   data/raw/cropburn/jacketal_shrug_mapping.csv
#   data/raw/cropburn/jacketal_replication_package/Data/Analysis/
#     dataset_for_analysis_plot_level.dta
#
# Output:
#   data/clean/cropburn/cropburn_data.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(haven)   # read Stata .dta files
  library(dplyr)   # data manipulation
  library(readr)   # read/write CSVs
})


# -----------------------------------------------------------------------------
# 1. Load the village-level crosswalk
#
# Jack et al. use their own village identifiers. This mapping table links each
# Jack et al. village to its SHRUG pc11 village ID, which we need to merge in
# satellite-derived features downstream.
# -----------------------------------------------------------------------------

shrug_mapping <- read_csv(
  "data/raw/cropburn/jacketal_shrug_mapping.csv",
  col_types = cols(pc11_tv_id = "c"),
  col_select = c("pc11_tv_id", "village_id")
)


# -----------------------------------------------------------------------------
# 2. Load and reshape the raw plot-level data
#
# The Jack et al. dataset has one row per plot. We keep only the variables
# needed for the RSV analysis and rename them to the conventions used
# throughout this package.
# -----------------------------------------------------------------------------

plots_raw <- read_dta(
  "data/raw/cropburn/jacketal_replication_package/Data/Analysis/dataset_for_analysis_plot_level.dta"
)

# Drop redundant ID columns (we keep blp_village_id as our village_id)
# and rename key variables to package-wide names:
#   R_prob  = continuous remote sensing predictor (probability of not burning)
#   R_bal   = binary predictor calibrated to balance false positive and false negative rates
#   R_max   = binary predictor calibrated to maximise accuracy
plots <- plots_raw %>%
  select(-village_id, -blp_plot_id, -sc_plot_id) %>%
  rename(
    village_id = blp_village_id,
    R_prob     = rs_p_not_burned_cont,
    R_bal      = rs_p_not_burned_bin_min,
    R_max      = rs_p_not_burned_bin_max
  )


# -----------------------------------------------------------------------------
# 3. Construct the treatment indicator (D)
#
# The experiment assigned villages to one of several PES (Payments for
# Ecosystem Services) arms. We treat any PES assignment as treatment = 1.
#   pesonly  = unconditional cash transfer arms (800 or 1600 Rs)
#   pesuct   = conditional cash transfer arms (800 Rs, 25% or 50% of savings)
#   pesany   = any PES arm  →  used as the binary treatment D
# -----------------------------------------------------------------------------

plots <- plots %>%
  mutate(
    pesonly = pes800 | pes1600,
    pesuct  = pes800uct25 | pes800uct50,
    pesany  = pesonly | pesuct,
    D       = as.numeric(pesany)
  )


# -----------------------------------------------------------------------------
# 4. Construct outcome variables (Y)
#
# We have three sources of ground truth for whether a plot was burned:
#   Y       = original label from Jack et al. remote sensing (rs_label)
#   Y_sc    = surveyors' in-person observation (sc = "spot check")
#   Y_mon   = monitor observations (up to 3 rounds; monp = "monitor plot")
#
# Y_recon (reconstructed) = 1 if *any* ground-truth source says not-burned,
# i.e. we take the conservative (minimum) across spot-check and monitor.
# All outcomes are coded 1 = not burned (treatment-consistent direction).
# -----------------------------------------------------------------------------

plots <- plots %>%
  mutate(
    # Aggregate up to three monitor rounds: burned if burned in any round
    monp_p_burnt_any = pmax(monp_pv_burnt_any1, monp_pv_burnt_any2, monp_pv_burnt_any3, na.rm = TRUE),

    # Convert "burned" flags to "not burned" outcomes
    Y_sc  = as.numeric(!sc_pv_burnt_any),
    Y_mon = as.numeric(!monp_p_burnt_any),
    Y     = as.numeric(!rs_label),

    # Reconstructed outcome: not burned only if both sources agree
    Y_recon = as.numeric(pmin(Y_sc, Y_mon, na.rm = TRUE))
  )


# -----------------------------------------------------------------------------
# 5. Construct sample indicators
#
# is_sc / is_mon flag which plots have each type of ground-truth observation.
# Sample membership (S) is defined separately in 03_data_splits.R, where it
# differs between the observational and validation study designs.
# -----------------------------------------------------------------------------

plots <- plots %>%
  mutate(
    is_sc    = as.numeric(!is.na(Y_sc)),
    is_mon   = as.numeric(!is.na(Y_mon)),
    district = haven::as_factor(district_fix, levels = "labels")
  )


# -----------------------------------------------------------------------------
# 6. Filter and select final columns
#
# Drop plots that are unusable: missing a village ID, or missing both the
# remote sensing predictor (R) and the reconstructed outcome (Y_recon).
# Then retain only the variables needed for downstream estimation.
# -----------------------------------------------------------------------------

plots <- plots %>%
  filter(
    !is.na(village_id),
    !(is.na(R_bal) & is.na(Y_recon)),
    !is.na(R_max), !is.na(R_bal), !is.na(R_prob)
  ) %>%
  select(
    unique_plot_id,
    village_id, district,
    R_bal, R_max, R_prob,
    Y, Y_recon,
    D,
    # Covariates used in some robustness specs
    rabovemed, baseline_complete, listing_not_complete, vill_added_back,
    is_sc, is_mon
  ) %>%
  mutate(across(
    c(D, Y, Y_recon, starts_with("R_"),
      baseline_complete, listing_not_complete, vill_added_back,
      rabovemed, is_sc, is_mon),
    as.numeric
  ))


# -----------------------------------------------------------------------------
# 7. Merge SHRUG village identifiers
#
# Attach pc11_tv_id from the crosswalk so each plot can be linked to its
# SHRUG village (needed to join satellite features in later scripts).
# We use a right join to keep all plots; plots without a SHRUG match will
# have pc11_tv_id = NA and are retained for the experimental sample.
# -----------------------------------------------------------------------------

plots <- right_join(shrug_mapping, plots, by = "village_id") %>%
  arrange(unique_plot_id) %>%
  select(-unique_plot_id)


# -----------------------------------------------------------------------------
# 8. Save cleaned plot-level dataset
# -----------------------------------------------------------------------------

output_path <- "data/clean/cropburn/cropburn_data.csv"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_csv(plots, output_path)
cat(sprintf("Saved cleaned plot data to: %s\n", output_path))

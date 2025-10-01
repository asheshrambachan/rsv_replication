# =============================================================================
# Table: Village-Level Descriptive Statistics 
# =============================================================================

rm(list = ls())

## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(kableExtra)
  library(tibble)
})
options(readr.show_col_types = F)


## Define mappings for labels
sample_levels <- c(
  "Holdout"   = "Observational",
  "Control"   = "Experimental: Untreated",
  "Buffer"    = "Experimental: Untreated",
  "Treatment" = "Experimental: Treated"
)
smartcard_levels <- c(
  "Holdout"   = "N/A",
  "Control"   = "2012",
  "Buffer"    = "2011",
  "Treatment" = "2010"
)


## Load data, select relevant columns
data <- read_csv(
  "data/poverty/processed/poverty_data.csv",
  col_select = c("shrid2", "wave", "tot_p", "tot_f", "urban")
  ) %>% 
  mutate(
    Sample = recode_factor(wave, !!!sample_levels),
    Smartcards = recode_factor(wave, !!!smartcard_levels)
  ) 

## Compute summary statistics (group by Sample x Smartcards)
tab <- data %>% 
  group_by(Sample, Smartcards) %>%
  summarise(
    `Number of villages`        = n_distinct(shrid2),
    `Average population`        = round(mean(tot_p), 0),
    `Average fraction female`   = round(mean(tot_f), 3),
    # `Average fraction urban`    = round(mean(urban), 3),
    .groups = "drop"
  ) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Metric")
colnames(tab) <- NULL


## Generate table using kableExtra
latex_tab <- kable(
    tab, 
    format    = "latex",
    booktabs  = TRUE,
    align     = "lcccc",
    escape    = FALSE,
    linesep   = ""
  ) 

## Patch table environment for custom placement and alignment
latex_tab <- sub(
  pattern = paste0(
    "\\\\begin\\{tabular\\}\\{lcccc\\}\n",
    "\\\\toprule\n",
    "Sample & Observational & Experimental: Untreated & Experimental: Untreated & Experimental: Treated\\\\\\\\\n",
    "Smartcards & N/A & 2012 & 2011 & 2010\\\\\\\\"
  ),
  replacement = paste0(
    "\\\\begin{tabular}{l|cccc}\n",
    "\\\\toprule\n",
    "Sample & Observational & Experimental: Untreated & Experimental: Untreated & Experimental: Treated\\\\\\\\\n",
    "Smartcards & N/A & 2012 & 2011 & 2010\\\\\\\\\n",
    "\\\\midrule"
  ),
  latex_tab
  )

print(latex_tab)
## Save table
output_path <- "outputs/poverty/poverty_summary_stats.tex"
save_kable(latex_tab, output_path)
cat(sprintf("Saved table to: %s\n", output_path))

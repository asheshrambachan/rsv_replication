# =============================================================================
# Table: Village-Level Descriptive Statistics
#
# Reports the number of villages, average population, and average fraction
# female by wave (treated, buffer, untreated, non-study).
#
# Input:  data/clean/smartcard/smartcard_data.csv
# Output: tables/smartcard/tab_summary_stats.tex
# =============================================================================

rm(list = ls())

## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})
options(readr.show_col_types = FALSE)

## Define mappings for labels
sample_levels <- c(
  "Observational (N/A)" = "Non-Study Mandals",
  "Experimental: Untreated (2012)" = "Untreated Mandals",
  "Experimental: Untreated (2011)" = "Buffer Mandals",
  "Experimental: Treated (2010)" = "Treated Mandals"
)

smartcard_levels <- c(
  "Observational (N/A)"   = "N/A",
  "Experimental: Untreated (2012)" = "2012",
  "Experimental: Untreated (2011)" = "2011",
  "Experimental: Treated (2010)" = "2010"
)

## Desired column order
wave_order <- c("Observational (N/A)",
                "Experimental: Untreated (2012)",
                "Experimental: Untreated (2011)",
                "Experimental: Treated (2010)"
                )

## Load data, select relevant columns
data <- read_csv(
  "data/clean/smartcard/smartcard_data.csv",
  col_select = c("shrid2", "wave", "tot_p", "tot_f")
) %>%
  mutate(
    wave = factor(wave, levels = wave_order),
    Sample = recode(as.character(wave), !!!sample_levels),
    Smartcards = recode(as.character(wave), !!!smartcard_levels)
  )

## Compute summary statistics by wave
tab <- data %>%
  group_by(wave, Sample, Smartcards) %>%
  summarise(
    `Number of villages`      = n_distinct(shrid2),
    `Average population`      = round(mean(tot_p, na.rm = TRUE), 0),
    `Average fraction female` = round(mean(tot_f, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(wave)

## Build rows manually (avoids knitr/xtable dependencies and gives exact control
## over the column order and LaTeX formatting)
sample_row <- c("Sample", tab$Sample)
smartcard_row <- c("Smartcards Rollout", tab$Smartcards)
villages_row <- c("Number of villages", tab$`Number of villages`)
pop_row <- c("Average population", tab$`Average population`)
female_row <- c("Average fraction female", tab$`Average fraction female`)

tex <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  paste(sample_row, collapse = " & ") |> paste0(" \\\\"),
  paste(smartcard_row, collapse = " & ") |> paste0(" \\\\"),
  "\\midrule ",
  paste(villages_row, collapse = " & ") |> paste0("\\\\"),
  paste(pop_row, collapse = " & ") |> paste0("\\\\"),
  paste(female_row, collapse = " & ") |> paste0("\\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)

## Save table
output_path <- "tables/smartcard/tab_summary_stats.tex"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
writeLines(tex, output_path)

cat(sprintf("Saved table to: %s\n", output_path))

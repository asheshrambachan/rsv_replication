## Balance/descriptive table about dimension of each shrid and 
## other observable characteristics 

library(dplyr)
library(readr)
library(magrittr)

rm(list = ls())
# 11-28-543-04817-802945
# TODO: this table is prior to dropping entries with missing y values
data <- read_csv(
  "data/clean/antipoverty/data.csv",
  col_select = c("shrid2", "wave", "clusters", "tot_p", "tot_f", "urban", "Y05k", "Y10k", "Ycons"),
  show_col_types = F
  ) %>%
  filter(
    # tot_p >= 100,
    # !(is.na(Y05k) | is.na(Y10k) | is.na(Ycons))
    ) # Identify and remove small shrid based on population size i.e filter out all shrids with pop size of 100 or lower

tab <- data %>% # there are 28 shrids that should be dropped?
  mutate(
    Sample = factor(
      wave, 
      levels = c("Holdout", "Control", "Buffer", "Treatment"),
      labels = c("Observational", "Experimental: Untreated", "Experimental: Untreated", "Experimental: Treated")
    ),
    Smartcards = factor(
      wave, 
      levels = c("Holdout", "Control", "Buffer", "Treatment"),
      labels = c("N/A", "2012", "2011", "2010")
    )
    ) %>%
  group_by(Sample, Smartcards) %>%
  summarise(
    "Number of villages" = n(),
    "Average population" = round(mean(tot_p)),
    "Average fraction female" = round(mean(tot_f),3),
    "Average fraction urban" = round(mean(urban),3),
    .groups = "drop"
  ) %>%
  t(.) 

library(kableExtra)
latex_tab <- kable(tab, align = "c", format = "latex", linesep="", booktabs=T, 
      label = "summaries", 
      caption = "Village summary statistics."
      ) %>%
  row_spec(2, hline_after = TRUE, extra_latex_after = "%")

# Save LaTeX table
output_path <- "output/tables/antipoverty_village_summary_stat.tex"
save_kable(latex_tab, output_path)
cat(sprintf("Saved LaTeX table to: %s\n", output_path))
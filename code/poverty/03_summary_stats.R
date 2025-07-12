## Balance/descriptive table about dimension of each shrid and 
## other observable characteristics 


rm(list = ls())

library(dplyr)
library(readr)
library(kableExtra)

sample_levels <- c(
  "Holdout" = "Observational",
  "Control" = "Experimental: Untreated",
  "Buffer" = "Experimental: Untreated",
  "Treatment" = "Experimental: Treated"
)
smartcard_levels <- c(
  "Holdout" = "N/A",
  "Control" = "2012",
  "Buffer" = "2011",
  "Treatment" = "2010"
)

data <- read_csv(
  "data/processed/poverty/data.csv",
  col_select = c("shrid2", "wave", "tot_p", "tot_f", "urban"),
  show_col_types = F
  ) %>% 
  mutate(
    Sample = recode_factor(wave, !!!sample_levels),
    Smartcards = recode_factor(wave, !!!smartcard_levels)
  ) %>%
  arrange(desc(urban)) %>%  # So that Urban==1 comes first, # Note: there are 15 shrids that appear as both urban and rural. 
  distinct(shrid2, .keep_all = T) 

tab <- data %>% 
  group_by(Sample, Smartcards) %>%
  summarise(
    "Number of villages" = n(),
    "Average population" = round(mean(tot_p)),
    "Average fraction female" = round(mean(tot_f),3),
    "Average fraction urban" = round(mean(urban),3),
    .groups = "drop"
  ) %>%
  t()

latex_tab <- kable(tab, align = "c", format = "latex", linesep="", booktabs=T, 
      label = "summaries", 
      caption = "Village summary statistics."
      ) %>%
  row_spec(2, hline_after = TRUE, extra_latex_after = "%")

# Save LaTeX table
output_path <- "output/tables/poverty_summary_stats.tex"
save_kable(latex_tab, output_path)
cat(sprintf("Saved LaTeX table to: %s\n", output_path))
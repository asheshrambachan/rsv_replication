## Balance/descriptive table about dimension of each shrid and 
## other observable characteristics 


rm(list = ls())

library(dplyr)
library(readr)
library(kableExtra)

data <- read_csv(
  "data/processed/poverty/data.csv",
  col_select = c("shrid2", "wave", "clusters", "tot_p", "tot_f", "urban"),
  show_col_types = F
  )

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

latex_tab <- kable(tab, align = "c", format = "latex", linesep="", booktabs=T, 
      label = "summaries", 
      caption = "Village summary statistics."
      ) %>%
  row_spec(2, hline_after = TRUE, extra_latex_after = "%")

# Save LaTeX table
output_path <- "output/tables/poverty_summary_stats.tex"
save_kable(latex_tab, output_path)
cat(sprintf("Saved LaTeX table to: %s\n", output_path))
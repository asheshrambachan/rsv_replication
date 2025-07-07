## Balance/descriptive table about dimension of each shrid and 
## other observable characteristics 

library(stargazer)
library(dplyr)
library(magrittr)

rm(list = ls())

# TODO: this table is prior to dropping entries with missing y values
data <- read.csv("data/clean/antipoverty/data_wo_features.csv") %>%
  select(c("shrid2", "wave", "clusters", "tot_p", "tot_f", "urban")) %>%
  filter(tot_p >= 100) # Identify and remove small shrid based on population size i.e filter out all shrids with pop size of 100 or lower

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
kable(tab, align = "c", format = "latex", linesep="", booktabs=T, 
      label = "summaries", 
      caption = "Village summary statistics."
      ) %>%
  row_spec(2, hline_after = TRUE, extra_latex_after = "%") 

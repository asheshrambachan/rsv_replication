## Balance/descriptive table about dimension of each shrid and 
## other observable characteristics 

library(stargazer)
library(dplyr)
library(magrittr)

rm(list = ls())

# TODO: this table is prior to dropping entries with missing y values
data <- read.csv("data/clean/antipoverty/data_wo_features.csv") %>%
  select(c("shrid2", "wave", "clusters", "tot_p", "tot_m", "tot_f", "urban")) %>%
  filter(tot_p >= 100) # Identify and remove small shrid based on population size i.e filter out all shrids with pop size of 100 or lower

tab_balance <- data %>% # there are 28 shrids that should be dropped?
  mutate(wave = factor(wave, levels = c("Holdout", "Control", "Buffer","Treatment"))) %>%
  group_by(wave) %>%
  summarise(
    n_shrids = n(),
    mean_pop = mean(tot_p),
    mean_pop_f = mean(tot_f),
    mean_urban = mean(urban),
  ) %>%
  data.frame(row.names = "wave") %>%
  mutate(
    across(everything(), ~ round(., 3)),
    across(mean_pop, ~ round(., 0)),
  ) %>%
  t(.) %>%
  set_rownames(c(
    "Number of shrids", "Average pop", "Average female pop", "Urban area"))

stargazer(
  tab_balance,
  type="latex",
  title = "Summary statistics about villages (i.e., shrids) based on their treatment status.",
  out = "output/tables/antipoverty_village_summary.tex"
)

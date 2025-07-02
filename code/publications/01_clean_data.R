library(here)
library(dplyr)
library(readxl)

# Function to safely read and select from a specific sheet
read_lit_sheet <- function(sheet_name, filter_rsv = FALSE) {
  
  # Define shared columns to keep
  selected_columns <- c(
    "Year", "Journal", "Link", "Search Term", "Title", "Author", "Notes", 
    "what is RSV?", "is RCT?", "is observational?", "by economists?", 
    "independent var", "dependent var", "how RSV is used", 
    "what is being sensed?", "what is used to sense?"
  )
  
  df <- read_xlsx(here("data/raw/literature_review.xlsx"), sheet = sheet_name) 
  
  if (filter_rsv && all(c("Is it an RSV?", "Published?") %in% names(df))) {
    df <- df %>%
      filter(`Is it an RSV?` == "Y", `Published?` == "Y")
  }
  
  df <- df %>% select(any_of(selected_columns))
  
  return(df)
}


# Read and combine data
top_econ <- read_lit_sheet("most_journals", filter_rsv = TRUE)
nature <- read_lit_sheet("nature")
science <- read_lit_sheet("science")

# Combine all sheets
combined_data <- bind_rows(top_econ, science, nature)

# Clean and transform
cleaned_data <- combined_data %>%
  rename(
    year = Year,
    exptype_field = `is RCT?`,
    exptype_observational = `is observational?`,
    by_economists = `by economists?`,
    independent_var = `independent var`,
    dependent_var = `dependent var`,
    RSV_role = `how RSV is used`,
    what_sensed = `what is being sensed?`,
    what_type_RSV = `what is used to sense?`
  ) %>%
  mutate(
    satellite = grepl("satellite", what_type_RSV, ignore.case = TRUE),
    by_economists = by_economists == "Y",
    RCT = exptype_field == "Y" | exptype_observational == "Y"
  )

# Export cleaned data
write.csv(cleaned_data, here("data/clean/publication_data.csv"), row.names = FALSE)


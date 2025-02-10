setwd("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Pipeline_1-Exposure")

# Load necessary libraries
library(tidyverse)
library(readxl)
library(visdat)
library(plotly)
library(dlookr)
library(mice)
library(VIM)

# Load dataset and filter for relevant sectors
empl_pers <- readxl::read_xlsx("Outputs/Data/EMPL_persons_raw_data.xlsx")

View(empl_pers) # Some NUTS_ID have NA sectors:

library(dplyr)

# Identify NUTS_IDs that have complete sector information
sector_reference <- empl_pers %>%
  filter(!is.na(Sector)) %>%
  select(NUTS_ID, Sector) %>%
  distinct()

# Identify NUTS_IDs with missing Sector
nuts_with_na <- empl_pers %>%
  filter(is.na(Sector)) %>%
  select(NUTS_ID) %>%
  distinct()

# Get a reference NUTS_ID with a complete sector list
reference_nuts_id <- sector_reference %>% 
  pull(NUTS_ID) %>% 
  unique() %>% 
  first()  # Taking the first valid one

# Get the sectors corresponding to the reference NUTS_ID
reference_sectors <- sector_reference %>%
  filter(NUTS_ID == reference_nuts_id) %>%
  pull(Sector)

# Expand and fill missing sectors for the NUTS_IDs that had NA
filled_sectors <- nuts_with_na %>%
  mutate(Sector = list(reference_sectors)) %>%
  unnest(Sector)

# Merge back into the original dataset
empl_pers <- empl_pers %>%
  filter(!is.na(Sector)) %>%
  bind_rows(filled_sectors)

# View the updated dataset
View(empl_pers)


# Inspect the structure of the data
unique(empl_pers$NUTS_ID)

# Transform the data to wide format
empl_pers_wide <- empl_pers %>%
  select(NUTS_ID, Time, Sector, Empl_pers) %>%
  pivot_wider(
    names_from = Sector,      # Pivot the Sector column
    values_from = Empl_pers   # Use Empl_pers as values
  )

# Inspect the wide-format data
glimpse(empl_pers_wide)

# Summary of missing values per column
missing_summary <- empl_pers_wide %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "missing_{col}"))

# Proportion of missing values
missing_percentage <- empl_pers_wide %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100, .names = "pct_missing_{col}"))

# View missingness summary
missing_summary
missing_percentage

# Prepare data for imputation
data_for_imp <- empl_pers_wide %>%
  select(-NUTS_ID, -Time)  # Remove non-numeric columns for imputation

# Impute missing values with mice
imputed <- mice(
  data_for_imp,
  method = "pmm",  # Predictive Mean Matching
  m = 5,           # Number of imputations
  maxit = 50,      # Maximum iterations
  seed = 123       # For reproducibility
)

# Extract the first completed dataset
empl_pers_wide_imputed <- complete(imputed, "long") %>%
  group_by(.id) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

# Add back the NUTS_ID and Time columns
empl_pers_wide_final <- bind_cols(
  empl_pers_wide %>% select(NUTS_ID, Time),
  empl_pers_wide_imputed
)

# Inspect the imputed dataset
glimpse(empl_pers_wide_final)
sum(is.na(empl_pers_wide_final))
colSums(is.na(empl_pers_wide_final))

View(empl_pers_wide_final)

# Compare before and after for a sample sector (e.g., C10)
# Summary statistics function
summarize_data <- function(data) {
  data %>%
    summarise(across(
      everything(),
      list(
        mean = ~ mean(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        pct_missing = ~ mean(is.na(.)) * 100
      ),
      .names = "{col}_{fn}"
    ))
}

# Summarize before and after imputation
pre_imputation_summary <- summarize_data(empl_pers_wide %>% select(-NUTS_ID, -Time))
post_imputation_summary <- summarize_data(empl_pers_wide_final %>% select(-NUTS_ID, -Time))

# Combine summaries for comparison
comparison <- bind_rows(
  pre_imputation_summary %>% mutate(stage = "Pre-Imputation"),
  post_imputation_summary %>% mutate(stage = "Post-Imputation")
)

# Pivot for easier comparison
comparison_long <- comparison %>%
  pivot_longer(cols = -stage, names_to = c("Variable", "Metric"), names_sep = "_") %>%
  pivot_wider(names_from = stage, values_from = value)

# View the comparison
options(scipen = 999)
head(comparison_long)

empl_pers_final <- empl_pers_wide_final %>%
  pivot_longer(
    cols = setdiff(names(empl_pers_wide_final), c("NUTS_ID", "Time", ".id")),  
    names_to = "Sector",       
    values_to = "Empl_pers"
  ) %>%
  filter(Sector != ".id") 

EMPL_persons_imputed_data <- empl_pers_final |> 
  select(-Time, -.id)

# Save the long-format data
writexl::write_xlsx(EMPL_persons_imputed_data,
             "Outputs/Data/EMPL_persons_imputed_data.xlsx")

return("Outputs/Data/EMPL_persons_imputed_data.xlsx")
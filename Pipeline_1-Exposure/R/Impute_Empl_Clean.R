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

# Convert the wide-format data back to long format
empl_pers_final <- empl_pers_wide_final %>%
  pivot_longer(
    cols = -c(NUTS_ID, Time), # Keep these columns as is
    names_to = "Sector",      # Column name for the sector variable
    values_to = "Empl_pers"   # Column name for the employment values
  )

EMPL_persons_imputed_data <- empl_pers_final

# return(EMPL_persons_imputed_data)

# Save the long-format data
writexl::write_xlsx(EMPL_persons_imputed_data,
             "Outputs/Data/EMPL_persons_imputed_data.xlsx")

return("Outputs/Data/EMPL_persons_imputed_data.xlsx")
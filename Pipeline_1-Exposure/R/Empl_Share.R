# Load necessary libraries
library(readxl)
library(dplyr)

# Load the dataset
EMPL_persons_imputed <- readxl::read_xlsx("Outputs/Data/EMPL_persons_imputed_data.xlsx")

# Extract the country code from the NUTS_ID (first two characters)
EMPL_persons_imputed <- EMPL_persons_imputed %>%
  mutate(Country = substr(NUTS_ID, 1, 2))

# Separate country-level and regional-level data
country_data <- EMPL_persons_imputed %>% filter(nchar(NUTS_ID) == 2)
regional_data <- EMPL_persons_imputed %>% filter(nchar(NUTS_ID) > 2)

# Calculate national totals for each sector (or subsector) in each country
national_totals <- country_data %>%
  group_by(Country, Sector) %>%
  summarise(National_Empl = sum(Empl_pers, na.rm = TRUE), .groups = "drop")

# Join national totals back to the regional data
regional_with_shares <- regional_data %>%
  left_join(national_totals, by = c("Country", "Sector")) %>%
  mutate(
    Regional_Share = ifelse(
      National_Empl > 0,  # Check if the national total is greater than 0
      Empl_pers / National_Empl, # Calculate share of employment in region relative to national total
      0  # Assign 0 if national total is 0 to avoid division by zero
    )
  )

unique(regional_with_shares$Sector)

# Verify that shares sum to 1 for each country-sector combination
verification <- regional_with_shares %>%
  group_by(Country, Sector) %>%
  summarise(Total_Share = sum(Regional_Share, na.rm = TRUE), .groups = "drop")

print("Verification of shares summing to 1:")
print(verification)

### RECALCULATE WITH AGGREGATED SECTORS
# Step 1: Create a mapping table for aggregation
sector_mapping <- data.frame(
  Original_Sector = c("C", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31", "C32", "C33"),
  Aggregated_Sector = c("C", "C10-C12", "C10-C12", "C10-C12", "C13-C15", "C13-C15", "C13-C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31-C32", "C31-C32", "C33")
)

# Step 2: Map the original sectors to the aggregated sectors
regional_with_shares <- regional_with_shares %>%
  left_join(sector_mapping, by = c("Sector" = "Original_Sector"))

# Step 3: Aggregate employment data by `Country`, `NUTS_ID`, and `Aggregated_Sector`
aggregated_data <- regional_with_shares %>%
  group_by(Country, NUTS_ID, Aggregated_Sector) %>%
  summarise(Aggregated_Empl = sum(Empl_pers, na.rm = TRUE), .groups = "drop")

# Step 4: Calculate national totals for each aggregated sector
national_totals_aggregated <- aggregated_data %>%
  group_by(Country, Aggregated_Sector) %>%
  summarise(National_Empl = sum(Aggregated_Empl, na.rm = TRUE), .groups = "drop")

# Step 5: Join the national totals back to the regional data and recalculate shares
regional_with_aggregated_shares <- aggregated_data %>%
  left_join(national_totals_aggregated, by = c("Country", "Aggregated_Sector")) %>%
  mutate(
    Regional_Share = ifelse(
      National_Empl > 0, 
      Aggregated_Empl / National_Empl, 
      0
    )
  )

# Step 6: Verify shares sum to 1 for each country and aggregated sector
verification_aggregated <- regional_with_aggregated_shares %>%
  group_by(Country, Aggregated_Sector) %>%
  summarise(Total_Share = sum(Regional_Share, na.rm = TRUE), .groups = "drop")

print("Verification of aggregated shares summing to 1:")
print(verification_aggregated)

### Add High-tech aggregation by NACE Rev.2 

# Step 7: Define a mapping for technological intensity based on NACE Rev.2
tech_mapping <- data.frame(
  Aggregated_Sector = c("C21", "C26", "C20", "C27", "C28", "C29", "C30", "C19", "C22", "C23", "C24", "C25", "C33", "C10-C12", "C13-C15", "C16", "C17", "C18", "C31-C32"),
  Tech_Grade = c("High-tech", "High-tech", "Medium-high-tech", "Medium-high-tech", "Medium-high-tech", "Medium-high-tech", "Medium-high-tech", 
                 "Medium-low-tech", "Medium-low-tech", "Medium-low-tech", "Medium-low-tech", "Medium-low-tech", "Medium-low-tech", 
                 "Low-tech", "Low-tech", "Low-tech", "Low-tech", "Low-tech", "Low-tech")
)

# Step 8: Join the technological intensity information to the regional data
regional_with_aggregated_shares <- regional_with_aggregated_shares %>%
  left_join(tech_mapping, by = "Aggregated_Sector")

regional_with_aggregated_shares <- regional_with_aggregated_shares %>%
  rename(Sector = Aggregated_Sector)

EMPL_shares_data <- regional_with_aggregated_shares
# return(EMPL_shares_data)

# Step 9: Save the updated dataset with the technology grade included
writexl::write_xlsx(EMPL_shares_data, 
             "Outputs/Data/EMPL_shares_data.xlsx")

return("Outputs/Data/EMPL_shares_data.xlsx")

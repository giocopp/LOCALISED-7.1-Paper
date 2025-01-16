setwd("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Pipeline_1-Exposure")

### INSTALL PACKAGES

remotes::install_github(
  "eurostat/restatapi"
)

libs <- c(
  "restatapi",
  "tidyverse",
  "giscoR",
  "sf",
  "classInt"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libs == FALSE)) {
  install.packages(
    libs[!installed_libs],
    dependencies = TRUE
  )
}

invisible(
  lapply(
    libs, library,
    character.only = TRUE
  )
)

### GET DATA
### 

indicator_df <- restatapi::get_eurostat_data(
  id = "env_ac_ainah_r2",
  filters = c("GHG", "THS_T"),
  date_filter = c(2022),
  exact_match = F,
  label = F,
  cflags = T,
  keep_flags = T
)

indicator_df_f <- indicator_df |> 
  dplyr::rename(
    "NUTS_ID" = "geo",
    "Emissions" = "values",
    "Sector" = "nace_r2",
    "Time" = "time",
    "Flags" = "flags"
  ) 

indicator_df_f <- indicator_df_f |> 
  dplyr::filter(substr(Sector, 1, 1) == "C") |> 
  dplyr::filter(!str_detect(NUTS_ID, "ZZ"))  

base_data <- readxl::read_excel("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Base Data/base_data.xlsx") |> 
  dplyr::select(1, 3)

indicator_df_f <- base_data |> 
  dplyr::left_join(
    indicator_df_f,
    by = "NUTS_ID"
  )

indicator_df_f <- indicator_df_f |> 
  dplyr::select(c(NUTS_ID, dplyr::everything()))

Emissions_raw_data <- indicator_df_f |>
  mutate(Sector = str_replace(Sector, "C31_C32", "C31-C32")) |> 
  dplyr::filter(!str_detect(NUTS_ID, "ZZ"))

unique(Emissions_raw_data$Sector)

### RECALCULATE WITH AGGREGATED SECTORS
# Step 1: Create a mapping table for aggregation
sector_mapping <- data.frame(
  Original_Sector = c("C", "C10-C12", "C13-C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31-C32", "C33"),
  Aggregated_Sector = c("C", "C10-C12", "C13-C15", "C16-C18", "C16-C18", "C16-C18", "C19-C22", "C19-C22", "C19-C22", "C19-C22", "C23", "C24", "C25+C28-C30", "C26-C27", "C26-C27", "C25+C28-C30", "C25+C28-C30", "C25+C28-C30", "C31-C32", "C33")
)

# Step 2: Map the original sectors to the aggregated sectors
Emissions_raw_data <- Emissions_raw_data %>%
  left_join(sector_mapping, by = c("Sector" = "Original_Sector"))

# Step 3: Aggregate employment data by `Country`, `NUTS_ID`, and `Aggregated_Sector`
aggregated_data <- Emissions_raw_data %>%
  group_by(NUTS_ID, Aggregated_Sector) %>%
  summarise(Aggregated_Emissions = sum(Emissions, na.rm = TRUE), .groups = "drop")

Emissions_raw_data <- aggregated_data %>%
  rename(Sector = Aggregated_Sector, Emissions = Aggregated_Emissions)

### DOWNSCALE EMSSIONS

# Load the datasets
shares <- readxl::read_excel("Outputs/Data/EMPL_shares_data.xlsx") |>
  filter(nchar(NUTS_ID) != 3) |>
  dplyr::filter(!str_detect(NUTS_ID, "ZZ"))

# Ensure data types match for merging
emissions <- Emissions_raw_data %>%
  mutate(NUTS_ID = as.character(NUTS_ID),
         Sector = as.character(Sector))

shares <- shares %>%
  mutate(NUTS_ID = as.character(NUTS_ID),
         Sector = as.character(Sector))

# Filter emissions to keep only national-level data
national_emissions <- emissions %>% filter(nchar(NUTS_ID) == 2)
View(national_emissions)

# Merge national emissions with regional shares
regional_emissions <- national_emissions %>%
  left_join(shares, by = c("NUTS_ID" = "Country", "Sector" = "Sector")) %>%
  mutate(Regional_Emissions = Emissions * Regional_Share) %>%
  select(NUTS_ID = NUTS_ID.y, Sector, Emissions, Regional_Share, Regional_Emissions)

# Normalize emissions for Sector "C" (total manufacturing)
# Define a function to normalize the emissions
normalize_emissions <- function(data) {
  
  # Identify which rows are total manufacturing (C) and which are subsectors (Cxx, etc.)
  is_total_sector <- data$Sector == "C"
  is_subsector <- startsWith(data$Sector, "C") & data$Sector != "C"
  
  # Initialize the new column
  data$Normalized_Emissions <- NA
  
  # Extract values for total sector C
  total_values <- data$Regional_Emissions[is_total_sector]
  
  # Extract values for subsectors
  subsector_values <- data$Regional_Emissions[is_subsector]
  
  # Compute min and max for total sector C
  min_C <- min(total_values, na.rm = TRUE)
  max_C <- max(total_values, na.rm = TRUE)
  
  # Compute min and max for all subsector values combined
  min_sub <- min(subsector_values, na.rm = TRUE)
  max_sub <- max(subsector_values, na.rm = TRUE)
  
  # Normalize total sector C values
  data$Normalized_Emissions[is_total_sector] <- (data$Regional_Emissions[is_total_sector] - min_C) /
    (max_C - min_C)
  
  # Normalize subsector values across all subsectors
  data$Normalized_Emissions[is_subsector] <- (data$Regional_Emissions[is_subsector] - min_sub) /
    (max_sub - min_sub)
  
  return(data)
}

# Example usage:
regional_emissions <- normalize_emissions(regional_emissions)

Regional_emissions_data_final <- regional_emissions |> 
  dplyr::select(NUTS_ID, Sector, Regional_Emissions, Normalized_Emissions) |> 
  rename(GHG_Emissions = Regional_Emissions,
         Exposure_Index = Normalized_Emissions)

Regional_emissions_data <- Regional_emissions_data_final |> 
  dplyr::select(NUTS_ID, Sector, GHG_Emissions)

Regional_emissions_index <- Regional_emissions_data_final |> 
  dplyr::select(NUTS_ID, Sector, Exposure_Index)

# return(Regional_emissions_data_final)

# Save the updated dataset
writexl::write_xlsx(
  Regional_emissions_data,
  "Outputs/Data/EXP_Data_raw.xlsx")

writexl::write_xlsx(
  Regional_emissions_index,
  "Outputs/Data/EXP_Data_index.xlsx")

return("Outputs/Data/EXP_Data_raw.xlsx", "Outputs/Data/EXP_Data_index.xlsx")



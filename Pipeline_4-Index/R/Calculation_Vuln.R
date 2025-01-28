setwd("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index")

remotes::install_github("eurostat/restatapi")

libs <- c(
  "restatapi",
  "tidyverse",
  "giscoR",
  "sf",
  "classInt",
  "mice",
  "visdat",
  "VIM", 
  "readxl", 
  "writexl"
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

Index_s <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Pipeline_2-Vulnerability/Weighting and Aggregation/Outputs/Data/Index_Data_Select.xlsx") |> 
  select(NUTS_ID, Sector_ID, everything())

sector_name_map <- c(
  "C"            = "Total Manufacturing",
  "C10-C12"      = "Food, Beverage and Tobacco",
  "C13-C15"      = "Textiles, Leather and Wearing",
  "C16-C18"      = "Wood, Paper and Printing",
  "C19-C22"      = "Chemical, Petrolchemical, Pharmaceutical and Plastic",
  "C23"          = "Cement, Ceramics, Glass, and Lime",
  "C24"          = "Basic Metals",
  "C26-C27"      = "Electronics and Electrical Equipment",
  "C25+C28-C30"  = "Fabricated Metals, Machinery, Vehicles and Transport Equipment"
)

base_data <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Base Data/base_data_plus.xlsx") |> 
  select(CNTR_CODE, NUTS_ID, NUTS_NAME) |> 
  filter(nchar(NUTS_ID) > 2)

NZBC_Index <- Index_s |> 
  left_join(base_data, by = "NUTS_ID") |>
  rename(Region_Name = NUTS_NAME,
         Country = CNTR_CODE) |> 
  mutate(Sector_Name = recode(Sector_ID, !!!sector_name_map)) |> 
  select(NUTS_ID, Country, Region_Name, Sector_ID, Sector_Name, Exposure_Index, Energy_Index, Labor_Index, Sup_Ch_Index, Tech_Index, Finance_Index, Inst_Index)

write_xlsx(NZBC_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/NZBC_Index_Data.xlsx")
write_csv(NZBC_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/NZBC_Index_Data.csv")

### Vulnerability Index calculation
###
columns <- c("Energy_Index", "Labor_Index", "Sup_Ch_Index", "Tech_Index", "Finance_Index", "Inst_Index")
weights <- c(1, 1, 1, 1, 1, 1)  # Example weights: Energy_Index is more important
weights <- weights / sum(weights)

Vuln_Index <- NZBC_Index |> 
  rowwise() |>  # Process row by row
  mutate(
    Vulnerability_Index = sum(
      c(Energy_Index, Labor_Index, Sup_Ch_Index, Tech_Index, Finance_Index, Inst_Index) * weights,
      na.rm = TRUE
    ) / 
      sum(weights[!is.na(c(Energy_Index, Labor_Index, Sup_Ch_Index, Tech_Index, Finance_Index, Inst_Index))])  # Adjust weights for non-NA values
  ) |> 
  ungroup()

Vuln_Index <- Vuln_Index |> 
  mutate(
    Vulnerability_Index = case_when(
      # Normalize sector "C" separately
      Sector_ID == "C" ~ 0.01 + 
        (Vulnerability_Index - min(Vulnerability_Index[Sector_ID == "C"], na.rm = TRUE)) / 
        (max(Vulnerability_Index[Sector_ID == "C"], na.rm = TRUE) - 
           min(Vulnerability_Index[Sector_ID == "C"], na.rm = TRUE)) * (0.99 - 0.01),
      
      # Normalize other sectors
      TRUE ~ 0.01 + 
        (Vulnerability_Index - min(Vulnerability_Index[Sector_ID != "C"], na.rm = TRUE)) / 
        (max(Vulnerability_Index[Sector_ID != "C"], na.rm = TRUE) - 
           min(Vulnerability_Index[Sector_ID != "C"], na.rm = TRUE)) * (0.99 - 0.01)
    )
  )

write_xlsx(Vuln_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/Vuln_Index_Data.xlsx")
write_csv(Vuln_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/Vuln_Index_Data.csv")

### Risk Index calculation
### 

# Define weights (must match the columns ending with "Index")
# Define columns and weights
columns <- c("Exposure_Index", "Vulnerability_Index")
weights <- c(4, 3)  # Example weights: Energy_Index is more important
weights <- weights / sum(weights)  # Normalize weights to sum to 1

# Calculate the weighted geometric mean
# Define columns and weights
columns <- c("Exposure_Index", "Vulnerability_Index")  # Columns to include
weights <- c(4, 3)  # Example weights: Exposure is more important than Vulnerability
weights <- weights / sum(weights)  # Normalize weights to sum to 1

# Calculate the RISK
# Calculate weighted arithmetic mean
Risk_Index <- Vuln_Index |>
  rowwise() |>
  mutate(
    Risk_Index = Exposure_Index * weights[1] + Vulnerability_Index * weights[2]
  ) |>
  ungroup()

Risk_Index <- Risk_Index |> 
  mutate(
    Risk_Index = case_when(
      # Normalize sector "C" separately
      Sector_ID == "C" ~ 0.01 + 
        (Risk_Index - min(Risk_Index[Sector_ID == "C"], na.rm = TRUE)) / 
        (max(Risk_Index[Sector_ID == "C"], na.rm = TRUE) - 
           min(Risk_Index[Sector_ID == "C"], na.rm = TRUE)) * (0.99 - 0.01),
      
      # Normalize other sectors
      TRUE ~ 0.01 + 
        (Risk_Index - min(Risk_Index[Sector_ID != "C"], na.rm = TRUE)) / 
        (max(Risk_Index[Sector_ID != "C"], na.rm = TRUE) - 
           min(Risk_Index[Sector_ID != "C"], na.rm = TRUE)) * (0.99 - 0.01)
    )
  )

write_xlsx(Risk_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/Risk_Index_Data.xlsx")
write_csv(Risk_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/Risk_Index_Data.csv")

return("Outputs/Data/Vuln_Index_Data.xlsx")
return("Outputs/Data/Vuln_Index_Data.csv")
return("Outputs/Data/Risk_Index_Data.xlsx")
return("Outputs/Data/Risk_Index_Data.csv")


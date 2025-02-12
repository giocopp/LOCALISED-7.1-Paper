## Set working directory and install/load required packages
setwd("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index")

# Install remotes and the required packages if missing
if (!"remotes" %in% rownames(installed.packages())) install.packages("remotes")
remotes::install_github("eurostat/restatapi")

libs <- c("restatapi", "tidyverse", "giscoR", "sf", "classInt",
          "mice", "visdat", "VIM", "readxl", "writexl")
not_installed <- !(libs %in% rownames(installed.packages()))
if (any(not_installed)) install.packages(libs[not_installed], dependencies = TRUE)
invisible(lapply(libs, library, character.only = TRUE))


## Read and join input data
Index_s <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Pipeline_2-Vulnerability/Weighting and Aggregation/Outputs/Data/Index_Data_Select.xlsx") %>% 
  select(NUTS_ID, Sector_ID, everything())

sector_name_map <- c(
  "C"           = "Total Manufacturing",
  "C10-C12"     = "Food, Beverage and Tobacco",
  "C13-C15"     = "Textiles, Leather and Wearing",
  "C16-C18"     = "Wood, Paper and Printing",
  "C19-C20"     = "Chemical and Petrolchemical",
  "C21-C22"     = "Pharmaceutical and Plastic",
  "C23"         = "Cement, Ceramics, Glass, and Lime",
  "C24"         = "Basic Metals",
  "C26-C27"     = "Electronics and Electrical Equipment",
  "C25+C28-C30" = "Fabricated Metals, Machinery, Vehicles and Transport Equipment"
)

base_data <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Base Data/base_data_plus.xlsx") %>% 
  select(CNTR_CODE, NUTS_ID, NUTS_NAME) %>% 
  filter(nchar(NUTS_ID) > 2)

NZBC_Index <- Index_s %>% 
  left_join(base_data, by = "NUTS_ID") %>%
  rename(Region_Name = NUTS_NAME, Country = CNTR_CODE) %>% 
  mutate(Sector_Name = recode(Sector_ID, !!!sector_name_map)) %>% 
  select(Country, NUTS_ID, Region_Name, Sector_ID, Sector_Name, 
         Exposure_Index, Energy_Index, Labor_Index, Sup_Ch_Index, Tech_Index, Finance_Index, Inst_Index)

# Write the joined index data
write_xlsx(NZBC_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/NZBC_Index_Data.xlsx")
write_csv(NZBC_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/NZBC_Index_Data.csv")


## Vulnerability Index Calculation
# Define columns and weights for vulnerability calculation
vuln_cols <- c("Energy_Index", "Labor_Index", "Sup_Ch_Index", "Tech_Index", "Finance_Index", "Inst_Index")
vuln_weights <- rep(1, length(vuln_cols))
vuln_weights <- vuln_weights / sum(vuln_weights)

Vuln_Index <- NZBC_Index %>%
  rowwise() %>%
  mutate(
    # Linear (arithmetic) aggregation: weighted average (adjusted for NA values)
    Vulnerability_Index_linear = {
      vals <- c_across(all_of(vuln_cols))
      valid <- !is.na(vals)
      if (sum(valid) == 0) NA_real_ else sum(vals[valid] * vuln_weights[valid]) / sum(vuln_weights[valid])
    },
    # Geometric aggregation: weighted geometric mean (only if all valid values are > 0)
    Vulnerability_Index_geo = {
      vals <- c_across(all_of(vuln_cols))
      valid <- !is.na(vals) & (vals > 0)
      if (sum(valid) == 0) NA_real_ else exp(sum(vuln_weights[valid] * log(vals[valid])) / sum(vuln_weights[valid]))
    }
  ) %>% 
  ungroup()

# Normalize the vulnerability indices separately for sector "C" and others
Vuln_Index <- Vuln_Index %>% 
  mutate(sector_group = if_else(Sector_ID == "C", "C", "other")) %>%
  group_by(sector_group) %>%
  mutate(
    Vulnerability_Index_linear = 0.01 + (Vulnerability_Index_linear - min(Vulnerability_Index_linear, na.rm = TRUE)) /
      (max(Vulnerability_Index_linear, na.rm = TRUE) - min(Vulnerability_Index_linear, na.rm = TRUE)) * (0.99 - 0.01),
    Vulnerability_Index_geo = 0.01 + (Vulnerability_Index_geo - min(Vulnerability_Index_geo, na.rm = TRUE)) /
      (max(Vulnerability_Index_geo, na.rm = TRUE) - min(Vulnerability_Index_geo, na.rm = TRUE)) * (0.99 - 0.01)
  ) %>%
  ungroup() %>%
  select(-sector_group)

# Write the vulnerability index outputs
write_xlsx(Vuln_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/Vuln_Index_Data.xlsx")
write_csv(Vuln_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/Vuln_Index_Data.csv")


## Risk Index Calculation
# Define weights for risk aggregation (Exposure and Vulnerability)
risk_weights <- c(1, 1)
risk_weights <- risk_weights / sum(risk_weights)

Risk_Index <- Vuln_Index %>%
  rowwise() %>%
  mutate(
    # Linear aggregation using Exposure_Index and Vulnerability_Index_linear
    Risk_Index_linear = Exposure_Index * risk_weights[1] + Vulnerability_Index_linear * risk_weights[2],
    # Geometric aggregation using Exposure_Index and Vulnerability_Index_geo
    Risk_Index_geo = if_else(Exposure_Index > 0 & Vulnerability_Index_geo > 0,
                             exp(risk_weights[1] * log(Exposure_Index) + risk_weights[2] * log(Vulnerability_Index_geo)),
                             NA_real_)
  ) %>%
  ungroup()

# Normalize the risk indices by sector group ("C" vs others)
Risk_Index <- Risk_Index %>% 
  mutate(sector_group = if_else(Sector_ID == "C", "C", "other")) %>%
  group_by(sector_group) %>%
  mutate(
    Risk_Index_linear = 0.01 + (Risk_Index_linear - min(Risk_Index_linear, na.rm = TRUE)) /
      (max(Risk_Index_linear, na.rm = TRUE) - min(Risk_Index_linear, na.rm = TRUE)) * (0.99 - 0.01),
    Risk_Index_geo = 0.01 + (Risk_Index_geo - min(Risk_Index_geo, na.rm = TRUE)) /
      (max(Risk_Index_geo, na.rm = TRUE) - min(Risk_Index_geo, na.rm = TRUE)) * (0.99 - 0.01)
  ) %>%
  ungroup() %>%
  select(-sector_group)

# Read and join the shares data
Shares <- read_excel("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Base Data/EMPL_shares_data.xlsx") %>% 
  rename(Sector_ID = Sector) %>% 
  select(NUTS_ID, Sector_ID, Regional_Share)

Risk_Index <- Risk_Index %>% 
  left_join(Shares, by = c("NUTS_ID", "Sector_ID")) %>% 
  rename(Manuf_Share = Regional_Share)

# Write the risk index outputs
write_xlsx(Risk_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/Risk_Index_Data.xlsx")
write_csv(Risk_Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/Risk_Index_Data.csv")


## (Optional) Print output file paths
return("Outputs/Data/Vuln_Index_Data.xlsx")
return("Outputs/Data/Vuln_Index_Data.csv")
return("Outputs/Data/Risk_Index_Data.xlsx")
return("Outputs/Data/Risk_Index_Data.csv")

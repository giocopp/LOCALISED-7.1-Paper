# Set working directory
setwd("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index")

### Install Necessary Packages ###
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

### Get data
Exposure_Index <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Pipeline_1-Exposure/Outputs/Data/Exp_Data_index_Empl.xlsx") |> 
  rename(Sector_ID = Sector) |> 
  select(NUTS_ID, Sector_ID, Exposure_Index_Empl) |> 
  rename(Exposure_Index = Exposure_Index_Empl)

Energy_Index <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Pipeline_2-Vulnerability/Energy/Outputs/Data/Energy_Data_Index.xlsx")

Labor_Index <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Pipeline_2-Vulnerability/Labor/Outputs/Data/Labor_Index.xlsx")

SupCh_Index <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Pipeline_2-Vulnerability/Supply Chain/Outputs/Data/SupCh_Index.xlsx")


Tech_Index <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Pipeline_2-Vulnerability/Technology/Outputs/Data/Tech_Index.xlsx")

Finance_Index <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Pipeline_2-Vulnerability/Finance/Outputs/Data/Finance_Index.xlsx")

Inst_Index <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Pipeline_2-Vulnerability/Institutions/Outputs/Data/Inst_Index.xlsx") |> 
  select(-1)

### Merge
View(Finance_Index)

Index_x <- Exposure_Index |>
  full_join(Energy_Index, by = c("NUTS_ID", "Sector_ID")) |>
  full_join(Labor_Index, by = c("NUTS_ID", "Sector_ID")) |>
  full_join(SupCh_Index, by = c("NUTS_ID", "Sector_ID")) |>
  full_join(Tech_Index, by = c("NUTS_ID", "Sector_ID")) |>
  full_join(Finance_Index, by = c("NUTS_ID", "Sector_ID")) |>
  full_join(Inst_Index, by = c("NUTS_ID", "Sector_ID")) |> 
  select(-c(13, 14, 15)) 

Index_x <- Index_x |>
  filter(!(Sector_ID %in% c("C31-C32", "C33")))

sum(is.na(Index_x))

Metadata <- read_excel("~/Desktop/LOCALISED-7.1-Paper/Metadata-Indicators.xlsx")

# Extract the second and third columns as a correspondence dictionary
correspondence_dict <- Metadata %>%
  select(2, 3) %>%             # Select the second and third columns
  deframe()                    # Convert to a named vector (dictionary)

# Reverse the dictionary for renaming
reverse_correspondence_dict <- setNames(names(correspondence_dict), correspondence_dict)

# Apply the renaming dynamically
Index <- Index_x |>
  rename_with(
    ~ ifelse(is.na(reverse_correspondence_dict[.x]), .x, reverse_correspondence_dict[.x]),
    .cols = everything()
  )

Index_select <- Index |>
  select(NUTS_ID, Sector_ID, ends_with("Index"))


### Save

write_xlsx(Index, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_2-Vulnerability/Weighting and Aggregation/Outputs/Data/Index_Data.xlsx")

write_xlsx(Index_select, "~/Desktop/LOCALISED-7.1-Paper/Pipeline_2-Vulnerability/Weighting and Aggregation/Outputs/Data/Index_Data_Select.xlsx")

return("Outputs/Data/Index_Data.xlsx")
return("Outputs/Data/Index_Data_Select.xlsx")


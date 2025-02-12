setwd("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Pipeline_1-Exposure/")

empl_region <- readxl::read_xlsx("~/Desktop/LOCALISED-7.1-Paper/Pipeline_2-Vulnerability/Labor/Outputs/Data/EMPL_persons_raw_data.xlsx") |> 
  mutate(Empl_pers = Empl_pers / 100)

# Impute missing employment values using median by Sector (drop Time if present)
empl_region <- empl_region %>%
  select(-Time) %>% 
  group_by(Sector) %>%
  mutate(Empl_pers = if_else(is.na(Empl_pers),
                             median(Empl_pers, na.rm = TRUE),
                             Empl_pers)) %>%
  ungroup()

# Save the employment region data (without normalization)
writexl::write_xlsx(empl_region, "Outputs/Data/EMPL_Region.xlsx")

###
###

Regional_emissions_index <- readxl::read_xlsx("Outputs/Data/EXP_Data_index.xlsx") 

# --- Aggregate employment data to match emissions sectors ---
empl_region_agg <- empl_region %>%
  mutate(Sector_Agg = case_when(
    Sector == "C" ~ "C",
    Sector %in% c("C10", "C11", "C12") ~ "C10-C12",
    Sector %in% c("C13", "C14", "C15") ~ "C13-C15",
    Sector %in% c("C16", "C17", "C18") ~ "C16-C18",
    Sector %in% c("C19", "C20") ~ "C19-C20",
    Sector %in% c("C21", "C22") ~ "C21-C22",
    Sector == "C23" ~ "C23",
    Sector == "C24" ~ "C24",
    Sector %in% c("C25", "C28", "C29", "C30") ~ "C25+C28-C30",
    Sector %in% c("C26", "C27") ~ "C26-C27",
    Sector %in% c("C31", "C32") ~ "C31-C32",
    Sector == "C33" ~ "C33",
    TRUE ~ NA_character_
  )) %>%
  group_by(Sector_Agg) %>%
  summarise(Empl_pers = mean(Empl_pers, na.rm = TRUE)) %>%
  ungroup()

# --- Join aggregated employment with emissions index and compute weighted index ---
Regional_emissions_index <- Regional_emissions_index %>%
  left_join(empl_region_agg, by = c("Sector" = "Sector_Agg")) %>%
  mutate(Weighted_GHG = GHG_Emissions * Empl_pers,
         sector_group = if_else(Sector == "C", "C", "subsector")) %>%
  group_by(sector_group) %>%
  mutate(Exposure_Index_Empl = 0.01 + ((Weighted_GHG - min(Weighted_GHG, na.rm = TRUE)) /
                                         (max(Weighted_GHG, na.rm = TRUE) - min(Weighted_GHG, na.rm = TRUE)))*(0.99 - 0.01)) %>%
  ungroup() %>%
  select(-sector_group)

Regional_emissions_index <- Regional_emissions_index |> 
  filter(Sector != "C31-C32",
         Sector != "C33")

writexl::write_xlsx(
  Regional_emissions_index,
  "Outputs/Data/EXP_Data_index_Empl.xlsx")

return("Outputs/Data/EXP_Data_index_Empl.xlsx")
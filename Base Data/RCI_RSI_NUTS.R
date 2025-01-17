RCI <- readxl::read_xlsx("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Base Data/RCI_2022.xlsx", sheet = "RCI_2022_raw_data")

RCI

base_data <- readxl::read_xlsx("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Base Data/base_data.xlsx")

base_data

# Crosswalk from NUTS2 to the aggregated code (RCI_Agg)
crosswalk_RCI <- data.frame(
  NUTS_ID      = c("AT12", "AT13", "BE10", "BE24", "BE31",
                 "HR05", "HR06", "CZ01", "CZ02", "DE30",
                 "DE40", "HU11", "HU12", "NL23", "NL32"),
  NUTS_agg    = c("AT_C", "AT_C", "BE_C", "BE_C", "BE_C",
                 "HR_C", "HR_C", "CZ_C", "CZ_C", "DE_C",
                 "DE_C", "HU_C", "HU_C", "NL_C", "NL_C"),
  stringsAsFactors = FALSE
)

crosswalk_RCI

base_data <- base_data |> 
  # 1) Left-join the crosswalk
  left_join(crosswalk_RCI, by = "NUTS_ID") |> 
  
  # 2) Create a 'join_code' that uses the crosswalk NUTS if available,
  #    otherwise just use the original NUTS_ID.
  mutate(join_code = coalesce(NUTS_agg, NUTS_ID)) |> 
  rename("RCI_code" = "join_code") |>
  select(CNTR_CODE, NUTS_ID, NUTS_NAME, RCI_code)

View(base_data)

# RSI
RIS <- readxl::read_xlsx("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Base Data/RIS_2023.xlsx") |>
  rename("NUTS_ID" = "Region") |>
  filter(Year == 2023)

RIS

NUTS_1_RIS <- RIS |> 
  filter(nchar(NUTS_ID) == 3) |> 
  distinct(NUTS_ID)

NUTS_1_RIS

# Create a data frame with NUTS 1 and NUTS 2 region codes
nuts_mapping <- data.frame(
  NUTS_1 = c("AT1", "AT2", "AT3", "BE1", "BE2", "BE3", "DE3", "DE4", "DE5", "DE6", "DE8", "DEC", "DEE", "DEF", "DEG", "EL3", "ES3", "ES7", "FI2", "FR1", "FRB", "FRC", "FRD", "FRE", "FRF", "FRG", "FRH", "FRI", "FRJ", "FRK", "FRL", "FRM", "FRY", "PT2", "PT3"),
  NUTS_2 = c(
    "AT11, AT12, AT13",
    "AT21, AT22",
    "AT31, AT32, AT33, AT34",
    "BE10",
    "BE21, BE22, BE23, BE24, BE25",
    "BE31, BE32, BE33, BE34, BE35",
    "DE30",
    "DE40",
    "DE50",
    "DE60",
    "DE80",
    "DEC0",
    "DEE0",
    "DEF0",
    "DEG0",
    "EL30",
    "ES30",
    "ES70",
    "FI20",
    "FR10",
    "FRB0",
    "FRC1, FRC2",
    "FRD1, FRD2",
    "FRE1, FRE2",
    "FRF1, FRF2, FRF3",
    "FRG0",
    "FRH0",
    "FRI1, FRI2, FRI3",
    "FRJ1, FRJ2",
    "FRK1, FRK2",
    "FRL0",
    "FRM0",
    "FRY1, FRY2, FRY3, FRY4, FRY5",
    "PT20",
    "PT30"
  )
)

crosswalk_RSI <- nuts_mapping |> 
  separate_rows(NUTS_2, sep = ",\\s*") |> 
  rename("NUTS_ID" = "NUTS_2") 

crosswalk_RSI

base_data <- base_data |> 
  left_join(crosswalk_RSI, by = "NUTS_ID") |> 
  mutate(join_code = coalesce(NUTS_1, NUTS_ID)) |> 
  rename("RIS_code" = "join_code") |> 
  select(CNTR_CODE, NUTS_ID, NUTS_NAME, RCI_code, RIS_code)

base_data

# Save base_data as base_data_plus.xlsx
openxlsx::write.xlsx(base_data, "/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Base Data/base_data_plus.xlsx", rowNames = FALSE)

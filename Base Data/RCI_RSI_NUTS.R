RCI <- readxl::read_xlsx("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Base Data/RCI_2022.xlsx", sheet = "RCI_2022_raw_data")

RIS <- readxl::read_xlsx("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Base Data/RIS_2023.xlsx") |> 
  rename("NUTS_ID" = "Region") |>
  filter(Year == 2023)
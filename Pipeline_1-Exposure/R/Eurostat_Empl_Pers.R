setwd("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Pipeline_1-Exposure")

### INSTALL PACKAGES

# install.packages("remotes")
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

indicator_df_og <- restatapi::get_eurostat_data(
  id = "sbs_r_nuts2021",
  filters = c("EMP_LOC_NR"),
  date_filter = c(2022, 2021),
  exact_match = F,
  label = F,
  cflags = T,
  keep_flags = T
)

rm(list = setdiff(ls(), "indicator_df_og"))

###
###
unique(indicator_df_og$geo)

indicator_df_f <- indicator_df_og |> 
  dplyr::select(1, 3, 4, 5, 6) |> 
  dplyr::rename(
    "NUTS_ID" = "geo",
    "Empl_pers" = "values",
    "Sector" = "nace_r2",
    "Time" = "time",
    "Flags" = "flags"
  ) 

indicator_df_f <- indicator_df_f |> 
  dplyr::filter(Time == 2022) |> 
  dplyr::filter(substr(Sector, 1, 1) == "C") |> 
  dplyr::filter(nchar(as.character(NUTS_ID)) != 3) |> 
  dplyr::select(-5) |> 
  dplyr::filter(substr(NUTS_ID, 1, 2) != "ZZ")

base_data <- readxl::read_excel("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Base Data/base_data.xlsx") |> 
  dplyr::select(1, 3)

indicator_df_f <- base_data |> 
  dplyr::left_join(
    indicator_df_f,
    by = "NUTS_ID"
  )

indicator_df_f <- indicator_df_f |> 
  dplyr::select(c(NUTS_ID, dplyr::everything()))

### IMPUTATION OF MISSING VALUES with older values
# Impute missing 2022 values with corresponding 2021 values
indicator_df_f <- indicator_df_f %>%
  group_by(Sector, NUTS_ID) %>%  # Group by Sector and NUTS_ID
  mutate(
    Empl_pers = ifelse(
      Time == 2022 & is.na(Empl_pers), 
      Empl_pers[Time == 2021],         
      Empl_pers                        
    )
  ) %>%
  ungroup()
  
EMPL_persons_raw_data <- indicator_df_f
# return(EMPL_persons_raw_data)

writexl::write_xlsx(
  EMPL_persons_raw_data,
  "Outputs/Data/EMPL_persons_raw_data.xlsx")

return("Outputs/Data/EMPL_persons_raw_data.xlsx")
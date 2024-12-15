# Load targets library
library(targets)

# Set target options
tar_option_set(
  packages = c(
    "targets",
    "tidyverse",
    "sf",
    "readxl",
    "classInt",
    "dlookr",
    "mice",
    "VIM",
    "writexl",
    "giscoR",
    "visdat",
    "ggplot2"
  ),
  format = "file" # Track file outputs
)

# Define the pipeline
list(
  tar_target(
    name = step1,
    command = source("R/Eurostat_Empl_Pers.R")$value, # Output tracked file
    format = "file"
  ),
  tar_target(
    name = step2,
    command = {
      step1_file <- step1 # Input file from step1
      source("R/Impute_Empl_Clean.R")$value # Output tracked file
    },
    format = "file"
  ),
  tar_target(
    name = step3,
    command = {
      step2_file <- step2 # Input file from step2
      source("R/Empl_Share.R")$value # Output tracked file
    },
    format = "file"
  ),
  tar_target(
    name = step4,
    command = {
      step3_file <- step3 # Input file from step3
      source("R/Eurostat_Emissions.R")$value # Output tracked file
    },
    format = "file"
  ),
  tar_target(
    name = step5,
    command = {
      step4_file <- step4 # Input file from step4
      source("R/Exposure_Map_RegEmis.R")$value # Outputs tracked plot files
    },
    format = "file"
  )
)

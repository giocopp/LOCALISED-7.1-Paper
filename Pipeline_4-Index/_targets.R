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
    command = source("R/Aggregation.R")$value, # Output tracked file
    format = "file"
  ),
  tar_target(
    name = step2,
    command = {
      step1_file <- step1 # Input file from step1
      source("R/Calculation_Vuln.R")$value # Output tracked file
    },
    format = "file"
  ),
  tar_target(
    name = step3,
    command = {
      step2_file <- step2 # Input file from step2
      source("R/Vuln_Index_Maps.R")$value # Output tracked file
    },
    format = "file"
  ),
  tar_target(
    name = step4,
    command = {
      step2_file <- step2 # Input file from step2
      source("R/Heatmaps.R")$value # Output tracked file
    },
    format = "file"
  )
)

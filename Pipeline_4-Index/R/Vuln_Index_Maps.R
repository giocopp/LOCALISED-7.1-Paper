setwd("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index")
#
# # Install Packages
# remotes::install_github("eurostat/restatapi")
#
libs <- c(
  "restatapi",
  "tidyverse",
  "giscoR",
  "sf",
  "classInt",
  "RColorBrewer",
  "ggnewscale"
)

installed_libs <- libs %in% rownames(installed.packages())

if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs], dependencies = TRUE)
}

invisible(lapply(libs, library, character.only = TRUE))

# Read data
Vuln_Index  <- readxl::read_xlsx("~/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index/Outputs/Data/Vuln_Index_Data.xlsx") |>
  select(NUTS_ID, Sector_ID, Vulnerability_Index)

Risk_Index <- readxl::read_xlsx("Outputs/Data/Risk_Index_Data.xlsx")

Risk_Index <- Vuln_Index |> 
  left_join(
    Risk_Index, 
    by = c("NUTS_ID", "Sector_ID")
  ) |>
  select(
    NUTS_ID, Sector_ID, Exposure_Index, Energy_Index, Labor_Index, 
    Sup_Ch_Index, Tech_Index, Finance_Index, Inst_Index, 
    Vulnerability_Index = Vulnerability_Index.x,  # Explicitly select from Vuln_Index
    Risk_Index, Manuf_Share
  )

# Get Boundaries
nuts2_sf <- giscoR::gisco_get_nuts(
  nuts_level = "2",
  resolution = "3",
  year = "2021"
)

countries_sf <- giscoR::gisco_get_countries(
  resolution = "3",
  region = "EU",
  year = "2020"
)

# Filter EU Countries
eu_list <- unique(countries_sf$CNTR_ID)

eu_sf <- nuts2_sf %>%
  dplyr::filter(CNTR_CODE %in% eu_list)

overseas_codes <- c("FRY", "ES7", "PT2")

eu_sf <- eu_sf %>%
  filter(!stringr::str_sub(NUTS_ID, 1, 3) %in% overseas_codes)

# Merge with NUTS2 boundaries
mapping_sf <- eu_sf %>%
  dplyr::left_join(Risk_Index, by = c("NUTS_ID"))

# Verify the merged dataset
if (nrow(mapping_sf) == 0) {
  stop("No data found after merging. Check 'NUTS_ID' consistency between datasets.")
}

# Save GeoJSON file
mapping_sf_GeoJSON <- mapping_sf |> 
  select(NUTS_ID, geometry)

# Save the sf object as a GeoJSON file
# st_write(mapping_sf_GeoJSON, "Outputs/Data/mapping_sf.geojson", driver = "GeoJSON")

# Define Lambert Projection
# Define Lambert Projection
crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

create_map <- function(
    data,                           # Dataset to use
    region = "EU",                  # Region to map
    variable = "Risk_Index",        # Variable to map
    variable_name = "Risk Index",   # Legend title
    sector = NULL,                  # Sector filter (optional)
    output_dir = "Outputs/Plots",   # Directory to save the plot
    fixed_range = c(0, 1),          # Fixed range for the variable
    color_palette = "Purples"       # Color palette for the map
) {
  # Define Lambert CRS
  crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Filter data for the region
  if (region == "EU") {
    region_sf <- data
    base_sf <- eu_sf
    bbox <- sf::st_bbox(sf::st_transform(eu_sf, crs = crs_lambert))
  } else {
    region_sf <- data %>%
      filter(CNTR_CODE == region)
    base_sf <- nuts2_sf %>%
      filter(CNTR_CODE == region)
    bbox <- sf::st_bbox(sf::st_transform(base_sf, crs = crs_lambert))
  }

  # Filter data for the sector (if provided)
  if (!is.null(sector)) {
    region_sf <- region_sf %>%
      filter(Sector_ID == sector)
  }

  # Ensure the variable is numeric and remove NA values
  region_sf <- region_sf %>%
    mutate(!!sym(variable) := as.numeric(!!sym(variable))) %>%
    filter(!is.na(!!sym(variable)))

  # Debugging: Print summary of the variable and geometries
  print(summary(region_sf[[variable]]))
  print(sf::st_is_empty(region_sf))

  # Apply Lambert projection to the base and region data
  base_sf <- sf::st_transform(base_sf, crs = crs_lambert)
  region_sf <- sf::st_transform(region_sf, crs = crs_lambert)

  # Define breaks and colors
  breaks <- seq(fixed_range[1], fixed_range[2], length.out = 6)
  cols <- colorRampPalette(brewer.pal(n = 9, name = color_palette))(length(breaks) - 1)

  # Create the map
  map_plot <- ggplot(data = region_sf) +
    geom_sf(aes_string(fill = variable), color = "black", size = 0.01) +
    geom_sf(data = base_sf, color = "black", size = 0.005, fill = "transparent") +
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE
    ) +
    scale_fill_gradientn(
      name = variable_name,
      colors = cols,
      breaks = breaks,
      labels = round(breaks, 2),
      limits = fixed_range,
      na.value = "grey80"
    ) +
    guides(
      fill = guide_colorbar(
        direction = "vertical",
        barheight = unit(35, "mm"),
        barwidth = unit(6, "mm"),
        title.position = "top",
        title.hjust = 0.5,
        label.position = "right",
        label.hjust = 0.5
      )
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 12, color = "grey10"),
      legend.text = element_text(size = 11, color = "grey10")
    )

  # Save the plot
  output_path <- file.path(output_dir, paste0(region, "_", sector, "_", variable, "_map.png"))
  ggsave(output_path, plot = map_plot, width = 15, height = 11, dpi = 900)

  # Return the path to the saved plot
  return(output_path)
}

### Usage
### 

# Define the regions (or country codes) you want to map
regions <- c("EU", "IT")  # You can add more codes as needed

# Define the sector to map (in your examples it's "C")
sector <- "C"

# Define a list of mapping variables with their associated legend names, color palettes, and fixed ranges
mapping_vars <- list(
  Risk_Index = list(var_name = "Risk Index", palette = "Purples", fixed_range = c(0, 1)),
  Vulnerability_Index = list(var_name = "Vulnerability Index", palette = "Blues", fixed_range = c(0, 1)),
  Exposure_Index = list(var_name = "Exposure Index", palette = "Reds", fixed_range = c(0, 1))
)

# Loop over each region and each mapping variable to create and save maps
for (reg in regions) {
  for (var in names(mapping_vars)) {
    var_info <- mapping_vars[[var]]
    output_path <- create_map(
      data = mapping_sf,
      region = reg,
      sector = sector,
      variable = var,
      variable_name = var_info$var_name,
      fixed_range = var_info$fixed_range,
      color_palette = var_info$palette
    )
    print(paste("Saved map for", reg, "with", var, "at", output_path))
  }
}

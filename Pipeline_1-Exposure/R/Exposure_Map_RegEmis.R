# Install Packages
remotes::install_github("eurostat/restatapi")

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
regional_emiss <- readxl::read_xlsx("Outputs/Data/EXP_Data.xlsx")

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

# Merge with NUTS2 boundaries
mapping_sf <- eu_sf %>%
  dplyr::left_join(regional_emiss, by = c("NUTS_ID"))

# Verify the merged dataset
if (nrow(mapping_sf) == 0) {
  stop("No data found after merging. Check 'NUTS_ID' consistency between datasets.")
}

# Define Lambert Projection
crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

# Function to Create a Map for a Specific Region and Sector
create_map <- function(region = "EU", sector = "C", output_dir = "Outputs/Plots") {
  # Ensure directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Filter data for the specified region
  if (region == "EU") {
    region_sf <- mapping_sf
    base_sf <- eu_sf
  } else {
    region_sf <- mapping_sf %>%
      filter(CNTR_CODE == region)
    base_sf <- nuts2_sf %>%
      filter(CNTR_CODE == region)
  }
  
  # Filter data for the specified sector
  sector_data <- region_sf %>%
    filter(Sector == sector) %>%
    mutate(Normalized_Emissions = as.numeric(Normalized_Emissions))
  
  # Check for missing or empty data
  if (nrow(sector_data) == 0) {
    stop(paste("No data available for region:", region, "and sector:", sector))
  }
  
  # Define breaks and colors dynamically
  breaks <- classInt::classIntervals(
    sector_data$Normalized_Emissions,
    n = 5,
    style = "equal"
  )$brks
  
  cols <- colorRampPalette(brewer.pal(n = 9, name = "Reds"))(length(breaks) - 1)
  
  # Dynamically calculate the bounding box for the selected region
  bbox <- sf::st_bbox(base_sf)  # Get the bounding box of the selected region
  
  # Create the map
  map_plot <- ggplot(data = sector_data) +
    geom_sf(mapping = aes(fill = Normalized_Emissions), color = "black", size = .01) +
    geom_sf(data = base_sf, color = "black", size = .005, fill = "transparent") +
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE
    ) +
    scale_fill_gradientn(
      name = "Emissions (Index)",
      colors = cols,
      breaks = breaks,
      labels = round(breaks, 2),
      limits = range(sector_data$Normalized_Emissions, na.rm = TRUE),
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
      legend.title = element_text(size = 16, color = "grey10"),
      legend.text = element_text(size = 13, color = "grey10")
    )
  
  # Save the plot
  output_path <- file.path(output_dir, paste0(region, "_", sector, "_emissions_map.png"))
  ggsave(output_path, plot = map_plot, width = 10, height = 7)
  
  # Return the path to the saved plot
  return(output_path)
}


# Example Usage
# Create a map for the EU and sector "C"
eu_map_path <- create_map(region = "EU", sector = "C")

# Create a map for Italy and sector "C10"
italy_map_path <- create_map(region = "IT", sector = "C")



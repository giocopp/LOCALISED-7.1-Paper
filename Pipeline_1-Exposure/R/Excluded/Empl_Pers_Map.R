### INSTALL PACKAGES

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

### GET BOUNDARIES

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

### FILTER COUNTRIES
### 

eu_list <- c(
  unique(countries_sf$CNTR_ID)
)

eu_sf <- nuts2_sf |> 
  dplyr::filter(
    CNTR_CODE %in% eu_list
  )

### LOAD EMPLOYMENT DATA
indicator_df_f <-  readxl::read_excel("~/Desktop/Localised 7.1 Paper/Exposure/Data/EMPL_persons_imputed_data.xlsx")

unique(indicator_df_f$NUTS_ID)

# Merge with NUTS2 boundaries

mapping_sf <-  eu_sf |> 
  dplyr::left_join(
    indicator_df_f,
    by = c("NUTS_ID")
  ) |> 
  na.omit()

# Defines breaks, colors and bounds

ni <- classInt::classIntervals(
  mapping_sf$Empl_pers,
  n = 6,
  style = "equal"
)$brks

brk <- ni |> 
  append(
    max(mapping_sf$Empl_pers)
  ) |> 
  head(-1)

library(RColorBrewer)

crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

xmin <- -10.66
ymin <- 33.5
xmax <- 39
ymax <- 67.5

bbox <- sf::st_sfc(
  sf::st_polygon(
    list(
      cbind(
        c(
          xmin,
          xmax,
          xmax,
          xmin,
          xmin
        ),
        c(
          ymin,
          ymin,
          ymax,
          ymax,
          ymin
        )
      )
    )
  ), crs = 4326
)

lambert_bbox <- sf::st_transform(
  bbox,
  crs = crs_lambert
)

bb <- sf::st_bbox(lambert_bbox)

# 10. MAP
#--------

plot_empl_map <- function(sector) {
  # Filter the dataset for the selected sector
  sector_data <- mapping_sf %>%
    filter(Sector == sector)
  
  # Ensure the `Empl_pers` column is numeric
  sector_data <- sector_data %>%
    mutate(Empl_pers = as.numeric(Empl_pers))
  
  # Define breaks and colors dynamically
  breaks <- classInt::classIntervals(
    sector_data$Empl_pers,
    n = 5,
    style = "equal"
  )$brks
  
  cols <- colorRampPalette(brewer.pal(n = 9, name = "Reds"))(length(breaks) - 1)
  
  # Create the plot
  ggplot(data = sector_data) +
    geom_sf(
      mapping = aes(
        fill = Empl_pers
      ),
      color = "black",
      size = .01
    ) +
    geom_sf(
      data = eu_sf,
      color = "black",
      size = .005,
      fill = "transparent"
    ) +
    coord_sf(
      crs = crs_lambert,
      xlim = c(bb["xmin"], bb["xmax"]),
      ylim = c(bb["ymin"], bb["ymax"]),
    ) +
    scale_fill_gradientn(
      name = "Employment (persons)",
      colors = cols,
      breaks = breaks,
      labels = round(breaks, 0),
      limits = c(
        min(sector_data$Empl_pers, na.rm = TRUE),
        max(sector_data$Empl_pers, na.rm = TRUE)
      ),
      na.value = "grey80"
    ) +
    guides(
      fill = guide_colorbar(
        direction = "vertical",  # Vertical orientation
        barheight = unit(20, "mm"), # Smaller bar height
        barwidth = unit(4, "mm"),   # Smaller bar width
        title.position = "top",     # Title at the top of the legend
        title.hjust = 0.5,          # Center align title
        label.position = "right",   # Labels on the right side of the bar
        label.hjust = 0.5           # Center align labels
      )
    ) +
    theme_void() +
    theme(
      legend.position = "right",   # Place legend on the right
      legend.title = element_text(
        size = 8, color = "grey10" # Smaller title size
      ),
      legend.text = element_text(
        size = 7, color = "grey10" # Smaller label size
      )
    )
}

plot_empl_map("C")
              
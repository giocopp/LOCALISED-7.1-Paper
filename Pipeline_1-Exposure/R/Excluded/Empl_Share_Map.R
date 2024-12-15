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
indicator_df_f <-  readxl::read_excel("~/Desktop/Localised 7.1 Paper/Exposure/Data/EMPL_shares_data.xlsx")

unique(indicator_df_f$Sector)

# Merge with NUTS2 boundaries

mapping_sf <-  eu_sf |> 
  dplyr::left_join(
    indicator_df_f,
    by = c("NUTS_ID")
  )

# Defines breaks, colors and bounds

ni <- classInt::classIntervals(
  mapping_sf$Regional_Share,
  n = 5,
  style = "equal"
)$brks

brk <- ni |> 
  append(
    max(mapping_sf$Regional_Share)
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
  library(ggnewscale)
  
  # Filter the dataset for the selected sector
  sector_data <- mapping_sf %>%
    filter(Sector == sector)
  
  # Split data by country
  sector_data_list <- split(sector_data, sector_data$Country)
  
  # Initialize ggplot with EU boundaries
  p <- ggplot() +
    geom_sf(
      data = eu_sf,
      color = "black",
      size = .005,
      fill = "transparent"
    )
  
  # Iterate through each country and add a layer with its own scale
  for (cnty in names(sector_data_list)) {
    cdata <- sector_data_list[[cnty]]
    
    # Skip countries with a single unique value for Regional_Share
    if (length(unique(cdata$Regional_Share)) == 1) {
      next
    }
    
    breaks <- classInt::classIntervals(
      cdata$Regional_Share,
      n = 5,
      style = "equal"
    )$brks
    
    cols <- colorRampPalette(brewer.pal(n = 9, name = "Reds"))(length(breaks) - 1)
    
    p <- p +
      ggnewscale::new_scale_fill() +
      geom_sf(
        data = cdata,
        aes(fill = Regional_Share),
        color = "black",
        size = .01
      ) +
      scale_fill_gradientn(
        colors = cols,
        breaks = breaks,
        labels = scales::comma(breaks),
        limits = c(
          min(cdata$Regional_Share, na.rm = TRUE),
          max(cdata$Regional_Share, na.rm = TRUE)
        ),
        na.value = "grey80"
      )
  }
  
  # Finalize the map with coordinates and themes
  p +
    coord_sf(
      crs = crs_lambert,
      xlim = c(bb["xmin"], bb["xmax"]),
      ylim = c(bb["ymin"], bb["ymax"])
    ) +
    theme_void()+
    theme(legend.position = "none")
}

plot_empl_map("C")

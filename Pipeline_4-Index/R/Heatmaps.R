setwd("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index")

# Load necessary libraries
libs <- c("restatapi", "tidyverse", "giscoR", "sf", "classInt", "RColorBrewer", "ggnewscale", "readxl")
installed_libs <- libs %in% rownames(installed.packages())
if (any(!installed_libs)) {
  install.packages(libs[!installed_libs], dependencies = TRUE)
}
invisible(lapply(libs, library, character.only = TRUE))

# Function to create a heatmap given a country and an index variable
create_heatmap <- function(country_code, index_type) {
  # Read and filter data for the selected country
  data <- readxl::read_xlsx("Outputs/Data/Risk_Index_Data.xlsx") %>%
    select(Country, Region_Name, Sector_Name, Exposure_Index, Vulnerability_Index, Risk_Index) %>%
    filter(Country == country_code)
  
  # Separate "Total Manufacturing" before clustering
  total_manufacturing <- data %>% filter(Sector_Name == "Total Manufacturing")
  remaining_sectors   <- data %>% filter(Sector_Name != "Total Manufacturing")
  
  # Cluster remaining sectors
  heatmap_data_remaining <- remaining_sectors %>% 
    select(Exposure_Index, Vulnerability_Index, Risk_Index) %>% 
    as.matrix()
  row_clusters_remaining <- hclust(dist(heatmap_data_remaining), method = "ward.D2")
  remaining_sectors <- remaining_sectors[row_clusters_remaining$order, ]
  
  # Combine "Total Manufacturing" with the reordered sectors and update factor levels
  ordered_data <- bind_rows(total_manufacturing, remaining_sectors) %>%
    mutate(Sector_Name = factor(Sector_Name, levels = unique(Sector_Name)))
  
  # Convert data to long format for ggplot
  heatmap_long <- ordered_data %>%
    pivot_longer(cols = c(Exposure_Index, Vulnerability_Index, Risk_Index),
                 names_to = "Index_Type", values_to = "Value") %>%
    mutate(Sector_Name = factor(Sector_Name, levels = unique(Sector_Name)))
  
  # Define custom color palette
  full_color_palette <- c(
    "#08306b", "#2171b5", "#6baed6", "#bdd7e7",
    "#f7fbff", "#ffffcc", "#ffeda0", "#feb24c",
    "#f03b20", "#bd0026"
  )
  
  # Define a common theme for the heatmaps
  theme_common <- theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      axis.title.x = element_text(size = 11, face = "bold"),
      axis.title.y = element_text(size = 11, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.6, "cm")
    )
  
  # Filter data for the specified index type
  df <- heatmap_long %>% filter(Index_Type == index_type)
  
  # Calculate min, median, and max for the color scale
  min_val    <- min(df$Value, na.rm = TRUE)
  median_val <- median(df$Value, na.rm = TRUE)
  max_val    <- max(df$Value, na.rm = TRUE)
  
  # Determine the legend title based on the index type
  legend_title <- switch(index_type,
                         "Risk_Index" = "Risk Value",
                         "Exposure_Index" = "Exposure Value",
                         "Vulnerability_Index" = "Vulnerability Value",
                         index_type)
  
  # Create and return the heatmap
  p <- ggplot(df, aes(x = Region_Name, y = Sector_Name, fill = Value)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(
      colors = full_color_palette,
      values = scales::rescale(c(min_val, median_val, max_val),
                               to = c(0, 1),
                               from = range(df$Value, na.rm = TRUE)),
      name = legend_title,
      breaks = c(min_val, median_val, max_val),
      labels = c(
        paste0("Min: ", round(min_val, 2)),
        paste0("Median: ", round(median_val, 2)),
        paste0("Max: ", round(max_val, 2))
      ),
      limits = c(min_val, max_val)
    ) +
    labs(x = "Region", y = "Sector") +
    theme_common +
    guides(
      fill = guide_colorbar(
        barwidth = 0.5,
        barheight = 7,
        ticks.colour = "black",
        ticks.linewidth = 0.5
      )
    )
  return(p)
}

# Use the country code directly in the function call
country_code <- "PL"  # Pass the country code here (e.g., "ES", "AT", "DE", etc.)
risk_heatmap          <- create_heatmap(country_code, "Risk_Index")
exposure_heatmap      <- create_heatmap(country_code, "Exposure_Index")
vulnerability_heatmap <- create_heatmap(country_code, "Vulnerability_Index")

# Display the heatmaps
print(risk_heatmap)
print(exposure_heatmap)
print(vulnerability_heatmap)

# Save the heatmaps with appropriate file names using the country_code variable
heatmaps <- list(
  Risk = risk_heatmap,
  Exposure = exposure_heatmap,
  Vulnerability = vulnerability_heatmap
)

for (name in names(heatmaps)) {
  ggsave(filename = paste0("Outputs/Plots/", country_code, "_", name, "_Heatmap.png"),
         plot = heatmaps[[name]], width = 10, height = 6, dpi = 800)
}

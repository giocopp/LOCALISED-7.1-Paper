# setwd("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index")
# 
# # Load necessary libraries
# libs <- c("restatapi", "tidyverse", "giscoR", "sf", "classInt", "RColorBrewer", "ggnewscale", "readxl")
# installed_libs <- libs %in% rownames(installed.packages())
# if (any(!installed_libs)) {
#   install.packages(libs[!installed_libs], dependencies = TRUE)
# }
# invisible(lapply(libs, library, character.only = TRUE))
# 
# # Function to create heatmaps for Risk, Exposure, and Vulnerability indices
# create_heatmap <- function(country_code, index_type) {
#   # Read and filter data for the selected country (keep all sectors)
#   data <- readxl::read_xlsx("Outputs/Data/Risk_Index_Data.xlsx") %>%
#     select(Country, Region_Name, Sector_Name, Exposure_Index, Vulnerability_Index, Risk_Index, Manuf_Share) %>%
#     filter(Country == country_code)
#   
#   # Separate "Total Manufacturing" from the other sectors
#   tm_data    <- data %>% filter(Sector_Name == "Total Manufacturing")
#   other_data <- data %>% filter(Sector_Name != "Total Manufacturing")
#   
#   # Cluster other sectors based on the index values (if any)
#   if(nrow(other_data) > 0) {
#     heatmap_data <- other_data %>% 
#       select(Exposure_Index, Vulnerability_Index, Risk_Index) %>% 
#       as.matrix()
#     row_clusters <- hclust(dist(heatmap_data), method = "ward.D2")
#     ordered_other_data <- other_data[row_clusters$order, ]
#     # Extract the unique sector names in the clustering order
#     other_levels <- unique(ordered_other_data$Sector_Name)
#   } else {
#     ordered_other_data <- other_data
#     other_levels <- character(0)
#   }
#   
#   # Combine "Total Manufacturing" rows first so they appear at the bottom
#   ordered_data <- bind_rows(tm_data, ordered_other_data)
#   # Set factor levels so that "Total Manufacturing" is the first level (appearing at the bottom in ggplot)
#   new_levels <- c("Total Manufacturing", other_levels)
#   ordered_data <- ordered_data %>%
#     mutate(Sector_Name = factor(Sector_Name, levels = new_levels))
#   
#   # Convert data to long format for ggplot (for the indices)
#   heatmap_long <- ordered_data %>%
#     pivot_longer(cols = c(Exposure_Index, Vulnerability_Index, Risk_Index),
#                  names_to = "Index_Type", values_to = "Value") %>%
#     mutate(Sector_Name = factor(Sector_Name, levels = new_levels))
#   
#   # Define custom color palette for the indices
#   full_color_palette <- c(
#     "#08306b", "#2171b5", "#6baed6", "#bdd7e7",
#     "#f7fbff", "#ffffcc", "#ffeda0", "#feb24c",
#     "#f03b20", "#bd0026"
#   )
#   
#   # Define a common theme for the heatmaps
#   theme_common <- theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
#       axis.text.y = element_text(size = 9),
#       axis.title.x = element_text(size = 11, face = "bold"),
#       axis.title.y = element_text(size = 11, face = "bold"),
#       legend.position = "right",
#       legend.title = element_text(size = 8, face = "bold"),
#       legend.text = element_text(size = 7),
#       legend.key.size = unit(0.6, "cm")
#     )
#   
#   # Filter data for the specified index type
#   df <- heatmap_long %>% filter(Index_Type == index_type)
#   
#   # Calculate min, median, and max for the color scale
#   min_val    <- min(df$Value, na.rm = TRUE)
#   median_val <- median(df$Value, na.rm = TRUE)
#   max_val    <- max(df$Value, na.rm = TRUE)
#   
#   # Determine the legend title based on the index type
#   legend_title <- switch(index_type,
#                          "Risk_Index" = "Risk Value",
#                          "Exposure_Index" = "Exposure Value",
#                          "Vulnerability_Index" = "Vulnerability Value",
#                          index_type)
#   
#   # Create and return the heatmap for the given index
#   p <- ggplot(df, aes(x = Region_Name, y = Sector_Name, fill = Value)) +
#     geom_tile(color = "white") +
#     scale_fill_gradientn(
#       colors = full_color_palette,
#       values = scales::rescale(c(min_val, median_val, max_val),
#                                to = c(0, 1),
#                                from = range(df$Value, na.rm = TRUE)),
#       name = legend_title,
#       breaks = c(min_val, median_val, max_val),
#       labels = c(
#         paste0("Min: ", round(min_val, 2)),
#         paste0("Median: ", round(median_val, 2)),
#         paste0("Max: ", round(max_val, 2))
#       ),
#       limits = c(min_val, max_val)
#     ) +
#     labs(x = "Region", y = "Sector") +
#     theme_common +
#     guides(
#       fill = guide_colorbar(
#         barwidth = 0.5,
#         barheight = 7,
#         ticks.colour = "black",
#         ticks.linewidth = 0.5
#       )
#     )
#   return(p)
# }
# 
# # Function to create a heatmap for Manufacturing Share using a greyscale palette
# create_manuf_share_heatmap <- function(country_code) {
#   # Read and filter data for the selected country, selecting only Manuf_Share
#   data <- readxl::read_xlsx("Outputs/Data/Risk_Index_Data.xlsx") %>%
#     select(Country, Region_Name, Sector_Name, Manuf_Share) %>%
#     filter(Country == country_code)
#   
#   # Separate "Total Manufacturing" from the other sectors
#   tm_data    <- data %>% filter(Sector_Name == "Total Manufacturing")
#   other_data <- data %>% filter(Sector_Name != "Total Manufacturing")
#   
#   # Order other sectors (clustering on Manuf_Share, which is a single variable)
#   if(nrow(other_data) > 0) {
#     heatmap_data <- as.matrix(other_data$Manuf_Share)
#     row_clusters <- hclust(dist(heatmap_data), method = "ward.D2")
#     ordered_other_data <- other_data[row_clusters$order, ]
#     other_levels <- unique(ordered_other_data$Sector_Name)
#   } else {
#     ordered_other_data <- other_data
#     other_levels <- character(0)
#   }
#   
#   # Combine "Total Manufacturing" rows first so they appear at the bottom
#   ordered_data <- bind_rows(tm_data, ordered_other_data)
#   new_levels <- c("Total Manufacturing", other_levels)
#   ordered_data <- ordered_data %>%
#     mutate(Sector_Name = factor(Sector_Name, levels = new_levels))
#   
#   # Define a greyscale palette (from light to dark)
#   grey_palette <- grey.colors(10, start = 0.95, end = 0.05)
#   
#   # Define a common theme
#   theme_common <- theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
#       axis.text.y = element_text(size = 9),
#       axis.title.x = element_text(size = 11, face = "bold"),
#       axis.title.y = element_text(size = 11, face = "bold"),
#       legend.position = "right",
#       legend.title = element_text(size = 8, face = "bold"),
#       legend.text = element_text(size = 7),
#       legend.key.size = unit(0.6, "cm")
#     )
#   
#   # Calculate min, median, and max for Manuf_Share
#   min_val <- min(ordered_data$Manuf_Share, na.rm = TRUE)
#   median_val <- median(ordered_data$Manuf_Share, na.rm = TRUE)
#   max_val <- max(ordered_data$Manuf_Share, na.rm = TRUE)
#   
#   # Create and return the greyscale heatmap for Manufacturing Share
#   p <- ggplot(ordered_data, aes(x = Region_Name, y = Sector_Name, fill = Manuf_Share)) +
#     geom_tile(color = "white") +
#     scale_fill_gradientn(
#       colors = grey_palette,
#       values = scales::rescale(c(min_val, median_val, max_val),
#                                to = c(0, 1),
#                                from = range(ordered_data$Manuf_Share, na.rm = TRUE)),
#       name = "Manufacturing Share",
#       breaks = c(min_val, median_val, max_val),
#       labels = c(
#         paste0("Min: ", round(min_val, 2)),
#         paste0("Median: ", round(median_val, 2)),
#         paste0("Max: ", round(max_val, 2))
#       ),
#       limits = c(min_val, max_val)
#     ) +
#     labs(x = "Region", y = "Sector") +
#     theme_common +
#     guides(
#       fill = guide_colorbar(
#         barwidth = 0.5,
#         barheight = 7,
#         ticks.colour = "black",
#         ticks.linewidth = 0.5
#       )
#     )
#   return(p)
# }
# 
# # Use the country code directly in the function call
# country_code <- "IT"  # e.g., "ES", "AT", "DE", etc.
# 
# # Create heatmaps for the indices
# risk_heatmap          <- create_heatmap(country_code, "Risk_Index")
# exposure_heatmap      <- create_heatmap(country_code, "Exposure_Index")
# vulnerability_heatmap <- create_heatmap(country_code, "Vulnerability_Index")
# 
# # Create the greyscale heatmap for Manufacturing Share
# manuf_share_heatmap <- create_manuf_share_heatmap(country_code)
# 
# # Display all the heatmaps
# print(risk_heatmap)
# print(exposure_heatmap)
# print(vulnerability_heatmap)
# print(manuf_share_heatmap)
# 
# # Save the heatmaps with appropriate file names using the country_code variable
# heatmaps <- list(
#   Risk = risk_heatmap,
#   Exposure = exposure_heatmap,
#   Vulnerability = vulnerability_heatmap,
#   Manuf_Share = manuf_share_heatmap
# )
# 
# for (name in names(heatmaps)) {
#   ggsave(filename = paste0("Outputs/Plots/", country_code, "_", name, "_Heatmap.png"),
#          plot = heatmaps[[name]], width = 10, height = 6, dpi = 800)
# }

setwd("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Pipeline_4-Index")

# Load necessary libraries
libs <- c("restatapi", "tidyverse", "giscoR", "sf", "classInt", "RColorBrewer", "ggnewscale", "readxl")
installed_libs <- libs %in% rownames(installed.packages())
if (any(!installed_libs)) {
  install.packages(libs[!installed_libs], dependencies = TRUE)
}
invisible(lapply(libs, library, character.only = TRUE))

# Function to create heatmaps for Risk, Exposure, and Vulnerability indices
create_heatmap <- function(country_code, index_type) {
  # Read and filter data for the selected country, excluding Total Manufacturing
  data <- readxl::read_xlsx("Outputs/Data/Risk_Index_Data.xlsx") %>%
    select(Country, Region_Name, Sector_Name, Exposure_Index, Vulnerability_Index, Risk_Index, Manuf_Share) %>%
    filter(Country == country_code, Sector_Name != "Total Manufacturing")
  
  # Cluster remaining sectors based on the index values
  if(nrow(data) > 0) {
    heatmap_data <- data %>% 
      select(Exposure_Index, Vulnerability_Index, Risk_Index) %>% 
      as.matrix()
    row_clusters <- hclust(dist(heatmap_data), method = "ward.D2")
    ordered_data <- data[row_clusters$order, ]
    new_levels <- unique(ordered_data$Sector_Name)
    ordered_data <- ordered_data %>%
      mutate(Sector_Name = factor(Sector_Name, levels = new_levels))
  } else {
    ordered_data <- data
  }
  
  # Convert data to long format for ggplot (for the indices)
  heatmap_long <- ordered_data %>%
    pivot_longer(cols = c(Exposure_Index, Vulnerability_Index, Risk_Index),
                 names_to = "Index_Type", values_to = "Value") %>%
    mutate(Sector_Name = factor(Sector_Name, levels = unique(ordered_data$Sector_Name)))
  
  # Define custom color palette for the indices
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
  
  # Create and return the heatmap for the given index
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

# Function to create a heatmap for Manufacturing Share using a greyscale palette
create_manuf_share_heatmap <- function(country_code) {
  # Read and filter data for the selected country, excluding Total Manufacturing
  data <- readxl::read_xlsx("Outputs/Data/Risk_Index_Data.xlsx") %>%
    select(Country, Region_Name, Sector_Name, Manuf_Share) %>%
    filter(Country == country_code, Sector_Name != "Total Manufacturing")
  
  # Order sectors by clustering on Manuf_Share (a single variable)
  if(nrow(data) > 0) {
    heatmap_data <- as.matrix(data$Manuf_Share)
    row_clusters <- hclust(dist(heatmap_data), method = "ward.D2")
    ordered_data <- data[row_clusters$order, ]
    new_levels <- unique(ordered_data$Sector_Name)
    ordered_data <- ordered_data %>% 
      mutate(Sector_Name = factor(Sector_Name, levels = new_levels))
  } else {
    ordered_data <- data
  }
  
  # Define a greyscale palette (from light to dark)
  grey_palette <- grey.colors(10, start = 0.95, end = 0.05)
  
  # Define a common theme
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
  
  # Calculate min, median, and max for Manuf_Share
  min_val <- min(ordered_data$Manuf_Share, na.rm = TRUE)
  median_val <- median(ordered_data$Manuf_Share, na.rm = TRUE)
  max_val <- max(ordered_data$Manuf_Share, na.rm = TRUE)
  
  # Create and return the greyscale heatmap for Manufacturing Share
  p <- ggplot(ordered_data, aes(x = Region_Name, y = Sector_Name, fill = Manuf_Share)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(
      colors = grey_palette,
      values = scales::rescale(c(min_val, median_val, max_val),
                               to = c(0, 1),
                               from = range(ordered_data$Manuf_Share, na.rm = TRUE)),
      name = "Manufacturing Share",
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
# Define multiple countries
countries <- c("IT", "AT", "DE", "ES", "PL")

# Loop over each country and create/save the heatmaps
for (cc in countries) {
  
  # Create heatmaps for the indices
  risk_heatmap          <- create_heatmap(cc, "Risk_Index")
  exposure_heatmap      <- create_heatmap(cc, "Exposure_Index")
  vulnerability_heatmap <- create_heatmap(cc, "Vulnerability_Index")
  
  # Create the greyscale heatmap for Manufacturing Share
  manuf_share_heatmap <- create_manuf_share_heatmap(cc)
  
  # Print (optional)
  print(risk_heatmap)
  print(exposure_heatmap)
  print(vulnerability_heatmap)
  print(manuf_share_heatmap)
  
  # Save the heatmaps for the current country
  heatmaps <- list(
    Risk = risk_heatmap,
    Exposure = exposure_heatmap,
    Vulnerability = vulnerability_heatmap,
    Manuf_Share = manuf_share_heatmap
  )
  
  for (name in names(heatmaps)) {
    ggsave(
      filename = paste0("Outputs/Plots/", cc, "_", name, "_Heatmap.png"),
      plot = heatmaps[[name]],
      width = 10,
      height = 6,
      dpi = 800
    )
  }
}

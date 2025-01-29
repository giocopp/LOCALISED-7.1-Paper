# Load necessary libraries
libs <- c(
  "restatapi",
  "tidyverse",
  "giscoR",
  "sf",
  "classInt",
  "RColorBrewer",
  "ggnewscale",
  "readxl"
)

installed_libs <- libs %in% rownames(installed.packages())

if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs], dependencies = TRUE)
}

invisible(lapply(libs, library, character.only = TRUE))

# Read data
Risk_Index_Heat <- readxl::read_xlsx("Outputs/Data/Risk_Index_Data.xlsx") %>%
  select(Country, Region_Name, Sector_Name, Exposure_Index, Vulnerability_Index, Risk_Index)

# Filter for Austria
Risk_Index_Heat_AT <- Risk_Index_Heat %>%
  filter(Country == "AT") %>%
  select(-Country)  # Remove Country column since it is now redundant

# Convert to a matrix for clustering (keeping only numeric columns)
heatmap_data_weighted <- Risk_Index_Heat_AT %>%
  select(Exposure_Index, Vulnerability_Index, Risk_Index) %>%
  as.matrix()

# Perform hierarchical clustering on rows and columns
row_clusters_weighted <- hclust(dist(heatmap_data_weighted), method = "ward.D2")
col_clusters_weighted <- hclust(dist(t(heatmap_data_weighted)), method = "ward.D2")

# Reorder the data based on clustering
heatmap_data_weighted <- heatmap_data_weighted[row_clusters_weighted$order, col_clusters_weighted$order]

# Convert reordered matrix back to a long format for ggplot
heatmap_long_weighted <- as.data.frame(heatmap_data_weighted) %>%
  mutate(Region = Risk_Index_Heat_AT$Region_Name[row_clusters_weighted$order],
         Sector = Risk_Index_Heat_AT$Sector_Name[row_clusters_weighted$order]) %>%
  pivot_longer(cols = c(Exposure_Index, Vulnerability_Index, Risk_Index),
               names_to = "Index_Type", values_to = "Value")

# Calculate min, median, and max for Value
min_value <- min(heatmap_long_weighted$Value, na.rm = TRUE)
median_value <- median(heatmap_long_weighted$Value, na.rm = TRUE)
max_value <- max(heatmap_long_weighted$Value, na.rm = TRUE)

# Define custom color palette
full_color_palette <- c(
  "#08306b", # Deep Blue (Low Risk)
  "#2171b5", # Medium Blue
  "#6baed6", # Light Blue
  "#bdd7e7", # Pale Blue
  "#f7fbff", # White (Neutral)
  "#ffffcc", # Pale Yellow
  "#ffeda0", # Light Yellow
  "#feb24c", # Orange
  "#f03b20", # Red
  "#bd0026"  # Dark Red (High Risk)
)

# Define the heatmap with full color spectrum
risk_heatmap <- ggplot(heatmap_long_weighted, aes(x = Region, y = Sector, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = full_color_palette,
    name = "Risk Value",
    breaks = c(min_value, median_value, max_value),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(x = "Region", y = "Sector") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 0.5,
      barheight = 7,
      ticks.colour = "black",
      ticks.linewidth = 0.5
    )
  )

# Define Exposure Heatmap
exposure_heatmap <- ggplot(heatmap_long_weighted %>% filter(Index_Type == "Exposure_Index"), 
                           aes(x = Region, y = Sector, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = full_color_palette,
    name = "Exposure Value",
    breaks = c(min_value, median_value, max_value),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(x = "Region", y = "Sector") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 0.5,
      barheight = 7,
      ticks.colour = "black",
      ticks.linewidth = 0.5
    )
  )

# Define Vulnerability Heatmap
vulnerability_heatmap <- ggplot(heatmap_long_weighted %>% filter(Index_Type == "Vulnerability_Index"), 
                                aes(x = Region, y = Sector, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = full_color_palette,
    name = "Vulnerability Value",
    breaks = c(min_value, median_value, max_value),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(x = "Region", y = "Sector") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.6, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 0.5,
      barheight = 7,
      ticks.colour = "black",
      ticks.linewidth = 0.5
    )
  )

# Display the new heatmaps
print(risk_heatmap)
print(exposure_heatmap)
print(vulnerability_heatmap)

# Save the new heatmaps
ggsave("Outputs/Plots/AT_Risk_Heatmap.png", risk_heatmap, width = 10, height = 6, dpi = 800)
ggsave("Outputs/Plots/AT_Exposure_Heatmap.png", exposure_heatmap, width = 10, height = 6, dpi = 800)
ggsave("Outputs/Plots/AT_Vulnerability_Heatmap.png", vulnerability_heatmap, width = 10, height = 6, dpi = 800)


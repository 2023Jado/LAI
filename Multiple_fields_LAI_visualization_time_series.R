library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)

# Load the polygon shapefile
polygon <- vect("G:/Third_Semester/LAI/Polygon_for_mapping/Field_2_Senegal.shp")

# List all original and gapfilled LAI .tif files in the directory
orig_files <- list.files(pattern = "_orig_.*\\.tif$")
gapfilled_files <- list.files(pattern = "_int_.*\\.tif$")

# Function to extract date from the file names (assuming the pattern yyyy-mm-dd in filename)
extract_date <- function(filename) {
  date_str <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
  as.Date(date_str)
}

# Initialize empty lists to store dates, polygon names, and LAI values
dates <- c()
polygon_names <- c()
orig_lai_values <- c()
gapfilled_lai_values <- c()

# Loop through original LAI files and extract mean LAI values for each polygon
for (i in seq_along(orig_files)) {
  # Read the original and gapfilled rasters
  orig_raster <- rast(orig_files[i])
  gapfilled_raster <- rast(gapfilled_files[i])
  
  # Loop through each polygon and extract mean LAI values
  for (name in unique(polygon$Name)) {
    polygon_of_interest <- polygon[polygon$Name == name, ]
    
    # Extract mean LAI values for the polygon of interest
    orig_value <- terra::extract(orig_raster, polygon_of_interest, fun = mean, na.rm = TRUE)[,2]
    gapfilled_value <- terra::extract(gapfilled_raster, polygon_of_interest, fun = mean, na.rm = TRUE)[,2]
    
    # Append the values and corresponding date to lists
    dates <- c(dates, extract_date(orig_files[i]))
    polygon_names <- c(polygon_names, rep(name, length(orig_value)))
    orig_lai_values <- c(orig_lai_values, orig_value)
    gapfilled_lai_values <- c(gapfilled_lai_values, gapfilled_value)
  }
}

# Create a data frame for plotting
lai_data <- data.frame(
  Date = as.Date(dates),
  Name = polygon_names,
  Original_LAI = orig_lai_values,
  Gapfilled_LAI = gapfilled_lai_values
)

# Reshape the data for ggplot
lai_data_long <- lai_data %>%
  pivot_longer(cols = c("Original_LAI", "Gapfilled_LAI"), names_to = "Type", values_to = "LAI")

# Plot the time series with customizations and faceting
ggplot(lai_data_long, aes(x = Date, y = LAI, color = Type)) +
  geom_line() +
  
  # Set the x-axis limits and breaks
  scale_x_date(
    limits = as.Date(c("2022-01-01", "2023-12-31")),
    breaks = seq(as.Date("2022-01-01"), as.Date("2023-12-31"), by = "5 months"),
    labels = date_format("%Y-%m")
  ) +
  
  # Customize axis text
  theme(
    axis.title.x = element_text(size = 12, color = "black", face = "bold"),  
    axis.title.y = element_text(size = 12, color = "black", face = "bold"),  
    axis.text.x = element_text(size = 12, color = "black", angle = 45, vjust = 1, hjust = 1),  
    axis.text.y = element_text(size = 12, color = "black", angle = 90) 
  ) +
  
  # Customize legend: remove underscores and set labels
  scale_color_manual(values = c("Original_LAI" = "darkgreen", "Gapfilled_LAI" = "red"),
                     labels = c("Original LAI", "Gapfilled LAI")) +
  
  # Remove the legend title ("Type")
  guides(color = guide_legend(title = NULL)) +
  
  # Facet by polygon name
  facet_wrap(~ Name, scales = "free_y") +
  
  # Labels and theme
  labs(title = "", x = "Date", y = "Mean LAI") +
  theme_minimal()

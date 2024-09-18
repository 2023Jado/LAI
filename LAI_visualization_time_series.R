# Load necessary libraries
library(terra)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(scales)

# Set working directory 
setwd("G:/Third_Semester/LAI/28QCD/LAI-20240818T115108Z-001")

# Define the coordinates of the point of interest
# point_of_interest <- c(x = 390303, y = 1816634) 

# Load a polygon of interest (one field)
polygon <- vect("G:/Third_Semester/LAI/Polygon_for_mapping/Field_1_Senegal.shp")
plot(polygon)

# List all original and gapfilled LAI .tif files in the directory
orig_files <- list.files(pattern = "_orig_.*\\.tif$")
gapfilled_files <- list.files(pattern = "_int_.*\\.tif$")

# Function to extract date from the file names (assuming the pattern yyyy-mm-dd in filename)
extract_date <- function(filename) {
  date_str <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
  as.Date(date_str)
}

# Initialize empty lists to store dates and LAI values
dates <- c()
orig_lai_values <- c()
gapfilled_lai_values <- c()


# # Loop through original LAI files and extract values at the point of interest
# for (i in seq_along(orig_files)) {
#   # Read the original and gapfilled rasters using terra
#   orig_raster <- rast(orig_files[i])
#   gapfilled_raster <- rast(gapfilled_files[i])
#   
#   # Extract LAI values for the point of interest
#   orig_value <- terra::extract(orig_raster, point_of_interest)
#   gapfilled_value <- terra::extract(gapfilled_raster, point_of_interest)
#   
#   # Append the values and corresponding date to lists
#   dates <- c(dates, extract_date(orig_files[i]))
#   orig_lai_values <- c(orig_lai_values, orig_value)
#   gapfilled_lai_values <- c(gapfilled_lai_values, gapfilled_value)
# }

# Loop through original LAI files and extract mean LAI values for the polygon
for (i in seq_along(orig_files)) {
  # Read the original and gapfilled rasters
  orig_raster <- rast(orig_files[i])
  gapfilled_raster <- rast(gapfilled_files[i])
  
  # Extract mean LAI values for the polygon
  orig_value <- terra::extract(orig_raster, polygon, fun = mean, na.rm = TRUE)[,2]
  gapfilled_value <- terra::extract(gapfilled_raster, polygon, fun = mean, na.rm = TRUE)[,2]
  
  # Append the values and corresponding date to lists
  dates <- c(dates, extract_date(orig_files[i]))
  orig_lai_values <- c(orig_lai_values, orig_value)
  gapfilled_lai_values <- c(gapfilled_lai_values, gapfilled_value)
}

# Create a data frame for plotting
lai_data <- data.frame(
  Date = as.Date(dates),
  Original_LAI = orig_lai_values,
  Gapfilled_LAI = gapfilled_lai_values
)
# Reshape the data for ggplot
lai_data_long <- lai_data %>%
  pivot_longer(cols = c("Original_LAI", "Gapfilled_LAI"), names_to = "Type", values_to = "LAI")


# Plot the time series with customizations
ggplot(lai_data_long, aes(x = Date, y = LAI, color = Type)) +
  geom_line() +
  
  # Set the x-axis limits and breaks
  scale_x_date(
    limits = as.Date(c("2022-01-01", "2023-12-31")),
    breaks = seq(as.Date("2022-01-01"), as.Date("2023-12-31"), by = "2 months"),
    labels = date_format("%Y-%m")
  ) +
  
  # Customize axis text
  theme(
    axis.title.x = element_text(size = 12, color = "black", face = "bold"),  
    axis.title.y = element_text(size = 12, color = "black", face = "bold"),  
    axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 12, color = "black", angle = 90) 
  ) +
  # Remove the underscore and set custom labels in the legend
  scale_color_manual(values = c("Original_LAI" = "darkgreen", "Gapfilled_LAI" = "red"),
                     labels = c("Original LAI", " Max gapfilled LAI")) +
  
  # Remove the legend title ("Type")
  guides(color = guide_legend(title = NULL)) +
  
  # Labels and theme
  labs(title = "", x = "Date", y = "Mean LAI") +
  theme_minimal()

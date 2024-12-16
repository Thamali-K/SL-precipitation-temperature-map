# SL-precipitation-temperature-map
This repository contains R code for analyzing 50 years of temperature and precipitation data for Sri Lanka. The analysis combines climatic variables into a bivariate map to visualize spatial patterns and correlations

# Load libraries
library(geodata)
library(sf)
library(terra)

# Set a directory to save downloaded data
path <- tempdir()  # Temporary directory for this example 

# Load Sri Lanka boundaries (admin level 0)
sl0_spat <- gadm(country = "Sri Lanka", level = 0, path = path)

# Load Sri Lanka boundaries (admin level 1)
sl1_spat <- gadm(country = "Sri Lanka", level = 1, path = path)

# Convert SpatVector objects to sf objects
sl0 <- st_as_sf(sl0_spat)
sl1 <- st_as_sf(sl1_spat)


plot(st_geometry(sl1), border = "gray")

# Add the country boundary (administrative level 0) to the existing plot in bold
plot(st_geometry(sl0), add = TRUE, size = 2)

install.packages("climateR")
library(climateR)
library(biscale)

temp_terra = getTerraClim(AOI = sl0, 
                          varname = c("tmax", "tmin"),
                          startDate = "1973-01-01",
                          endDate  = "2023-12-01")
install.packages("remotes")

remotes::install_github("mikejohnson51/climateR")

library(climateR)
library(biscale)

temp_terra = getTerraClim(AOI = sl0, 
                          varname = c("tmax", "tmin"),
                          startDate = "1973-01-01",
                          endDate  = "2023-12-01")

mean_temp_monthly <- (temp_terra$tmax + temp_terra$tmin) / 2


# Calculate the annual mean temperature by averaging the monthly means across all months
annual_mean_temp <- mean(mean_temp_monthly, na.rm = TRUE)

# Plot the annual mean temperature
plot(annual_mean_temp)


new_res <- 0.025  # Set the new resolution
new_raster <- rast(ext(annual_mean_temp), resolution = new_res, crs = crs(annual_mean_temp))
annual_mean_temp <- resample(x = annual_mean_temp, y = new_raster, method="bilinear")

# Crop the resampled temperature data to match the boundaries of the United Kingdom
sl_temp <- terra::crop(annual_mean_temp, y = sl0, mask = TRUE)

# Plot the cropped annual mean temperature for the United Kingdom
plot(sl_temp)


ppt_terra = getTerraClim(AOI = sl0, 
                         varname = "ppt",
                         startDate = "1973-01-01",
                         endDate  = "2023-12-01")

annual_mean_ppt <- mean(ppt_terra$ppt, na.rm = TRUE)

new_raster <- rast(ext(annual_mean_ppt), resolution = new_res, crs = crs(annual_mean_ppt))
annual_mean_ppt <- resample(x = annual_mean_ppt, y = new_raster, method = "bilinear")

# Crop the resampled precipitation data to match the boundaries of the United Kingdom
sl_ppt <- terra::crop(annual_mean_ppt, y = sl0, mask = TRUE)

# Plot the cropped annual mean precipitation for the United Kingdom
plot(sl_ppt)

# Combine temperature and precipitation rasters into a single raster stack
temp_ppt <- c(sl_temp, sl_ppt)

# Assign descriptive names to each raster layer in the stack
names(temp_ppt) <- c("temp", "ppt")

temp_ppt_df <- temp_ppt |> 
  project(sl0) |> 
  as.data.frame(xy = TRUE)

# Display the first few rows of the resulting data frame to verify the data
head(temp_ppt_df)

print(temp_ppt_df)

data <- bi_class(temp_ppt_df,
                 x = ppt, 
                 y = temp, 
                 style = "quantile", dim = 4)

library(tidyverse)
data |> 
  count(bi_class) |> 
  ggplot(aes(x = bi_class, y = n)) +
  geom_col() +  # Create a bar plot to show the count of each bivariate class
  labs(title = "Distribution of Bivariate Classes", x = "Bivariate Class", y = "Frequency")

pallet <- "Bluegill"

# Create the bivariate map using ggplot2
map <- ggplot() +
  theme_void(base_size = 14) +  # Set a minimal theme for the map
  xlim(79.5, 82.3) +  # Set the x-axis limits for the map (longitude range)
  ylim(5.8,10.0 ) +  # Set the y-axis limits for the map (latitude range)
  # Plot the bivariate raster data with appropriate fill color based on bivariate classes
  geom_raster(data = data, mapping = aes(x = x, y = y, fill = bi_class), color = NA, linewidth = 0.1, show.legend = FALSE) +
  # Apply the bivariate color scale using the selected palette and dimensions
  bi_scale_fill(pal = pallet, dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  # Overlay the first administrative level boundaries of the United Kingdom
  geom_sf(data = sl1, fill = NA, color = "black", linewidth = 0.20) +
  # Overlay the country-level boundary of the United Kingdom
  geom_sf(data = sl0, fill = NA, color = "black", linewidth = 0.40) +
  # Add labels for the map
  labs(title = "Sri Lanka: Temperature and Precipitation Patterns", 
       subtitle = "Mean temperature and precipitation patterns based on 50 years of data.",
       caption = "Source: Terra Climate Data     Author: Thamali Kariyawasam") +
  # Customize the appearance of the title, subtitle, and caption
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size = 10, face = "bold", hjust = 7))

# Create the legend for the bivariate map
legend <- bi_legend(pal = pallet,   
                    flip_axes = FALSE,
                    rotate_pal = FALSE,
                    dim = 4,
                    xlab = "Precipitation (mm)",
                    ylab = "Temperature (C)",
                    size = 10)

# Combine the map and legend using cowplot
library(cowplot)
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +  # Draw the main map plot
  draw_plot(legend, 0.05, 0.05, 0.28, 0.28)  # Draw the legend in the specified position

# Display the final map with legend
finalPlot

library(biscale)
?bi_pal  # Shows the help file with all available palettes
bi_palettes_preview() 

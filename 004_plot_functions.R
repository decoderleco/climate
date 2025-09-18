library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(magick)
library(sf)

plot_french_stations <- function(df, lat_col = latitude, lon_col = longitude, title = "Stations météo") {
  
  # get the map of France
  france <- ne_countries(scale = "medium", country = "France", returnclass = "sf")
  
  ggplot() +
    geom_sf(data = france, fill = "lightgrey", color = "black") +
    geom_point(data = df, aes(x = {{lon_col}}, y = {{lat_col}}),
               color = "red", alpha = 0.7, size = 2) +
    coord_sf(xlim = c(-5, 10), ylim = c(41, 51), expand = FALSE) +  # France's limits
    labs(title = title, x = "Longitude", y = "Latitude") +
    theme_minimal()
}


plot_interval_map <- function(data, start, end) {
  df <- data %>% filter(start_year == start, end_year == end)
  
  # get the map of France
  france <- ne_countries(scale = "medium", country = "France", returnclass = "sf")
  
  # get bounding box of mainland France only (continental + Corsica)
  france_met <- france %>% 
    st_crop(xmin = -5, xmax = 10, ymin = 41, ymax = 51)  # optional, or compute st_bbox(france) for automatic bbox
  bbox <- st_bbox(france_met)
  
  ggplot() +
    geom_sf(data = france_met, fill = "white", color = "black") +  # mainland France
    geom_point(
      data = df,
      aes(x = longitude, y = latitude, color = trend),
      size = 2, alpha = 0.8
    ) +
    scale_color_manual(values = c(
      "increase" = "red",
      "neutral"  = "grey",
      "decrease" = "blue"
    )) +
    labs(
      title = paste0("Temperature trend ", start, "-", end),
      color = "Trend"
    ) +
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE
    ) +
    theme_minimal()
}


# Function to create a GIF from PNGs with the same prefix
create_gif_from_pngs <- function(png_dir, gif_dir, prefix, gif_name = "animation.gif", fps = 1) {
  # Ensure output folder exists
  dir.create(gif_dir, showWarnings = FALSE, recursive = TRUE)
  
  # List PNG files with the given prefix
  png_files <- list.files(png_dir, pattern = paste0("^", prefix, ".*\\.png$"), full.names = TRUE)
  png_files <- sort(png_files)  # sort to maintain correct order
  
  if(length(png_files) == 0) {
    stop("No PNG files found with prefix: ", prefix)
  }
  
  # Read PNGs and create animation
  images <- image_read(png_files)
  animation <- image_animate(images, fps = fps)
  
  # Save GIF
  gif_file <- file.path(gif_dir, gif_name)
  image_write(animation, gif_file)
  
  message("✅ GIF created at: ", gif_file)
  return(gif_file)
}
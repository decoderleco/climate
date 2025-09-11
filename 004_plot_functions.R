library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

plot_french_stations <- function(df, lat_col = latitude, lon_col = longitude, title = "Stations météo") {
  
  # récupérer la carte de France
  france <- ne_countries(scale = "medium", country = "France", returnclass = "sf")
  
  ggplot() +
    geom_sf(data = france, fill = "lightgrey", color = "black") +
    geom_point(data = df, aes(x = {{lon_col}}, y = {{lat_col}}),
               color = "red", alpha = 0.7, size = 2) +
    coord_sf(xlim = c(-5, 10), ylim = c(41, 51), expand = FALSE) +  # limites approximatives de la France
    labs(title = title, x = "Longitude", y = "Latitude") +
    theme_minimal()
}
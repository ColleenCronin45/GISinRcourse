# Data loading script for Holiday Joe

# setup -------------------------------------------------------------------

library(tidyverse)

# File path to shapefiles:

shapefile_paths <-
  list.files(
    'data/raw/holiday_joe_data',
    pattern = 'geojson$')

# File path to rasters:

raster_paths <-
  list.files(
    'data/raw/holiday_joe_data',
    pattern = 'tif$')

# read in data ------------------------------------------------------------

# Point files:

shapefile_paths %>% 
  file.path('data/raw/holiday_joe_data', .) %>% 
  purrr::map(
    ~ sf::st_read(.x) %>% 
      sf::st_transform(crs = 5070)) %>% 
  set_names(
    str_remove(shapefile_paths, '.geojson')) %>% 
  list2env(.GlobalEnv)
  
# Rasters:

rasters_start <- 
  raster_paths %>% 
  file.path('data/raw/holiday_joe_data', .) %>% 
  purrr::map(
    ~ terra::rast(.x)) %>%
  set_names('canopy',
            'dem',
            'precip',
            'tmin')
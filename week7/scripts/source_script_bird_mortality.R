# Source script for bird mortality analysis

library(tidyverse)

# A single non-spatial file:

nlcd_key <- 
  read_rds('data/processed/birds_cicadas_lc.rds') %>% 
  pluck('nlcd_key')

# load polygon data --------------------------------------------------------

list(
  
  # Polygon shapefile of US Census data for Washington DC:
  
  census =
    sf::st_read('data/raw/shapefiles/dc_census.geojson') %>% 
    select(GEOID, INCOME, POPULATION),
  
  # Multipolygon shapefiles for DC National Park Service parks:
  
  nps = 
    sf::st_read('data/raw/shapefiles/dc_national_parks.geojson'),
  
  # Multipolygon shapefiles for DC city parks:
  
  parks_and_rec = 
    sf::st_read('data/raw/shapefiles/dc_parks_and_recreation.geojson')) %>% 
  
  # Pre-processing:
  
  purrr::map(
    ~ .x %>% 
      set_names(
        names(.) %>% 
          tolower()) %>% 
      
      # Transform to EPSG 5070:
      
      sf::st_transform(crs = 5070) %>% 
      
      # Fix invalid geometries:
      
      sf::st_make_valid()) %>% 
  
  # Send to the global environment:
  
  list2env(.GlobalEnv)

# point data --------------------------------------------------------------

list(
  
  # iNaturalist observations of cicada:
  
  cicadas = 
    read_csv('data/raw/cicadas_brood_x_2021.csv') %>% 
    select(datetime, longitude, latitude) %>% 
    filter(lubridate::month(datetime) == 5) %>% 
    sf::st_as_sf(
      coords = c('longitude', 'latitude'),
      crs = 4326),
  
  # Location of sick birds from DC's wildlife rehab center:
  
  birds = 
    sf::st_read('data/raw/shapefiles/sick_birds.geojson')) %>% 
  
  # Process the shapes:
  
  purrr::map(
    ~ .x %>%
      
      # Transform to the same CRS as the polygons:
      
      sf::st_transform(crs = st_crs(census)) %>% 
      
      # Filter points to the Washington DC region:
      
      sf::st_filter(census)) %>% 
  
  # Send to the global environment:
  
  list2env(.GlobalEnv)

# raster data -------------------------------------------------------------

rasters <-
  list.files(
    'data/raw/rasters',
    pattern = 'tif',
    full.names = TRUE) %>%
  purrr::map(
    ~ terra::rast(.x) %>% 
      terra::crop(census) %>% 
      terra::mask(census)) %>%
  set_names('canopy', 'imp', 'nlcd') %>%
  terra::rast()

# Canopy has a different datum!

rasters$canopy <-
  terra::project(rasters$canopy,
                 rasters$imp)

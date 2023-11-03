# Source script for problem set 6

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
  
  map(
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

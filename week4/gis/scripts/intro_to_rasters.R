# Introduction to rasters

# setup -------------------------------------------------------------------

library(tmap)
library(sf)
library(tidyverse)

tmap_mode('view')

# load polygons -----------------------------------------------------------

# Polygon files:

polygons <-
  list(
    
    # Polygon shapefile of US Census data for Washington DC:
    
    census =
      st_read('data/raw/shapefiles/dc_census.geojson') %>% 
      select(GEOID, INCOME, POPULATION),
    
    # Multipolygon shapefile for Rock Creek Park:
    
    rock_creek = 
      st_read('data/raw/shapefiles/rock_creek_park.geojson'),
    
    # Polygon shapefile of DC waterbodies:
    
    water = 
      st_read('data/raw/shapefiles/Waterbodies_2019.geojson')) %>% 
  
  # Pre-processing:
  
  map(
    ~ .x %>% 
      set_names(
        names(.) %>% 
          tolower()) %>% 
      
      # EPSG 32618 is UTM zone 18N for Washington DC:
      
      st_transform(crs = 32618) %>% 
      
      # Fix invalid geometries:
      
      st_make_valid())
 

# Dissolve inner borders from polygons:

unionized_polygons <-
  polygons %>%
  map(
    ~ st_union(.x) %>% 
      st_sf())

# Remove water from census shape:

census_no_water <-
  polygons$census %>% 
  st_difference(
    st_union(polygons$water))

# load points -------------------------------------------------------------

points <-
  list(
    
    # iNaturalist observations of cicada:
    
    cicadas = 
      read_csv('data/raw/cicadas_brood_x_2021.csv') %>% 
      select(datetime, longitude, latitude) %>% 
      filter(lubridate::month(datetime) == 5) %>% 
      st_as_sf(
        coords = c('longitude', 'latitude'),
        crs = 4326),
    
    # Location of sick birds from DC's wildlife rehab center:
    
    birds = 
      st_read('data/raw/shapefiles/sick_birds.geojson')) %>% 
  
  # Process the shapes:
  
  map(
    ~ .x %>%
      
      # Transform to the same CRS as the polygons:
      
      st_transform(crs = st_crs(polygons$census)) %>% 
      
      # Filter points to the Washington DC region:
      
      st_filter(polygons$census))

# load rasters ------------------------------------------------------------

imp <-
  terra::rast('data/raw/rasters/impervious_surface.tif')

plot(imp)

# Now you! Complete the code below to read in all of the rasters at once:

rasters <-
  list(
    canopy = 'data/raw/rasters/canopy_cover.tif',
    imp = 'data/raw/rasters/impervious_surface.tif',
    nlcd = 'data/raw/rasters/nlcd.tif')

# the SpatVector ----------------------------------------------------------

# Convert an sf object to a SpatVector:

points$birds %>% 
  terra::vect()

# pre-processing ----------------------------------------------------------

rasters$can %>% 
  terra::plot()

# Now you! Use temp, the rasters list, and purrr::map() to crop and mask all
# rasters to the DC shapefile:

rasters

# tmap --------------------------------------------------------------------

# Basemap:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # Canopy cover:
  
  tm_shape(rasters_prj$canopy) +
  tm_raster()

# extracting data from rasters --------------------------------------------

# Global summary statistic, mean:

rasters$imp %>% 
  terra::global(mean)

# Mean impervious surface by census tract:

polygons$census %>% 
  st_transform(crs = 5070) %>% 
  terra::vect() %>% 
  terra::extract(
    rasters$imp,
    .,
    mean,
    na.rm = TRUE)

# Now you! Add a field to polygons$census that represents the proportion of
# impervious surface in each polygon:

polygons$census

# Extract impervious surface to points:

points$birds %>% 
  st_transform(crs = 5070) %>% 
  terra::vect() %>% 
  terra::extract(rasters$imp, .) %>% 
  pull(imp)

# Now you! Add a field to points$birds that represents the proportion of
# impervious surface within 500 m of each location:

points$birds %>% 
  st_transform(crs = 5070) %>% 
  terra::vect()
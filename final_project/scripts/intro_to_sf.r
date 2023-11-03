# Introduction to SF

# This script provides an introduction into how to use simple features shape
# files in R with the package sf. Topics covered include, reading, plotting,
# subsetting, mutating, and writing shape files.

# setup -------------------------------------------------------------------

library(sf)
library(tidyverse)

# read in the shapefiles --------------------------------------------------

# Get sites data:

sites <- 
  read_csv('C:/Users/User/Documents/Week1/gis/gis/data/raw/sites.csv') %>% 
  st_as_sf(
    coords = c('lon', 'lat'),
    crs = 4326) %>% 
  st_transform(crs = 5070)

st_crs(sites)

# States:

states <-
  st_read('C:/Users/User/Documents/Week1/gis/gis/data/raw/shapefiles/states.shp') %>% 
  st_transform(
    st_crs(sites)) %>% 
  set_names(
    names(.) %>% 
      tolower()
  )
  

# DC census data (now you):

dc_census <-
  st_read('C:/Users/User/Documents/Week1/gis/gis/data/raw/shapefiles/dc_census.shp')%>% 
  st_transform(
    st_crs(sites)) %>% 
    set_names(
      names(.) %>% 
        tolower()
    )
    
# simple maps -------------------------------------------------------------

# Plot states:

states %>% 
  ggplot() +
  geom_sf() +
  theme_void()

# Plot census data (now you):

dc_census %>% 
  ggplot() +
  geom_sf() +
  theme_void()

st_is_longlat(states)

class(states)

# check column names

states %>% names()
  
# subsetting --------------------------------------------------------------

states %>% 
  filter(region == 'South') %>% 
  ggplot() +
  geom_sf() +
  geom_sf(
    data =
      states %>% 
      filter(name %in% c('Maryland', 'Virginia')),
    fill = 'red') + 
  theme_void()

# Now you! Plot dc_census such that census tracts with a population of 0 are
# filled with the color red and all other tracts are gray:

dc_census %>% 
  ggplot() +
  geom_sf() +
  geom_sf(
    data =
      dc_census %>% 
      filter(population == 0),
    fill = 'red') +
  theme_void()

# mutation ----------------------------------------------------------------

states %>% 
  filter(region == 'Norteast') %>%
  mutate(pop_density = ttl__15/area) %>% 
  ggplot() +
  geom_sf(aes(fill = pop_density)) +
  theme_void()

# Now you! Plot the population density per unit land area in Washington DC:

dc_census %>% 
  transmute(
    aland = aland/1000000,
    pop_density = population/aland) %>% 
  ggplot() +
  geom_sf(aes(fill = pop_density)) +
  theme_void()

# saving sf files ---------------------------------------------------------

# Save as in ESRI-shapefile format:

sites %>% 
  st_write('C:/Users/User/Documents/Week1/gis/gis/data/raw/shapefiles/sites.shp')

# Save as a geojson file:

states %>% 
  st_write('C:/Users/User/Documents/Week1/gis/gis/data/processed/states.geojson')

# Save as an rds file:

dc_census %>% 
  write_rds('C:/Users/User/Documents/Week1/gis/gis/data/processed/dc_census.rds')

# Save as a kml file:

sites %>% 
  st_write(('C:/Users/User/Documents/Week1/gis/gis/data/processed/sites.kml'))

# Note: To read these files (not run):

# st_read('data/raw/shapefiles/sites.shp')

# st_read('data/processed/states.geojson')

# read_rds('data/processed/dc_census.rds')

# st_read('data/processed/sites.kml')

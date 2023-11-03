# Advanced sf: Geometry operations

# setup -------------------------------------------------------------------

library(tmap)
library(sf)
library(tidyverse)

# Set the tmap mode for the entire document:

tmap_mode('view')

# load polygons -----------------------------------------------------------

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
  map( ~ .x %>% 
    st_union() %>% 
      st_sf())

# Remove water from census shape:

census_no_water <-
  polygons$census %>% 
  st_difference(unionized_polygons$water)


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
  
  # process teh shapes:
  map(
    ~ .x %>% 
  # transform to the same CRS as the polygons:
      
      st_transform(crs = st_crs(polygons$census)) %>% 
  
    # filter points to the Washington DC region:
 
    st_filter(polygons$census))

# Now you! Modify the script above such that the point data only include
# observations in Washington DC.

# invalid polygons --------------------------------------------------------

# Basemap:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # Add census layer:
  
  polygons$census %>%
  tm_shape(name = 'Census tracts') +
  tm_polygons(alpha = 0.6) +
  
  # checking for reason the polygon is invalid:
  
  # polygons$water %>%
  # filter(
  #   !st_is_valid(.)) %>% 
  # st_is_valid(reason = TRUE)
  
  # Add water layer:
  
  polygons$water %>%
  st_make_valid() %>%  
  tm_shape(name = 'Water bodies') +
  tm_polygons(col = 'blue')

# cicadas and sick birds --------------------------------------------------

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # Add cicada observations:
  
  points$cicadas %>%
  tm_shape(name = 'Cicadas') +
  tm_markers() +
  
  # Add locations of sick birds:
  
  points$birds %>% 
  tm_shape(name = 'Sick birds') +
  tm_dots(col = 'red',
          size = 0.05)

# Now you! Use purrr::map to calculate the number of sick birds and cicadas per
# census block:

counts_by_census <-
  points %>%
  map(
    ~ st_join(census_no_water, .x) %>%
      as_tibble() %>%
      group_by(geoid) %>%
      summarize(n = n()) %>%
      left_join(census_no_water, .) %>%
      mutate(area =
          st_area(.) %>%
          units::set_units("km^2"),
        density = n / area))

# Basemap:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # Cicadas:
  
  counts_by_census$cicadas %>% 
  tm_shape(name = 'Cicadas by census tract') +
  tm_polygons(
    title = 'Cicadas',
    style = 'kmeans',
    col = 'density',
    palette = 'viridis') +
  
  # Birds:
  
  counts_by_census$birds %>% 
  tm_shape(name = 'Bird mortality by census tract') +
  tm_polygons(
    title = 'Sick birds',
    style = 'kmeans',
    col = 'density',
    palette = 'YlOrRd',
    n = 5) +
  
  polygons$water %>%
  st_make_valid() %>%  
  tm_shape(name = 'Water bodies') +
  tm_polygons(col = 'blue')


# area calculations -------------------------------------------------------

# Calculate cicadas per square km:

counts_by_census$cicadas %>% 
  mutate(area = 
  st_area(.) %>% 
  units::set_units("km^2"),
  density = n/area)

# Modify the code block above such that area is added as a column in the
# counts_by_census$cicadas.

# Now you! Modify the counts_by_census map function such that it calculates the
# density of cicadas and bird mortalities (per km^2) by census tract.

# unions and differences --------------------------------------------------

# Union - District of Columbia

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  polygons$census %>% 
  st_difference(unionized_polygons$water) %>% 
  tm_shape() +
  tm_polygons(alpha = 0.7) +
  
  polygons$rock_creek %>% 
  tm_shape() +
  tm_polygons(col = '#228b22') +
  
  polygons$water %>% 
  tm_shape() +
  tm_polygons(col = 'blue')

# Difference - census files

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  polygons$census %>% 
  tm_shape() +
  tm_polygons(alpha = 0.7)


# centroids ---------------------------------------------------------------

# Basemap:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  # Cicadas:
  
  counts_by_census$cicadas %>% 
  tm_shape(name = 'Cicadas by census tract') +
  tm_polygons(
    title = 'Cicadas',
    style = 'quantile',
    col = 'density',
    palette = 'viridis') +
  
  # Birds:
  
  counts_by_census$birds %>% 
  st_centroid() %>% 
  tm_shape(name = 'Bird mortality by census tract') +
  tm_dots(
    title = 'Sick birds',
    style = 'quantile',
    col = 'density',
    palette = 'YlOrRd',
    n = 5,
    size = 0.1)

# buffers and intersections -----------------------------------------------

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  unionized_polygons$rock_creek %>%
  st_buffer(dist = 500) %>% 
  st_intersection(unionized_polygons$census) %>% 
  tm_shape() +
  tm_polygons(col = 'yellow') +
  
  unionized_polygons$rock_creek %>% 
  tm_shape() +
  tm_polygons(col = '#228b22') +
  
  points$birds %>% 
  tm_shape(name = 'Sick birds') +
  tm_dots(col = 'red',
          size = 0.05)

# distance calculations ---------------------------------------------------

# Distance to park:

points$cicadas %>% 
  mutate(
    distance_to_park =
      st_distance(
        .,
        unionized_polygons$rock_creek) %>% 
      as.vector())

# could also have used as.numeric() above instead pf as.vector


# Add distance to Rock Creek Park to points$cicadas as a vector called
# distance_to_park:

points$cicadas %>% 
  mutate(
    distance_to_park =
      st_distance(
        .,
        unionized_polygons$rock_creek) %>% 
      as.vector())

# Now you! Modify the tmap below such that cicada points are colored by
# distance-from-park:

tm_basemap(
  c('Esri.WorldTopoMap',
    'OpenStreetMap',
    'Esri.WorldImagery')) +
  
  unionized_polygons$rock_creek %>%
  tm_shape() +
  tm_polygons(col = '#228b22') +
  
  points$cicadas %>% 
  mutate(
    distance_to_park = 
      st_distance(
        .,
        unionized_polygons$rock_creek) %>% 
        as.vector()) %>% 
  tm_shape() +
  tm_dots(col = "distance_to_park",
          style = "kmeans")

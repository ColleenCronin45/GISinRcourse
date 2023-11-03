
# setup -------------------------------------------------------------------

library(sf)
library(tidyverse)

source("C:/Users/User/Documents/Week1/gis/gis/scripts/source_script.R")


# read and pre-process data -----------------------------------------------

# census data:

census <-
  st_read("data/raw/shapefiles/census.shp",
          quiet = TRUE) %>% 
  clean_names() %>% 
  
  # filtering for just DC:
  
  filter(state_name == "DC") %>% 
  
  #select columns of interest:
  select(geoid, aland, population)

# Counties:

counties <-
  st_read("data/raw/shapefiles/counties.geojson",
          quiet = TRUE) %>% 
  clean_names() %>% 
  
  # removing all fields because no longer needed:
  
  select(!everything())

# Bike share locations:

dc_bikes <-
  read_csv("data/raw/capital_bike_share_locations.csv") %>% 
  clean_names() %>% 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326) %>% 
  
  #selecting columns of interest:
  
  select(objectid, name, region_name) %>% 
  # changing crs:
  
  st_transform(
    st_crs(census))


# data analysis -----------------------------------------------------------


# change field names:

names(census)

# rather than using set names and tolower, the source script gives us a function to do this:

census %>% 
  clean_names() %>% 
  
  # Extracting just the names for illustration purposes:
  
  names()

# check which states are present in the census data:

census %>% 
  pull(state_name) %>% 
  unique()

# check rows in census:

nrow(census)

# remove other columns that refer to the state since no longer needed:

census %>% 
  select(!c(statefp, state_name))

# checking number of unique counties:

census %>% 
  pull(countyfp) %>% 
  unique()

# remocing that variable as well:

census %>% 
  select(!c(statefp, countyfp, state_name))

# selecting columns of interest (the geometry column auto-stays):

census %>% 
  select(geoid, aland, population)

# removing all fields in counties because we were only left with 1 feature anyway:

counties %>% 
  select(!everything())

# selecting columns of interest in dc_bikes:

dc_bikes %>% 
  select(objectid, name, region_name)

# view crs of census:

st_crs(census)

# extract epsg code:

st_crs(census)$srid

st_crs(counties)$srid

st_crs(dc_bikes)$srid

# looking at rows:

nrow(census)

nrow(counties)

nrow(dc_bikes)

# dc bikes has more features, but it is a point object, so let's print the geometry and crs:

st_geometry(dc_bikes)

# conversely, census and couties are multipolygons:

st_geometry(census)

st_geometry(counties)

# converting polygons to points to determine number of point features:

census %>% 
  st_cast("POINT") %>%
  nrow()

counties %>% 
  st_cast("POINT") %>%
  nrow()

# census has most vertices/features and thus should be the model crs to use

# changing the dc_bikes crs: 

dc_bikes %>% 
  st_transform(crs = 4269)

# another way:

dc_bikes %>% 
  st_transform(
    st_crs(census))

# to find your UTM zone - add 180 to your target longitude, dive by 6 and round up to nearest whole number:

my_utm_zone <-
  function(lon) {
    ceiling(
      (180 + lon)/6)
  }

# the longitudinal center of DC is -77°, so the crs is:

my_utm_zone(-77)

# getting the EPSG code associated with our UTM:

lonlat_to_utm(c(-77, 38))

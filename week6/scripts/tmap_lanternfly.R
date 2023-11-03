# Introduction to tmap

# setup -------------------------------------------------------------------

library(lubridate)
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)

# Assign a read directory:

read_dir <-
  'data/processed/lanternfly_tmap'

# Load iNaturalist observations of spotted lanternflies and convert to EPSG 5070
# (Note:  These data were recorded in EPSG 4326):

spotted_lanternfly <-
  read_dir %>% 
  file.path('lanternfly_proc.rds') %>% 
  read_rds() %>% 
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326) %>% 
  st_transform(crs = 5070)

# Now you! Use a map function to read in the geojson data within our read
# directory and save the object as a list object named “shapes” with the list
# items named “states” and “counties”.

shapes <-
  list.files(
    read_dir,
    pattern = "geojson",
    full.names = TRUE) %>% 
  map(
    ~ st_read(.x)) %>% 
  set_names("counties", "states")


# Now you! Modify the map statement above such that it also subsets the data to
# polygons that contain spotted lanternfly observations.

shapes <-
  list.files(
    read_dir,
    pattern = "geojson",
    full.names = TRUE) %>% 
  map(
    ~ st_read(.x) %>%
  st_filter(spotted_lanternfly)) %>% 
  set_names("counties", "states")


# Now you! Working in the same code block, add a second map statement that
# calculates the total number of spotted lanternfly observations in each
# polygon and returns a list of resultant shapefiles.

shapes <-
  list.files(
    read_dir,
    pattern = "geojson",
    full.names = TRUE) %>% 
  map(
    ~ st_read(.x) %>%
      st_filter(spotted_lanternfly)) %>% 
  set_names("counties", "states") %>% 
  map(
    ~ spotted_lanternfly %>% 
      st_join(.x %>% 
              select(geoid)) %>% 
      as_tibble() %>% 
      group_by(geoid) %>% 
      summarize(n = n()) %>% 
      left_join(
        .x,
        .,
        by = "geoid"))
  

# tmap --------------------------------------------------------------------

# Set the tmap mode:

tmap_mode('plot')

# Generate a choropleth map with tmap:

tm_shape(shapes$states) + 
  tm_polygons(
    col = 'n',
    title = "Observations")

# Now you! Generate a choropleth map of counties where the fill color of
# counties is determined by the number of lanternfly observations:

# Add states as background:

tm_shape(shapes$states) +
  (tm_polygons()) +
  
  # Add counties with count by observations:
  
tm_shape(shapes$counties) + 
  tm_polygons(
    col = 'n',
    
    # Change classification scheme:
    style = 'fixed',
    breaks = 
      c(1,
        50,
        100,
        500,
        1000,
        1500),
    palette = "YlOrRd",
    n = 5,
    contrast = c(0.2, 0.9),
    title = "Observations") +
  
  # Map layout:
  
  tm_layout(
    main.title = "Spotted lanternfly distribution",
    frame = "#999999",
    legend.outside = TRUE)

# Installing packages to use palette explorer:

install.packages("shiny")
install.packages("shinyjs")
library(shiny)
library(shinyjs)

palette_explorer()

# interactive maps --------------------------------------------------------

# Now you! Add spotted lanternfly observations to the map with the function
# tm_dots:

# change the mode to "view" making an interactive map, an interface to the R package leaflet, which is an interface to the Java package leaflet:

tmap_mode('view')

# Add basemaps:

tm_basemap(
  c("OpenStreetMap",
    "Esri.WorldImagery")
) +
tm_shape(shapes$counties,
         name = "Counties") + 
  tm_polygons(
    col = 'n',
    style = 'fixed',
    breaks = 
      c(1,
        50,
        100,
        500,
        1000,
        1500),
    palette = "YlOrRd",
    n = 5,
    contrast = c(0.2, 0.9),
    title = "Observations") +
  
  # Add observations as dots:
  
  tm_shape(spotted_lanternfly,
           name = "Observations") +
  tm_dots(clustering = TRUE) 
  

# gifs with tmap ----------------------------------------------------------

tmap_mode('plot')

# Add states as a background layer:

tm_shape(shapes$states) +
  tm_polygons() +
  
  # Subset lanternfly observations to 2019:
  
  spotted_lanternfly %>% 
  filter(year(datetime) == 2019) %>% 
  
  # Join with counties:
  
  st_join(
    shapes$counties %>% 
      select(geoid)) %>% 
  
  # Calculate the number of lanternfly observations per county:
  
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarize(n_lanternflies = n()) %>% 
  
  # Convert to a shapefile with only counties with observations:
  
  inner_join(
    shapes$counties %>% 
      st_filter(spotted_lanternfly),
    .,
    by = 'geoid') %>% 
  
  # Add to plot:
  
  tm_shape() +
  tm_polygons(
    col = 'n_lanternflies',
    style = 'fixed',
    breaks = 
      c(1, 
        50,
        100, 
        500, 
        1000, 
        1500),
    palette = "YlOrRd",
    n = 5, 
    contrast = c(0.2, 0.9),
    title = 'Observations') +
  
  # Map layout:
  
  tm_layout(
    main.title = 
      paste('Spotted lanternfly distribution ', 2019),
    frame = '#999999',
    legend.outside = TRUE)

# Using the code above and purrr map function, generate a list of tmaps that displays the distribution of lanternflies by county and year from 2015-2022:

my_list <-
  map(
    2015:2022,
    ~ tm_shape(shapes$states) +
      tm_polygons() +
      spotted_lanternfly %>% 
      filter(year(datetime) == .x) %>% 
      st_join(
        shapes$counties %>% 
          select(geoid)) %>% 
      as_tibble() %>% 
      group_by(geoid) %>% 
      summarize(n_lanternflies = n()) %>% 
      inner_join(
        shapes$counties %>% 
          st_filter(spotted_lanternfly),
        .,
        by = 'geoid') %>% 
      tm_shape() +
      tm_polygons(
        col = 'n_lanternflies',
        style = 'fixed',
        breaks = 
          c(1, 
            50,
            100, 
            500, 
            1000, 
            1500),
        palette = "YlOrRd",
        n = 5, 
        contrast = c(0.2, 0.9),
        title = 'Observations') +
      tm_layout(
        main.title = 
          paste('Spotted lanternfly distribution ', .x),
        frame = '#999999',
        legend.outside = TRUE))

# Animate map:

tmap_animation(
  my_list,
  width = 900,
  height = 700,
  fps = 1
)


# setup -------------------------------------------------------------------

library(sf)
library(tidyverse)

states <-
  st_read("data/raw/shapefiles/states.shp") %>% 
  filter(REGION %in% c("Norteast", "South"))

# using quiet to reduce info in output

states <-
  st_read(
    "data/raw/shapefiles/states.shp",
    quiet = TRUE) %>% 
  filter(REGION %in% c("Norteast", "South"))

# setting the theme

theme_set(
  new = theme_void())

# create an sfg for a location

c(-90.0216, 33.65646) %>% 
  st_point() %>% 
  class()

#transform an sfg to an sfc

c(-90.0216, 33.65646) %>% 
  st_point() %>% 
  st_sfc()

# adding the coordinate system:

c(-90.0216, 33.65646) %>% 
  st_point() %>% 
  st_sfc(crs = 4269)

# mapping

states %>% 
  ggplot() +
  
  # Map state layer:
  
  geom_sf() + 
  
  # Map lines:
  
  geom_sf(
    data = 
      c(-90.0216, 33.65646) %>% 
      st_point() %>% 
      st_sfc(crs = 4269),
    size = 3)

# convert sfc to an sf object:

c(-90.0216, 33.65646) %>% 
  st_point() %>% 
  st_sfc(crs = 4269) %>% 
  st_sf()

#  the class of the object is an sf file and a data frame:

c(-90.0216, 33.65646) %>% 
  st_point() %>% 
  st_sfc(crs = 4269) %>% 
  st_sf() %>% 
  class()

# add column landmark:

c(-90.0216, 33.65646) %>% 
  st_point() %>% 
  st_sfc(crs = 4269) %>% 
  st_sf() %>% 
  mutate(Landmark = "Avalon")

#plot output:

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(
    data = c(-90.0216, 33.65646) %>% 
      st_point() %>% 
      st_sfc(crs = 4269) %>% 
      st_sf() %>% 
      mutate(Landmark = "Avalon"),
    aes(color = Landmark),
    size = 3)

# creating a dataframe with a tibble:

tibble(
  Landmark = 
    c("Avalon",
      "Train station",
      "Smithsonian-Mason",
      "Gaslight Cafe",
      "Newport"),
  x = 
    c(-90.0216, 
      -79.07005, 
      -78.16464,
      -74.00044, 
      -71.33749),
  y = 
    c(33.65646,
      35.91097,
      38.88697,
      40.72979, 
      41.47934))

# creating a dataframe with a tribble:

landmarks_df <-
  tribble(
    ~ Landmark,         ~ x,        ~ y,
    "Avalon",            -90.02160, 33.65646,
    "Train station",     -79.07005, 35.91097,
    "Smithsonian-Mason", -78.16464, 38.88697,
    "Gaslight Cafe",     -74.00044, 40.72979,
    "Newport",           -71.33749, 41.47934) 

# POINTS

tibble(
  Landmark = "Avalon",
  x = -90.0216,
  y = 33.65646) %>% 
  st_as_sf(
    coords = c("x", "y"),
    crs = 4269)

landmarks <- 
  landmarks_df %>% 
  st_as_sf(
    coords = c("x", "y"),
    crs = 4269)

landmarks

# plotting

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(
    data = landmarks,
    aes(color = Landmark),
    size = 3)

# generate a linestring:

my_pilgrimage <- 
  landmarks_df %>%
  
  # remove all columns except long and lat:
  
  select(!Landmark) %>% 
  
  #convert df to matrix:
  
  as.matrix() %>% 
  
  # create linestring geometry:
  
  st_linestring() %>%
  
  #convert geometry to simple features column:
  
  st_sfc(crs = 4269) %>% 
  
  # convert object to sf:
  
  st_sf()

# plot:

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(data = my_pilgrimage) +
  geom_sf(
    data = landmarks,
    aes(color = Landmark),
    size = 3)

# the order of the rows in your matrix matters (see):

confused_pilgrimage <-
  landmarks_df %>%
  arrange(Landmark) %>% 
  select(!Landmark) %>% 
  as.matrix() %>% 
  st_linestring() %>% 
  st_sfc(crs = 4269) %>% 
  st_sf()

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(data = confused_pilgrimage) +
  geom_sf(
    data = landmarks,
    aes(color = Landmark),
    size = 3)

# POLYGONS

my_poly_list <-
  tribble(
    ~ Landmark,          ~ x,       ~ y,
    "Avalon",            -90.02160, 33.65646,
    "Train station",     -79.07005, 35.91097,
    "Newport",           -71.33749, 41.47934,
    "Gaslight Cafe",     -74.00044, 40.72979,
    "Smithsonian-Mason", -78.16464, 38.88697,
    "Avalon",            -90.02160, 33.65646) %>% 
  select(!Landmark) %>% 
  as.matrix() %>% 
  list()

# creating a polygon:

my_poly <- 
  my_poly_list %>% 
  
  #create apolygon geometry:
  
  st_polygon() %>% 
  
  #Convert the geometry to a simple features column:
  
  st_sfc(crs = 4269) %>% 
  
  #Convert the object to sf: 
  
  st_sf()

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(
    data = my_poly,
    fill = "#33eeff",
    alpha = 0.5) +
  geom_sf(
    data = landmarks,
    aes(color = Landmark),
    size = 3)

# order of points matters when creating a polygon:

messy_poly <-
  landmarks_df %>% 
  bind_rows(
    slice(landmarks_df, 1)) %>% 
  select(!Landmark) %>% 
  as.matrix() %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc(crs = 4269) %>% 
  st_sf()

# plot messy poly:

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(
    data = 
      messy_poly,
    fill = "#33eeff",
    alpha = 0.5) +
  geom_sf(
    data = landmarks,
    aes(color = Landmark),
    size = 3)

#cast our linestring my_pilgrimage back to points:

my_pilgrimage %>% 
  st_cast("POINT")

# plot results:

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(
    data = my_pilgrimage %>% 
      st_cast("POINT"),
    size = 3)

#casting the polygon to points yields equivalent point locations:

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(
    data = my_poly %>% 
      st_cast("POINT"),
    size = 3)

# just North Carolina:

states %>%  
  filter(NAME == "North Carolina") %>% 
  ggplot() +
  geom_sf()

#cast the geometry from polygon to point:
  
states %>%  
  filter(NAME == "North Carolina") %>% 
  st_cast("POINT") %>%
  ggplot() +
  geom_sf()

landmarks  

# cast points to linestring (not useful, still really points):

landmarks %>% 
  st_cast("LINESTRING")

# extract a matrix of coordinates from a point object:

landmarks %>% 
  st_coordinates() %>%

    # generate linestring geometry:
  
  st_linestring() %>% 
  
  # create and sfc object and crs:
  
  st_sfc(crs = 4269) %>% 
  
  # convert to sf:
  st_sf()

# making a line by combining to form a multipoint:

landmarks %>% 
  summarize(
    geometry = st_combine(geometry))

# cast these data to a linestring geometry:

landmarks %>% 
  summarize(
    geometry = st_combine(geometry)) %>% 
  st_cast("LINESTRING")

# plot to see if it worked:

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(
    data = 
      landmarks %>% 
      summarize(
        geometry = st_combine(geometry)) %>% 
      st_cast("LINESTRING"))

# how to change the order of points:

landmarks %>% 
  mutate(order = c(1, 3:2, 4:5)) %>%
  arrange(order)

# plot (with text) to see:

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf_text(
    data = 
      landmarks %>% 
      mutate(order = c(1, 3:2, 4:5)) %>% 
      arrange(order),
    aes(label = order),
    size = 8)

# cast points above to a line:

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(
    data = 
      landmarks %>% 
      mutate(order = c(1, 3:2, 4:5)) %>% 
      arrange(order) %>% 
      summarize(
        geometry = st_combine(geometry)) %>% 
      st_cast("LINESTRING"))

# converting points to polygons, first combine the points:

landmarks %>% 
  summarize(
    geometry = st_combine(geometry))

# then cast the output:

landmarks %>% 
  summarize(
    geometry = st_combine(geometry)) %>% 
  st_cast("POLYGON")

# plot to see how it looks:

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(
    data = 
      landmarks %>% 
      summarize(
        geometry = st_combine(geometry)) %>% 
      st_cast("POLYGON"),
    fill = "#33eeff",
    alpha = 0.5)

#to reorder the points of a messy polygon:

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf_text(
    data = 
      landmarks %>% 
      mutate(order = c(1:2, 5, 4:3)),
    aes(label = order),
    size = 8)

#arrange dataframe prior to casting the new points to a polygon:

states %>%  
  ggplot() +
  geom_sf() +
  geom_sf(
    data = 
      landmarks %>% 
      mutate(order = c(1:2, 5, 4:3)) %>% 
      arrange(order) %>% 
      summarize(
        geometry = st_combine(geometry)) %>% 
      st_cast("POLYGON"),
    fill = "#33eeff",
    alpha = 0.5)

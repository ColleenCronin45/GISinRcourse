# setup -------------------------------------------------------------------

library(lubridate)
library(sf)
library(tidyverse)

# A "new" theme:

theme_set(
  new = theme_bw())

# Load iNaturalist observations of spotted lanternflies (Note: The locations
# were recorded with the CRS EPSG 4326):

spotted_lanternfly <-
  read_rds('C:/Users/User/Documents/Week1/gis/gis/data/raw/spotted_lanternfly.rds') %>% 
  filter(quality_grade == 'research') %>% 
  select(
    !c(description,
       image_url,
       place_guess,
       quality_grade,
       user))

# Now you! Load the shapefiles states.shp, counties_low_res.geojson, and
# counties.geojson into your global environment.

states <-
  st_read("C:/Users/User/Documents/Week1/gis/gis/data/raw/shapefiles/states.shp") %>% 
  st_transform(5070) %>% 
  set_names(
    names(.) %>% 
      tolower()
  )

counties_low_res <-
  st_read("C:/Users/User/Documents/Week1/gis/gis/data/raw/shapefiles/counties_low_res.geojson")%>% 
  st_transform(5070) %>% 
  set_names(
    names(.) %>% 
      tolower()
  )
  
counties <-
  st_read("C:/Users/User/Documents/Week1/gis/gis/data/raw/shapefiles/counties.geojson")%>% 
  st_transform(5070) %>% 
  set_names(
    names(.) %>% 
      tolower()
  )

# Now you! Modify your code above such that the spatial data are projected with
# the CRS EPSG 5070:

# Now you! Modify your code above such that all names are lowercase.


# subset shapefiles using non-spatial joins -------------------------------

# Plot the states and counties_low_res files:

states %>% 
  ggplot() +
  geom_sf()

counties_low_res %>% 
  ggplot() +
  geom_sf()

# Use a semi-join to subset the counties to just those in the conterminous
# United States:

counties_low_res %>% 
  semi_join(
    states %>% 
      as_tibble(),
    by = c("state_name" = "name")
  ) %>% 
  ggplot() +
  geom_sf()

# Now you! Subset the counties file to just counties in states where
# spotted_lanternfly have been observed:

counties_lanternfly <-
  counties_low_res %>%
  semi_join(
    spotted_lanternfly, 
    by = c("state_name" = "state")
  )

# A map with county details for states with spotted lanternfly observations:

states %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = counties_lanternfly)
  
# summarize and join (non-spatial) ----------------------------------------

# Number of observations by state:

spotted_lanternfly %>% 
  group_by(state) %>% 
  summarize(n = n()) %>% 
  inner_join(
    states,
    .,
    by = c("name" = "state")
  ) %>% # class()
  ggplot() +
  geom_sf(aes(fill = n))

# Now you generate a map where states are colored by the number of spotted
# lanternfly observations and states with no observations are colored gray.

spotted_lanternfly %>% 
  group_by(state) %>% 
  summarize(n = n()) %>% 
  inner_join(
    states,
    .,
    by = c("name" = "state")
  ) %>% # class()
  ggplot() +
  geom_sf(data = states) +
  geom_sf(aes(fill = n))

# same as above

spotted_lanternfly %>% 
  group_by(state) %>% 
  summarize(n = n()) %>% 
  full_join(
    states,
    .,
    by = c("name" = "state")
  ) %>% # class()
  ggplot() +
  geom_sf(aes(fill = n))

# changing colors

spotted_lanternfly %>% 
  group_by(state) %>% 
  summarize(n = n()) %>% 
  full_join(
    states,
    .,
    by = c("name" = "state")
  ) %>% # class()
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "#dcdcdc")

# Spatial joins -----------------------------------------------------------

# Now you! Convert the spotted_lanternfly data frame to a spatial points object
# with the same CRS as the counties file:

lanternfly_sf <-
  spotted_lanternfly %>% 
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326) %>% 
  st_transform(st_crs(counties))

# Conduct a spatial join between lanternfly_sf and counties_lanternfly:

lanternfly_sf %>% 
  st_join(
    counties_lanternfly %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarize(n = n())

# Now you! Map counties by the number of spotted lanternflies observed:

lanternfly_sf %>% 
  st_join(
    counties_lanternfly %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarize(n = n()) %>% 
  full_join(
    counties_lanternfly,
    .,
    by = "geoid") %>% 
ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "#dcdcdc")

# to show only counties with lanternfly observations, change full join to inner join

lanternfly_sf %>% 
  st_join(
    counties_lanternfly %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarize(n = n()) %>% 
  inner_join(
    counties_lanternfly,
    .,
    by = "geoid") %>% 
  ggplot() +
  geom_sf(
    aes(fill = n),
color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "#dcdcdc")

# Now you! Create a map that shows the number of spotted lanternfly observations in impacted counties above the states in the conterminous US:

lanternfly_sf %>% 
  st_join(
    counties_lanternfly %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarize(n = n()) %>% 
  inner_join(
    counties_lanternfly,
    .,
    by = "geoid") %>% 
  ggplot() +
  geom_sf(data = states) +
  geom_sf(
    aes(fill = n),
    color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "#dcdcdc")


# Subset states to those with spotted lanternfly observations using a spatial
# filter:

states %>% 
  st_filter(lanternfly_sf)

# Modify the map so that only states with spotted lanternfly observations are
# shown:

lanternfly_sf %>% 
  st_join(
    counties_lanternfly %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  group_by(geoid) %>% 
  summarize(n = n()) %>% 
  inner_join(
    counties_lanternfly,
    .,
    by = "geoid") %>% 
  ggplot() +
  geom_sf(data = states %>% 
            st_filter(lanternfly_sf)
  ) +
  geom_sf(
    aes(fill = n),
    color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "#dcdcdc")

# Now you! Add state lines to the top of the map:

lanternfly_sf %>% 
  
  # join counties to lanternflies
  
  st_join(
    counties_lanternfly %>% 
      select(geoid)) %>% 
  as_tibble() %>% 
  
  # tally lanternflies by county
  
  group_by(geoid) %>% 
  summarize(n = n()) %>% 
  
  #join tally to counties
  
  inner_join(
    counties_lanternfly,
    .,
    by = "geoid") %>% 
  
  # plotting data
  
  ggplot() +
  
  # background states
  
  geom_sf(data = states %>% 
            st_filter(lanternfly_sf)
  ) +
  
  # affected counties
  
  geom_sf(
    aes(fill = n),
    color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "#dcdcdc") +
  
  # state borders
  
  geom_sf(
    data = states %>% 
      st_filter(lanternfly_sf),
    fill = NA)

# A review of joins -------------------------------------------------------

# Non-spatial joins with sf files are equivalent to data frame joins:

states %>% 
  semi_join(
    spotted_lanternfly, 
    by = c('name' = 'state')) %>% 
  ggplot() +
  geom_sf()

# Non-spatial joins can be a powerful tool when used in conjunction with
# summarize:

spotted_lanternfly %>% 
  group_by(state) %>% 
  summarize(n = n()) %>% 
  full_join(
    states, 
    .,
    by = c('name' = 'state')) %>% 
  ggplot() +
  geom_sf(aes(fill = n))

# Filtering spatial joins allow us to filter based on location:

states %>% 
  st_filter(lanternfly_sf) %>% 
  ggplot() +
  geom_sf()

# Intersecting spatial joins allow us to compare data between shapefiles:

lanternfly_sf %>% 
  st_join(states) %>% 
  as_tibble() %>% 
  group_by(name) %>% 
  summarize(n = n()) %>% 
  full_join(
    states,
    .,
    by = 'name') %>%
  ggplot() +
  geom_sf(aes(fill = n))

# Find out more!

?st_join
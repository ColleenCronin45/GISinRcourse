# Problem set 3, real-world version

# If the goal of my script was to produce the kable in question 9 and the map 
# in question 10, here is how my script would look!

# Note: This "real-world" version is still not quite "real-world" -- I am
# limiting the content to material that we have covered thus far in the 
# course.

# setup -------------------------------------------------------------------

library(tidyverse)
library(sf)

# read and pre-process county data ----------------------------------------

counties <- 
  st_read("data/raw/shapefiles/counties.geojson") %>% 
  
  # Clean names:
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  
  # Select columns of interest:
  
  select(geoid, state_name) %>% 
  
  # Subset to the District of Columbia, Maryland, or Virginia:
  
  filter(
    state_name %in% 
      c("District of Columbia",
        "Maryland",
        "Virginia")) %>% 
  
  # Transform CRS to EPSG 2283:
  
  st_transform(crs = 2283)

# read and pre-process cicada data ----------------------------------------

cicadas <- 
  read_csv("data/raw/cicadas_brood_x_2021.csv") %>% 
  
  # Subset to research grade and Brood X species:
  
  filter(
    quality_grade == "research",
    scientific_name %in% 
      c("Magicicada cassinii",
        "Magicicada septendecim",
        "Magicicada septendecula")) %>% 
  
  # Subset to fields of interest:
  
  select(datetime, scientific_name, longitude:latitude) %>% 
  
  # Convert to a shapefile with the same CRS as counties:
  
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326) %>% 
  st_transform(
    st_crs(counties)) %>% 
  
  # Join county data and subset to DC, MD, and VA:
  
  st_intersection(
    counties %>% 
      select(geoid, state_name))

# tabular output ----------------------------------------------------------

# Table describing the first observation by species and state:

cicadas %>% 
  
  # Calculate the first observation by county and species:
  
  as_tibble() %>% 
  summarize(
    datetime = min(datetime),
    .by = c(state_name, scientific_name)) %>%
  
  # Arrange by in the order of time observed:
  
  arrange(datetime) %>% 
  
  # Create a kable table:
  
  kableExtra::kable() %>% 
  kableExtra::kable_styling()

# map output --------------------------------------------------------------

# Generate a map of the number of cicada observed, across species, by 
# county:

cicadas %>%
  
  # Calculate the number of observations by county:
  
  as_tibble() %>%
  summarize(n = n(), .by = geoid) %>% 
  
  # Join to counties to make spatial:
  
  full_join(
    counties,
    .,
    by = "geoid") %>% 
  
  # Generate map:
  
  ggplot() +
  aes(fill = n) +
  geom_sf() +
  
  # Modify the fill color: 
  
  scale_fill_viridis_c(
    option = "cividis",
    na.value = "#dcdcdc") +
  
  # Change plot theme:
  
  theme_void()

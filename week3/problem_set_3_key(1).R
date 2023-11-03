# ANSWER KEY
# Problem set 3: Cicada emergence

# Getting started ---------------------------------------------------------

# 1. [0.5] Save and knit this document:

# * [0.1] Replace my name in the YAML header with yours
# * [0.1] Add the current date in the YAML header
# * [0.3] Save the .rmd file in the output folder of your project as (but 
#   replace my name with yours): problem_set_3_Evans_Brian.rmd

# 2. [0.25] Load the sf and tidyverse libraries.

library(sf)
library(tidyverse)

# 3. [0.5] Set the theme of all of the plots in this document to theme_void().

theme_set(
  theme_void())

# 4. [1.0] Read in and process the file cicadas_brood_x_2021.csv. In doing so:

# * [0.25] Load the data as a tibble;
# * [0.25] Subset the data to where the quality grade is classified as
#   “research”;
# * [0.25] Subset the data to the fields datetime, scientific_name, user, 
#   longitude, and latitude;
# * [0.25] Globally assign the resultant object with the name `cicadas_temp`.

cicadas_temp <-
  read_csv("data/raw/cicadas_brood_x_2021.csv") %>% 
  filter(quality_grade == "research") %>% 
  select(
    datetime:user, 
    longitude:latitude)

# 5. [1.25] Read and processes the file counties.geojson (or
# counties_low_res.geojson if you have memory issues!). In doing so:

# * [0.25] Read in the data as a simple features shapefile;
# * [0.25] Convert all field names to lowercase;
# * [0.25] Subset to the columns `geoid`, `name`, and `state_name` (in that
#.  order);
# * [0.25] Transform the Coordinate Reference System (CRS) of the object to
#   EPSG 2283 (a CRS often used for Virginia);
# * [0.25] Globally assign the resultant object with the name `counties_temp`.

counties_temp <-
  st_read("data/raw/shapefiles/counties.geojson") %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  select(geoid:name, state_name) %>% 
  st_transform(crs = 2283)

# Subsetting data ---------------------------------------------------------

# 6. [1.0] Brood X cicada are comprised of three species, Magicicada cassinii,
# M. septendecim, and M. septendecula. A large proportion of the observations
# (about 60%) could not be identified to species (you can explore this with
# group_by() and summarize()) and one of the observations was of a 13-year
# periodical cicada, M. tredecim (Note: This species is not a Brood X cicada!).

# * [0.50] Subset the data to Brood X cicada that could be identified to
#   species;
# * [0.25] Assign the object to your global environment with the name 
#   cicadas_brood_x;
# * [0.25] Remove cicadas_temp from your global environment 
#   (in a separate code block).

cicadas_brood_x <-
  cicadas_temp %>% 
  filter(
    scientific_name %in% 
      c("Magicicada cassinii",
        "Magicicada septendecim",
        "Magicicada septendecula"))

rm(cicadas_temp)

# 7. [1.0] Subset counties_temp to counties in the District of Columbia,
# Maryland, or Virginia [0.50], assign to your global environment as with the
# name `counties` [0.25], and in a separate code block, remove counties_temp
# from your global environment [0.25].

counties <-
  counties_temp %>% 
  filter(
    state_name %in% 
      c("District of Columbia",
        "Maryland",
        "Virginia"))

rm(counties_temp)

# 8. [1.5] Convert cicadas_brood_x to an sf point file [0.25] and subset to our
# area of interest. In doing so:

# * [0.25] Transform the CRS of the object to the same projection as counties;
# * [0.50] Subset the data to observations in the District of Columbia, 
#   Maryland, or Virginia;
# * [0.25] Assign the name cicadas_sf to the resultant object;
# * [0.25] Remove cicadas_brood_x from your global environment (in a separate
#   code block).

cicadas_sf <-
  cicadas_brood_x %>% 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326) %>% 
  st_transform(
    st_crs(counties)) %>% 
  st_filter(counties)

rm(cicadas_brood_x)

# data exploration --------------------------------------------------------

# 9. [1.5] Generate a kable [0.25] that displays the datetime that each species
# of cicada [0.50] was first observed in each state [0.50]. Please arrange the
# table from earliest to latest datetime [0.50] (Note: The columns of the
# summary table should be state_name, scientific_name, and datetime).


cicadas_sf %>% 
  st_join(
    counties %>% 
      select(state_name)) %>% 
  as_tibble() %>% 
  summarize(
    datetime = min(datetime),
    .by = c(state_name, scientific_name)) %>% 
  arrange(datetime) %>% 
  kableExtra::kable() %>% 
  
  # Optional:
  
  kableExtra::kable_styling()

# 10. [1.5] Generate a choropleth map of counties [0.50], where:

# * [0.50] The fill color is determined by the number of cicadas that were 
#   observed in each county;
# * [0.50] Counties with no observations are colored light gray.

cicadas_sf %>%
  st_join(
    counties %>% 
      select(geoid)) %>% 
  as_tibble() %>%
  summarize(n = n(), .by = geoid) %>% 
  full_join(
    counties,
    .,
    by = "geoid") %>% 
  ggplot() +
  aes(fill = n) +
  geom_sf() +
  scale_fill_viridis_c(
    option = "cividis",
    na.value = "#dcdcdc")

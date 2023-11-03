
# setup -------------------------------------------------------------------

library(lubridate)
library(tidyverse)

iris_tbl <-
  read_rds('C:/Users/User/Documents/week4/data/raw/iris.rds')

# split-apply-combine, Rube Goldberg method -------------------------------

# Now you! Subset iris_tbl to observations of Iris setosa:

list(
  iris_tbl %>% 
    filter(species == "setosa") %>% pull(sepal_length) %>% 
    mean(),
  
  iris_tbl %>% 
    filter(species == "versicolor") %>% pull(sepal_length) %>% 
    mean(),
  
  iris_tbl %>% 
    filter(species == "virginica") %>% pull(sepal_length) %>% 
    mean())

# Now you! Write a custom function that will calculate the mean sepal length of
# any of the iris species:

get_mean_sepal <-
  function(x) {
    iris_tbl %>% 
      filter(species == x) %>% pull(sepal_length) %>% 
      mean()
  }

get_mean_sepal("setosa")

# Now you! Use your custom function to generate a list of mean sepal lengths for
# the three iris species:

list(
  get_mean_sepal("setosa"),
  get_mean_sepal("versicolor"),
  get_mean_sepal("virginica")
)

# To output a vector instead of a list:

c(
  get_mean_sepal("setosa"),
  get_mean_sepal("versicolor"),
  get_mean_sepal("virginica")
)

# To create a 2-column tibble with species and mean sepal length:

tibble(
  species = 
    iris_tbl %>% 
    pull(species) %>% 
    unique(),
  sepal_length = 
    c(
      get_mean_sepal("setosa"),
      get_mean_sepal("versicolor"),
      get_mean_sepal("virginica")
    ))

# introducing purrr! ------------------------------------------------------

iris_tbl %>% 
  pull(species) %>% 
  unique() %>% 
  map(
    function(x) {
      iris_tbl %>% 
        filter(species == x) %>% pull(sepal_length) %>% 
        mean()
    })

iris_tbl %>% 
  pull(species) %>% 
  unique() %>% 
  map(
    ~iris_tbl %>% 
      filter(species == .x) %>% pull(sepal_length) %>% 
      mean())

# to get an atomic vector instead of a list:

iris_tbl %>% 
  pull(species) %>% 
  unique() %>% 
  map_dbl(
    ~iris_tbl %>% 
      filter(species == .x) %>% pull(sepal_length) %>% 
      mean())

# To output the data as a dataframe with 2 columns for species and mean sepal length using the map function:

iris_tbl %>% 
  pull(species) %>% 
  unique() %>% 
  map_dfr(
    ~ tibble(
      species = .x,
      sepal_length =
        iris_tbl %>% 
        filter(species == .x) %>% pull(sepal_length) %>% 
        mean()))

# A simpler way ....

iris_tbl %>% 
  group_by(species) %>% 
  summarize(sepal_length = mean(sepal_length))

# mapping when it matters! ------------------------------------------------

# Read in the shapefiles, convert column names to lower case, then transform the
# CRS to EPSG 5070:

states <-
  st_read('data/raw/shapefiles/states.shp') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  st_transform(5070)

counties_low_res <-
  st_read('data/raw/shapefiles/counties_low_res.geojson') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  st_transform(crs = 5070)

counties <-
  st_read('data/raw/shapefiles/counties.geojson') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  st_transform(crs = 5070)

# Read in as a list:
library(sf)

my_shapes <-
  list(
    st_read('data/raw/shapefiles/states.shp'),
    st_read('data/raw/shapefiles/counties_low_res.geojson'),
    st_read('data/raw/shapefiles/counties.geojson')) %>% 
  map(
    ~ .x %>% 
      set_names(
        names(.) %>% 
          tolower()) %>% 
      st_transform(crs = 5070))

# Read in all at once:

file.path("data/raw/shapefiles",
          c('states.shp', 
            "counties_low_res.geojson", 
            "counties.geojson"))

# the code above reconstructed our file paths and returns a character vector where the values in the vector are the file paths

# Using a single map function, read in the shapefiles and conduct all the pre-processing steps above:

my_shapes <-
  file.path(
    'data/raw/shapefiles',
    c('states.shp',
    'counties_low_res.geojson',
    'counties.geojson')) %>% 
  map(
    ~ st_read(.x) %>% 
      set_names(
        names(.) %>% 
          tolower()) %>% 
      st_transform(crs = 5070))


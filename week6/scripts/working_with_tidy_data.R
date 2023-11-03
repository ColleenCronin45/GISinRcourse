
# setup -------------------------------------------------------------------

library(tidyverse)

source('C:/Users/User/Documents/Week1/gis/gis/scripts/source_script_crime_and_equity.R')

# read in the data:

file.path(
  'C:/Users/User/Documents/Week1/gis/gis/data/processed',
  'weather_tidy.rds') %>% 
  read_rds() %>% 
  list2env(.GlobalEnv)

# mutation, assignment ----------------------------------------------------

stations

elevation

stations$elevation*3.2

# mutate in base R --------------------------------------------------------

temp <-
  stations

# Add a column of elevation in feet:

temp$elevation <-
  temp$elevation*3.28

# Look again at the observations file:

observations

# Now you! Use as.numeric() to convert observations$precip from a character to a
# numeric column and assign it to column name precip:

observations$precip <-
as.numeric(observations$precip)

# But as a warning:

as.numeric(c(1, 2, '3,1'))

# mutate in the tidyverse -------------------------------------------------

# Remove temp file:

rm(temp)

# Convert elevation from meters to feet:

mutate(
  stations,
  elevation_ft = elevation*3.28)

# Now you! Re-arrange the above as a piped code block:

stations %>%
  mutate(elevation = elevation*3.28)

# Add a new variable with mutate

mutate(
  stations,
  elevation = elevation*3.28)

# multiple mutations ------------------------------------------------------

# Look again at observations:

observations

# Now you! Transform the snow column from a character vector to a numeric
# vector:

observations %>%
  mutate(snow = as.numeric(snow))
  
# Use chained mutate statements to transform snow and temperature_min:
  
observations %>%
  mutate(snow = as.numeric(snow)) %>% 
  mutate(temperature_min = as.numeric(temperature_min))

# Separate multiple mutate statements with a comma:

observations %>%
  mutate(snow = as.numeric(snow),
         temperature_min = as.numeric(temperature_min))

# Now you! Modify our mutation statement from above such that snow,
# temperature_min, and temperature_max are all numeric.

observations %>%
  mutate(snow = as.numeric(snow),
         temperature_min = as.numeric(temperature_min),
         temperature_max = as.numeric(temperature_max))

# transmute ---------------------------------------------------------------

# Transmute to return just the columns "name" and elevation in feet:

stations %>% 
  transmute(name,
            elevation = elevation*3.28)

# Now you! Copy-and-paste your transmute code into the space below. Modify the
# code such that the key for the column “name” is station_name:

stations %>% 
  transmute(station_name = name,
            elevation = elevation*3.28)

# subsetting date frames: by column ---------------------------------------

# Subset by position (index) with select:

stations %>%
  select(5)

# Subset by multiple positions (indices) with select:

stations %>% 
  select(4:6)

# Subset by column name with select:

stations %>% 
  select(state)

# Subset by a range of column names with select:

stations %>% 
  select(elevation:name)

# Subset by a selection of column names:

stations %>% 
  select(name, station)

# Subset to all but a given column:

stations %>% 
  select(-state)

# Subset to all but a selection of adjacent columns:

stations %>% 
  select(!state)

stations %>% 
  select(!state:name)

# Subset to all but a selection of non-adjacent columns:

stations %>% 
  select(!c(name, elevation))

# Now you! Use select to subset stations to station, name, longitude, latitude,
# and elevation:

stations %>% 
  select(station,
         name,
         longitude,
         latitude,
         elevation)

# Modify the above with select(), such that the binding "station" is changed to
# "station_id":

stations %>% 
  select(station_id = station,
         name:state,
         longitude:elevation)

# Modify the above with rename(), such that the binding "station" is changed to
# "station_id":

stations %>% 
  select(station,
         name:state,
         longitude:elevation) %>% 
  rename(station_id = station)

# Select with everything() to reorder columns:

stations %>% 
  select(state,
         everything())

# Subset by condition:

stations %>% 
  select(where(is.numeric))

# Now you! The function is.character is a logical test for whether a value is a
# character. Subset the stations data frame to just character columns:

stations %>% 
  select(where(is.character))

# Subsetting the data and returning a vector with pull():

stations %>% 
  pull(state)

# subsetting date frames: by row index ------------------------------------

# In the tidyverse, subset by row index with slice():

observations %>% 
  slice(3)

# Subset rows by a vector of indices:

observations %>% 
  slice(1:3)

# Now you! Subset the observations data frame to the first and third values:

observations %>% 
  slice(1, 3)

observations %>% 
  slice(c(1, 3))

# slice_head():

observations %>% 
  slice_head(n = 3)

# slice_tail():

observations %>% 
  slice_tail()

# Now you! Subset the observations data frame to the last three rows of the data
# set and only the column date:

observations %>% 
  select(date) %>% 
  slice_tail(n = 3)

# Arrange data by a column and subset the data:

observations %>% 
  mutate(
    temperature_min = 
      as.numeric(temperature_min)) %>% 
  arrange(temperature_min) %>% 
  slice_head()

# Arrange data in descending order:

observations %>% 
  mutate(
    temperature_max = 
      as.numeric(temperature_max)) %>% 
  arrange(
    desc(temperature_max))

# Now you! Extract a vector of the dates on which the five highest temperatures
# were observed:

observations %>% 
  mutate(
    temperature_max = 
      as.numeric(temperature_max)) %>% 
  arrange(
    desc(temperature_max)) %>% 
  slice_head(n = 5) %>% 
    pull(date)
  

# subsetting date frames: by row & condition ------------------------------

# Use the tidyverse function filter() to subset to stations greater than 40
# degrees latitude:

stations %>% 
  filter(latitude > 40)

# Get a vector of station IDs where the latitude is greater than 40:

stations_north <-
  stations %>% 
  filter(latitude > 40) %>% 
  pull(station)

# Now you! Use stations_north and the filter function to subset the observations
# to where the latitude of the station is greater than 40:

observations %>% 
  filter(station %in% stations_north)

# Subset the observations to the maximum precipitation value:

observations %>% 
  filter(
    precip == max(precip))

# Remove NA values with drop_na():

observations %>% 
  drop_na() %>% 
  filter(
    precip == max(precip))

# Remove NA values with na.rm:

observations %>% 
  filter(
    precip == max(precip, 
                  na.rm = TRUE))

# Remove NA values with !is.na():

observations %>% 
  filter(!is.na(precip)) %>% 
  filter(
    precip == max(precip))

# Filter based on multiple conditions:

observations %>% 
  filter(
    lubridate::year(date) == 2010,
    station %in% stations_north)

# Filter based on multiple conditions, a caveat:

observations %>% 
  filter(!is.na(precip),
         precip == max(precip))

# Now you! subset the data frame to the day in 2015 in which the maximum
# temperature was observed at weather stations above 40 degrees north:
 
observations %>% 
  filter(
    lubridate::year(date) == 2015,
    station %in% stations_north)  %>% 
      mutate(
        temperature_max = 
          as.numeric(temperature_max)) %>% 
      filter(
        temperature_max == max(
          temperature_max,
          na.rm = TRUE))

# Using distinct() to subset to unique columns:

observations %>% 
  select(station) %>% 
  distinct()

# Now you! Use subsetting to extract a vector that contains the unique dates of
# weather observations in 2015:

observations%>% 
  filter(lubridate::year(date) == 2015) %>% 
  select(date) %>% 
  distinct() %>% 
  pull()

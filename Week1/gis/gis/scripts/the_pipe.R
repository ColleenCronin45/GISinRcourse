
# setup -------------------------------------------------------------------

library(tidyverse)

# read in data

messy_weather <-
  read_csv("C:/Users/User/Documents/Week1/gis/gis/data/raw/messy_weather.csv")


# review of functions -----------------------------------------------------

multiply_by_two <-
  function(x) {
    x*2
  }

# nested functions --------------------------------------------------------

# Non-nested version

z1 <- 1:5

z2 <-
  multiply_by_two(z1)

add_one <-
  function(x) {
    x+1
  }

add_one(z2)

# Nested version

add_one(
  multiply_by_two(1:5))


# the pipe ----------------------------------------------------------------

1:5 %>%
  multiply_by_two() %>% 
  add_one()

rm(
  z,
  z1,
  z2,
  add_one,
  multiply_by_two)


# noaa weather data -------------------------------------------------------

# Fix day column of messy weather:

messy_weather_long_days <-
  pivot_longer(
    data = messy_weather,
    cols = march_1:march_31,
    names_to = "day",
    values_to = "values",
    names_prefix = "march_")

messy_weather %>% 
    pivot_longer(
    cols = march_1:march_31,
    names_to = "day",
    values_to = "values",
    names_prefix = "march_")

# Combine year, month, and day columns:

messy_weather_date_fix <-
  unite(
    messy_weather_long_days,
    c("year", "month", "day"),
    col = "date",
    sep = "-",
    na.rm = TRUE)

# Fix day column of messy weather:

messy_weather %>% 
  pivot_longer(
    cols = march_1:march_31,
    names_to = "day",
    values_to = "values",
    names_prefix = "march_") %>% 
  
  # Combine year, month, and day columns:
  
  unite(
    c("year", "month", "day"),
    col = "date",
    sep = "-",
    na.rm = TRUE)

#Give variables their own column

messy_weather_wide_weather <-
  pivot_wider(
    messy_weather_date_fix,
    names_from = variable,
    values_from = values)

# Fix day column of messy weather:

messy_weather %>% 
  pivot_longer(
    cols = march_1:march_31,
    names_to = "day",
    values_to = "values",
    names_prefix = "march_") %>% 
  
  # Combine year, month, and day columns:
  
  unite(
    c("year", "month", "day"),
    col = "date",
    sep = "-",
    na.rm = TRUE) %>% 

  #Give variables their own column
  
  pivot_wider(
    names_from = variable,
    values_from = values)

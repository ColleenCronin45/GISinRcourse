# This script will take you through the process of joining tables by common
# columns (keys).

# setup -------------------------------------------------------------------

library(tidyverse)

# Tidy weather data (from week 2):

read_rds("C:/Users/User/Documents/Week1/gis/gis/data/processed/weather_tidy.rds") %>% 
  list2env(.GlobalEnv)

# Messy bird data

messy_birds <- 
  read_rds("C:/Users/User/Documents/Week1/gis/gis/data/raw/messy_birds.rds")

# Now you! The data frame messy_birds contains multiple levels of observation.
# Using "spp" as the common column, split the data into two tidy tibbles:

measures <- 
  messy_birds %>% 
  select(
    capture_id,
    spp,
    wing:mass)

bird_info <- 
  messy_birds %>% 
  select(spp:diet) %>% 
  distinct()

# mutating joins ----------------------------------------------------------

# Join table with the primary key TO the table with the foreign key (the target
# table):

measures %>% 
  left_join(
    bird_info,
    by = "spp")

# What if they were switched? In the current version of dplyr, this throws a
# warning message!

bird_info %>% 
  left_join(
    measures,
    by = "spp")

# Duplicates are a big deal! Why?

measures %>% 
  left_join(
    messy_birds %>% 
      select(spp:diet),
    by = "spp")

rm(messy_birds)

# Just grab your columns of interest from the source table:

measures %>% 
  left_join(
    bird_info %>% 
      select(spp, common_name),
    by = "spp")

# Now you! Join the observations and stations data frames such that the
# observations include the state and name of the weather station:

observations %>% 
  select(
    station:date,
    temperature_min:temperature_max) %>% 
  left_join(
    stations %>% 
      select(station, state),
    by = "station")


# What if the key columns have different names?

measures %>% 
  left_join(
    bird_info %>% 
      select(
        spp_code = spp, 
        common_name),
    by = c("spp" = "spp_code"))

# What happens when there are values in the target table that do not match?

measures %>% 
  filter(spp != "NOCA") %>% 
  left_join(
    bird_info %>% 
      select(spp, common_name),
    by = "spp")

# What happens when there are values in the source table that do not match?

measures %>% 
  left_join(
    bird_info %>% 
      filter(spp != "NOCA") %>% 
      select(spp, common_name),
    by = "spp")

# Full joins maintain non-matching records:

measures %>% 
  filter(spp != "NOCA") %>% 
  full_join(
    bird_info %>% 
      select(spp, common_name),
    by = "spp")

# Inner joins join the data across tables, but only maintain matching columns:

measures %>% 
  filter(spp != "NOCA") %>% 
  inner_join(
    bird_info %>% 
      select(spp, common_name),
    by = "spp")

measures %>% 
  inner_join(
    bird_info %>% 
      filter(spp != "NOCA") %>% 
      select(spp, common_name),
    by = "spp")

# Now you! Join the observations and stations data frames such that the
# observations include the name of weather stations in New York (NY):

observations %>%
  inner_join(
    stations %>% 
      filter(state == 'NY') %>% 
      select(station, name),
    by = "station"
  )
  

# filtering joins ---------------------------------------------------------

# Subset by extracting a vector:

omnivores <- 
  bird_info %>% 
  filter(diet == "omnivore") %>% 
  pull(spp)

measures %>% 
  filter(spp %in% omnivores)

rm(omnivores)

# Luckily, there is a semi_join!

measures %>% 
  semi_join(
    bird_info %>% 
      filter(diet == "omnivore"),
    by = "spp")

# ... and, it's opposite, the anti_join!

measures %>% 
  anti_join(
    bird_info %>% 
      filter(diet == "omnivore"),
    by = "spp")

# Now you! Use a filtering join to subset the stations data frame to just those
# that did NOT received snow in the month of March:

stations %>% 
  anti_join(
    observations %>% 
      filter(snow > 0),
    by = "station")

# try with semi-join

stations %>% 
  semi_join(
    observations %>% 
      filter(snow == 0),
    by = "station")


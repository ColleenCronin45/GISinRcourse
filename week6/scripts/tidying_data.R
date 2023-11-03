# setup -------------------------------------------------------------------

library(tidyverse)

source('C:/Users/User/Documents/Week1/gis/gis/scripts/source_script_crime_and_equity.R')

# read in data ------------------------------------------------------------

untidy_data <-
  read_rds('C:/Users/User/Documents/Week1/gis/gis/data/raw/untidy_data.rds')

# Send items in the list to the global environment:

list2env(
  untidy_data,
  envir = .GlobalEnv)

# Note: The above could have also been written as:

list2env(
  read_rds('data/raw/untidy_data.rds'),
  envir = .GlobalEnv)


# Each variable forms a column --------------------------------------------

# When values aren't atomic:

badDate

separate(
  data = badDate,
  col = observationDate,
  into = c('date1', 'date2'),
  sep = ", ")

# When a single variable occupies multiple columns:

really_bad_date

unite(
  data = really_bad_date,
  col = 'date',
  c(year, month, day),
    sep = '-')

# When there are transitive columns:

badYear

select(badYear, id:mass)


# Each observation forms a row --------------------------------------------

# Multiple observations per row:

untidyFrame

pivot_longer(
  data = untidyFrame,
  cols = treatmentA:treatmentB,
  names_to = 'treatment',
  names_prefix = 'treatment',
  values_to = 'value'
)

# Multiple rows per observation:

dfTooLong

pivot_wider(
  data = dfTooLong,
  names_from = measurement,
  values_from = value
)

# Each level of observation forms a table ---------------------------------

badBandingRecord

bird_list <-
  list(
  birds =
    select(
      badBandingRecord,
      birdID,
      observationDate,
      site,
      mass),
  sites =
    select(
      badBandingRecord,
      site,
      canopyCover))

bird_list[[2]] <- distinct(bird_list[[2]])

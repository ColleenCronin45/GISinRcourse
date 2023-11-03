# Categorizing and re-categorizing vectors in a data frame

# setup -------------------------------------------------------------------

library(tidyverse)

# Read in cicadas and birds and send the files to the global environment:

read_rds('data/processed/birds_cicadas_lc.rds') %>% 
  list2env(.GlobalEnv)

# the cut function --------------------------------------------------------

tibble(values = 1:10) %>% 
  mutate(
    classified = 
      cut(
        values, 
        breaks =  c(1, 5, 10),
        include.lowest = FALSE,
        right = TRUE))

# Now you! Modify the code below such that impervious surface values that are
# greater than 40 percent impervious surface are classified as high all other
# values are classified as high urban intensity:

cicadas %>% 
  mutate(
    urban_intensity = 
      cut(
        imp, 
        breaks =  c(0, 40, 100)))

# if_else -----------------------------------------------------------------

tibble(values = 1:10) %>% 
  mutate(
    classified = 
      if_else(values <= 3,
              'low',
              'high'))

# Now you! NCLD land cover classes 41 through 43 represent different types of
# forest. Tabulate the number of forest and non-forest observations.

cicadas %>% 
  mutate()

# Using if_else to fix bad coordinates:

bad_coords %>% 
  transmute(
    id,
    longitude = 
      if_else(
        longitude > 0,
        latitude, 
        longitude),
    latitude = 
      if_else(
        latitude < 0,
        longitude,
        latitude))

  # a stringr interlude -----------------------------------------------------

# if_else with str_detect:

nlcd_key %>% 
  mutate(
    name
  )

# Categorizing with stringr, the issue at hand:

birds %>% 
  pull(age) %>% 
  unique()

# if_else method:

birds %>% 
  mutate(
    age = 
      if_else(
        age %in% c('Nestling', 'Fledgling'),
        
        # Your code is correct, mine was not ...
        
        'Juvenile',
        age)) %>% 
  pull(age) %>% 
  unique()

# String replace method:

birds %>% 
  mutate(
    age) %>% 
  pull(age) %>% 
  unique()

# Now you! Use str_detect and if_else to reclassify the name field in nlcd_key
# to “developed” and “undeveloped land”:

nlcd_key %>% 
  mutate(
    name)

# case_when ---------------------------------------------------------------

# A case_when statement:

tibble(values = 1:10) %>% 
  mutate(
    classified = 
      case_when(
        values <= 3 ~ 'low',
        values <= 7 ~ 'medium',
        values <= 10 ~ 'high'))

# Now you! Modify the code below, using impervious surface and case_when to
# classify developed land into low (<= 20%), medium (<=  60%), and high
# development intensity:

birds %>% 
  mutate()

# Reclassify carefully!

birds %>% 
  transmute(
    age,
    new_age = 
      case_when(
        age == 'Juvenile' ~ age,
        age %in% c('Nestling', 'Fledgling') ~ 'Juvenile',
        TRUE ~ 'Adult')) %>% 
  distinct()


# Lesson goal: Use tidyverse tools to subset atomic vectors and lists.  Pay
# special attention to the functions:

# * magrittr::extract
# * purrr::keep
# * purrr::pluck
# * tidyselect:::select

# setup -------------------------------------------------------------------

library(tidyverse)

# Data for this lesson:

garden <- 
  read_rds("data/raw/my_covid_garden.rds")

instruments <- 
  read_rds("data/raw/four_instruments.rds")

# magrittr::extract by position -------------------------------------------

# Subset an atomic vector by a single position:

instruments[1]

magrittr::extract(instruments, 1)

instruments[[1]]

instruments %>% 
  purrr::pluck(1)

# Subset a list by single position:

garden %>% 
  .[1]

garden %>% 
  magrittr::extract(1)

garden %>% 
  .[[1]]

garden %>% 
  pluck(1)

# Subset an atomic vector by multiple positions:

instruments %>% 
  .[1:2]

instruments %>% 
  magrittr::extract(1:2)

instruments %>% 
  .[[1:2]]

instruments %>% 
  pluck(1:2)

# Subset a list by multiple positions:

garden %>% 
  .[1:2]

garden %>% 
  magrittr::extract(1:2)

# magrittr::extract by name -----------------------------------------------

# magrittr::extract from an atomic vector by a single name:

instruments %>% 
  magrittr::extract("roger")

instruments %>% 
  .[["roger"]]

instruments$roger

instruments %>% 
  pluck("roger")

# magrittr::extract from a list by a single name:

garden %>% 
  magrittr::extract("plants_and_shrooms")

garden %>% 
  .[["plants_and_shrooms"]]

garden$plants_and_shrooms

garden %>% 
  pluck("plants_and_shrooms")

# magrittr::extract from a list by multiple names:

garden %>% 
  magrittr::extract(
    c("garden_beds", "plants_and_shrooms"))

garden %>% 
  tidyselect:::select(garden_beds, plants_and_shrooms)

# Negated selection by name:

instruments %>% 
  tidyselect:::select(!roger)

garden %>% 
  tidyselect:::select(!food_events) %>% 
  names()

# Negated selection with partial string matching!

garden %>% 
  tidyselect:::select(
    matches("nt")) %>%
  names()

# Now you! Use matches() to magrittr::extract a vector of instruments where the
# name is not contained in the strings "roger" or "keith":

instruments

# lists can be deep -------------------------------------------------------

# magrittr::extraction function, [["x"]]:

garden[["plants_and_shrooms"]][["species"]]

# magrittr::extraction function, $:

garden$plants_and_shrooms$species

# purrr:

garden %>% 
  pluck("plants_and_shrooms") %>% 
  pluck("species")
  
# subsetting vectors with logic -------------------------------------------

# Ye olde indexing method for subsetting atomic vectors:

instruments[instruments == "guitar"]

# Now you! Use indexing and str_detect() to subset instruments to those that 
# are not "vocals" or "drums":

instruments

# Now with purrr::keep()!

instruments %>% 
  keep(~ .x %in% c("vocals", "drums"))

garden %>% 
  pluck("food_events") %>% 
  pull(species) %>% 
  unique() %>% 
  keep(
    ~ str_detect(.x, "squash"))

# Now you! Subset the vector garden$food_events$event to those that do not
# contain the word "eaten":



# Fin!

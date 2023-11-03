 # Source script for tutorial "Categorizing non-spatial and spatial data"

# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

# Load the source script:

source("scripts/source_script_categorize_tutorial.R")

# Set the tmap_mode for our entire session:

tmap_mode("view")


# two classes, if_else() --------------------------------------------------

# A simple logical test:

1:2 == 2

# Supply a logical test of a vector, the value if the test evaluates to 
# TRUE, and the value if the test evaluates to FALSE:

if_else(
  1:2 == 2, 
  true = "world", 
  false = "hello")

# Or, more simply:

if_else(
  1:2 == 2, 
  "world", 
  "hello")

# Have a look at example_df (created and assigned in the source script):

example_df

# Add a new variable that represents whether a value is equal to zero:

example_df %>% 
  mutate(is_zero = numbers == 0)

# Classify into two values:

example_df %>% 
  mutate(
    is_zero = numbers == 0,
    classes = 
      if_else(
        numbers == 0,
        "zero",
        "not zero"))

# If you're clear about the assignment of your classes ...

example_df %>% 
  mutate(
    classes = 
      if_else(
        numbers == 0,
        "zero",
        "not zero"))

# Modify an existing columns:

example_df %>% 
  mutate(
    numbers = 
      if_else(
        numbers == 0,
        "zero",
        "not zero"))

# Sometimes, we want to reclassify some, but not all, values:

example_df %>% 
  mutate(
    classes = 
      if_else(
        fruit %in% c("blackberry", "boysenberry"),
        "aggregate",
        fruit))

# ... this can be used for true = ... or false = ...:

example_df %>% 
  mutate(
    classes = 
      if_else(
        !fruit %in% c("blackberry", "boysenberry"),
        fruit,
        "aggregate"))

# Values must be of the same class with if_else():

example_df %>% 
  mutate(
    classes = 
      if_else(
        numbers == 0,
        "zero",
        1))

# ... but not with ifelse():

example_df %>% 
  mutate(
    classes = 
      ifelse(
        numbers == 0,
        "zero",
        1))

# if_else(), applied: nlcd ------------------------------------------------

# nlcd key, subset to id and name:

nlcd_key %>% 
  select(id:name)

# if_else, applied:

nlcd_key %>% 
  select(id:name) %>% 
  mutate(
    class = 
      if_else(
        id %in% 21:23,
        "developed",
        "undeveloped"))

# if_else(), applied: surface water and census_tracts ---------------------

# Have a look at the census file:

census

# Subset to geoid and awater:

census %>% 
  select(geoid, awater)

# Subset to features where there is no surface water:

census %>% 
  select(geoid, awater) %>% 
  filter(awater == 0)

# Classify census tracts with no surface water as "no" and all others as
# "yes":

census %>% 
  select(geoid, awater) %>% 
  
  # Classify surface water:
  
  mutate(
    surface_water = 
      ifelse(
        awater == 0,
        "yes",
        "no"))

# Map these data to view the distribution of surface water:

census %>% 
  select(geoid, awater) %>% 
  
  # Classify surface water:
  
  mutate(
    surface_water = 
      ifelse(
        awater == 0,
        "no",
        "yes")) %>% 
  
  # Visualize the distribution with a map:
  
  tm_shape() +
  tm_polygons(col = "surface_water")

# Generate unionized polygons based on this classification:

census %>% 
  select(geoid, awater) %>% 
  
  # Classify surface water:
  
  mutate(
    surface_water = 
      ifelse(
        awater == 0,
        "no",
        "yes")) %>% 
  
  # Unionize geometries:
  
  summarize(
    geometry = st_union(geometry),
    .by = surface_water)

# Map these data to view the unionized polygons:

census %>% 
  select(geoid, awater) %>% 
  
  # Classify surface water:
  
  mutate(
    surface_water = 
      ifelse(
        awater == 0,
        "no",
        "yes")) %>% 
  
  # Unionize geometries:
  
  summarize(
    geometry = st_union(geometry),
    .by = surface_water) %>% 
  
  # Visualize the distribution with a map:
  
  tm_shape() +
  tm_polygons(col = "surface_water")

# The map above suggests that there is a lot of surface water in DC. It is
# dangerously easy to lie with spatial data!

census %>% 
  select(geoid, awater) %>% 
  
  # Classify surface water:
  
  mutate(
    surface_water = 
      ifelse(
        awater == 0,
        "no",
        "yes")) %>% 
  
  # Unionize geometries:
  
  summarize(
    geometry = st_union(geometry),
    .by = surface_water) %>% 
  
  # Calculate the total area per class:
  
  mutate(
    area = st_area(.) %>% 
      units::set_units('km^2')) %>% 
  st_drop_geometry()

# three or more values, case_when() ---------------------------------------

# Have another look at example_df:

example_df

# Assign the class "low" to `number`values less than or equal to 20, "medium"
# for values between 30 and 60, and "high" for values greater than 60:

example_df %>% 
  mutate(
    class = 
      case_when(
        numbers <= 20 ~ "low",
        numbers <= 60 ~ "medium",
        numbers > 60 ~ "high"))

# Each logical test is evaluated in order:

example_df %>% 
  mutate(
    class = 
      case_when(
        numbers <= 60 ~ "medium",
        numbers <= 20 ~ "low",
        numbers > 60 ~ "high"))

# ... but this provides us with an opportunity:

example_df %>% 
  mutate(
    class = 
      case_when(
        numbers <= 20 ~ "low",
        numbers <= 60 ~ "medium",
        TRUE ~ "high"))

# But what happens when one of the values is NA?

example_df_with_na 

# ... let's see:

example_df_with_na %>% 
  mutate(
    class = 
      case_when(
        numbers <= 20 ~ "low",
        numbers <= 60 ~ "medium",
        TRUE ~ "high"))

# case_when(), applied: nlcd ----------------------------------------------

# Have another look at `nlcd_key`

nlcd_key %>% 
  select(id:name)

# Classify into developed, agricultural land, and other:

nlcd_key %>% 
  select(id:name) %>% 
  mutate(
    class = case_when(
      id %in% 21:24 ~ "developed",
      id %in% 81:82 ~ "agriculture",
      TRUE ~ "other"))

# case_when(), applied: population density of census tracts ---------------

# In the code block below, I calculate the population density of each
# census tract, by land area (aland, in square meters):

census %>% 
  mutate(
    population_density = 
      population/aland * 1E6)

# Visualize these data with a map:

census %>% 
  mutate(
    population_density = 
      population/aland * 1E6) %>% 
  tm_shape() +
  tm_polygons(col = "population_density")

# Use case_when to categorize population densities:

census %>% 
  mutate(
    population_density = 
      population/aland * 1E6,
    density_class = 
      case_when(
        population_density == 0 ~ "not populated",
        population_density <= median(population_density) ~ "populated",
        TRUE ~ "densely populated"))

# We can plot this on a map as is:

census %>% 
  mutate(
    population_density = 
      population/aland * 1E6,
    density_class = 
      case_when(
        population_density == 0 ~ "not populated",
        population_density <= median(population_density) ~ "populated",
        TRUE ~ "densely populated")) %>% 
  tm_shape() +
  tm_polygons(col = "density_class")

# ... or combine like values by unionizing census tracts by density_class:

census %>% 
  mutate(
    population_density = 
      population/aland * 1E6,
    density_class = 
      case_when(
        population_density == 0 ~ "not populated",
        population_density <= median(population_density) ~ "populated",
        TRUE ~ "densely populated")) %>% 
  summarize(
    geometry = st_union(geometry),
    .by = density_class) %>% 
  tm_shape() +
  tm_polygons(col = "density_class")

# case_when(), applied: crime ---------------------------------------------

# Our next goal will be to reclassify a the offense field of crime:

crime

# Subset to distinct values to get a better look:

crime %>% 
  distinct(offense)

# ... or subset to a unique vector of crimes:

crime %>% 
  pull(offense) %>% 
  unique()

# In our previous exploration of District crime, we calculated the number
# of crimes per census tract:

census %>% 
  select(geoid) %>% 
  
  # Join crime data to census:
  
  st_join(crime) %>% 
  
  # Count the number of observations per geoid and type of crime:
  
  as_tibble() %>% 
  summarize(
    n = n(),
    .by = c(geoid, offense))

# ... and then reshaped the data frame:

census %>% 
  select(geoid) %>% 
  
  # Join crime data to census:
  
  st_join(crime) %>% 
  
  # Count the number of observations per geoid and type of crime:
  
  as_tibble() %>% 
  summarize(
    n = n(),
    .by = c(geoid, offense)) %>% 
  
  # Reshape the resultant object: 
  
  pivot_wider(
    names_from = offense,
    values_from = n)

# Using case_when() for reclassification:

crime %>% 
  
  # Reclassify "assault w/dangerous weapon" and "sex abuse":
  
  mutate(
    offense = 
      case_when(
        offense == "assault w/dangerous weapon" ~ "assault",
        offense == "sex abuse" ~ "sex_abuse",
        TRUE ~ offense))

# Did it work? It's best to double-check!

crime %>% 
  
  # Reclassify "assault w/dangerous weapon" and "sex abuse":
  
  mutate(
    offense_classified = 
      case_when(
        offense == "assault w/dangerous weapon" ~ "assault",
        offense == "sex abuse" ~ "sex_abuse",
        TRUE ~ offense)) %>% 
  distinct(offense, offense_classified)

# Apply to the full operation:

census %>% 
  select(geoid) %>% 
  
  # Join crime data to census:
  
  st_join(
    crime %>% 
      
      # Reclassify "assault w/dangerous weapon" and "sex abuse":
      
      mutate(
        offense = 
          case_when(
            offense == "assault w/dangerous weapon" ~ "assault",
            offense == "sex abuse" ~ "sex_abuse",
            TRUE ~ offense))) %>% 
  
  # Count the number of observations per geoid and type of crime:
  
  as_tibble() %>% 
  summarize(
    n = n(),
    .by = c(geoid, offense)) %>% 
  
  # Reshape the resultant object: 
  
  pivot_wider(
    names_from = offense,
    values_from = n)

# Add to the larger code block:

crime %>% 
  
  # Reclassify "assault w/dangerous weapon" and "sex abuse":
  
  mutate(
    offense = 
      case_when(
        offense == "assault w/dangerous weapon" ~ "assault",
        offense == "sex abuse" ~ "sex_abuse",
        TRUE ~ offense))

# Apply to the full operation:

census %>% 
  select(geoid) %>% 
  
  # Join crime data to census:
  
  st_join(
    crime %>% 
      
      # Reclassify "assault w/dangerous weapon" and "sex abuse":
      
      mutate(
        offense = 
          case_when(
            offense == "assault w/dangerous weapon" ~ "assault",
            offense == "sex abuse" ~ "sex_abuse",
            TRUE ~ offense))) %>% 
  
  # Count the number of observations per geoid and type of crime:
  
  as_tibble() %>% 
  summarize(
    n = n(),
    .by = c(geoid, offense)) %>% 
  
  # Reshape the resultant object: 
  
  pivot_wider(
    names_from = offense,
    values_from = n)

# Make the output spatial by joining this table to the census shapefile:

crime %>% 
  
  # Reclassify "assault w/dangerous weapon" and "sex abuse":
  
  mutate(
    offense = 
      case_when(
        offense == "assault w/dangerous weapon" ~ "assault",
        offense == "sex abuse" ~ "sex_abuse",
        TRUE ~ offense))

# Apply to the full operation:

census_crimes <- 
  census %>% 
  select(geoid) %>% 
  
  # Join crime data to census:
  
  st_join(
    crime %>% 
      
      # Reclassify "assault w/dangerous weapon" and "sex abuse":
      
      mutate(
        offense = 
          case_when(
            offense == "assault w/dangerous weapon" ~ "assault",
            offense == "sex abuse" ~ "sex_abuse",
            TRUE ~ offense))) %>% 
  
  # Count the number of observations per geoid and type of crime:
  
  as_tibble() %>% 
  summarize(
    n = n(),
    .by = c(geoid, offense)) %>% 
  
  # Reshape the resultant object: 
  
  pivot_wider(
    names_from = offense,
    values_from = n) %>% 
  
  # Make spatial:
  
  left_join(census, ., by = "geoid")


# Visualize the spatial distribution of assaults by census tract:

tm_shape(census_crimes) +
  tm_polygons(col = "assault")

# NA interlude ------------------------------------------------------------

# Have a look at example_df_with_na

example_df_with_na

# Replace NA values with if_else():

example_df_with_na %>% 
  mutate(
    fruit = if_else(
      is.na(fruit),
      "bell pepper",
      fruit))

# Replace NA values with replace_na():

example_df_with_na %>% 
  mutate(
    fruit = replace_na(fruit, "bell pepper"))

# NA, applied -------------------------------------------------------------

# Visualize the spatial distribution of assaults by census tract:

tm_shape(census_crimes) +
  tm_polygons(col = "assault")


# Replace NA values with zero (if appropriate):

census_crimes %>% 
  mutate(
    assault = replace_na(assault, 0)) %>% 
  tm_shape() +
  tm_polygons(col = "assault")

# factors, fct_recode -----------------------------------------------------

# Change factor levels by hand:

c("tiny", "huge") %>% 
  fct_recode(small = "tiny", 
             large = "huge")

# Modify a single factor level:

c("tiny", "huge") %>% 
  fct_recode(small = "tiny")

# The values matters, not the repetitions of that value or the value's 
# position:

c("tiny", "huge", "tiny") %>% 
  fct_recode(small = "tiny")

# If you want to change a level to a value that contains a space, you
# need to wrap your new factor level in quotes:

c("tiny", "huge", "tiny") %>% 
  fct_recode(small = "tiny", 
             "very large" = "huge")

# Assigning levels by hand can be cumbersome if there are a lot of values:

example_df %>% 
  filter(
    str_detect(fruit, "berry")) %>% 
  mutate(
    fruit = 
      fruit %>% 
      fct_recode(
        "true berry" = "bilberry",
        "true berry" = "blueberry",
        "aggregate berry" = "blackberry",
        "aggregate berry" = "boysenberry"))

# factors, fct_collapse ---------------------------------------------------

# If you have a lot of values to reclassify, use fct_collapse:

example_df %>% 
  filter(
    str_detect(fruit, "berry")) %>% 
  mutate(
    fruit = 
      fruit %>% 
      fct_collapse(
        "true berry" = c("bilberry", "blueberry"),
        "aggregate berry" = c("blackberry", "boysenberry")))

# Specify level for other:

example_df %>% 
  mutate(
    fruit = 
      fruit %>% 
      fct_collapse(
        "true berry" = c("bilberry", "blueberry"),
        "aggregate berry" = c("blackberry", "boysenberry"),
        other_level = "other fruit"))

# factors, fct_relevel ----------------------------------------------------

# If we have just a few factor levels, we can reorder the factors by hand.
# for example, have another look at the order of factor levels below:

c("tiny", "huge", "tiny") %>% 
  fct_recode(small = "tiny", 
             large = "huge")

# ... so:

c("tiny", "huge", "tiny") %>% 
  fct_recode(small = "tiny", 
             large = "huge") %>% 
  fct_relevel("small", "large")

# If we were to plot the data with the original order ...

tibble(
  size = 
    c("tiny", "huge", "tiny") %>% 
    fct_recode(
      small = "tiny", 
      large = "huge")) %>% 
  ggplot() +
  aes(x = size) +
  geom_bar()

# Plotting the releveled data ...

tibble(
  size = 
    c("tiny", "huge", "tiny") %>% 
    fct_recode(
      small = "tiny", 
      large = "huge") %>% 
    fct_relevel("small", "large")) %>% 
  ggplot() +
  aes(x = size) +
  geom_bar()

# factors, fct_reorder ----------------------------------------------------

# Real data, sloppy plot:

birds %>% 
  summarize(n = n(), .by = common_name) %>% 
  ggplot() +
  aes(x = common_name,
      y = n) +
  geom_bar(stat = "identity") +
  coord_flip()

# ... made less sloppy by fct_recode()!

birds %>% 
  summarize(n = n(), .by = common_name) %>% 
  mutate(
    common_name = fct_reorder(common_name, n)) %>% 
  ggplot() +
  aes(x = common_name,
      y = n) +
  geom_bar(stat = "identity") +
  coord_flip()

# factors applied: nlcd ---------------------------------------------------

# if_else() did a great job at categorizing the data but a bad job at
# communicating the process:

nlcd_key %>% 
  select(id:name) %>% 
  mutate(
    class = 
      if_else(
        id %in% 21:23,
        "developed",
        "undeveloped"))

# We can do a better job at communicating our reclassification using
# fct_collapse:

nlcd_key %>% 
  select(id:name) %>% 
  mutate(
    class = 
      name %>% 
      fct_collapse(
        "developed" = 
          c("Developed, open space",
            "Developed, low intensity",
            "Developed, medium intensity",
            "Developed, high intensity"),
        other_level = "undeveloped"))

# factors applied: surface water ------------------------------------------

# We reclassified and unionized census tracts that contained surface water:

census %>% 
  select(geoid, awater) %>% 
  
  # Classify surface water:
  
  mutate(
    surface_water = 
      ifelse(
        awater == 0,
        "no",
        "yes")) %>% 
  
  # Unionize geometries:
  
  summarize(
    geometry = st_union(geometry),
    .by = surface_water) %>% 
  
  # Visualize the distribution with a map:
  
  tm_shape() +
  tm_polygons(col = "surface_water")

# We might want the "yes" should come first in our key":

census %>% 
  select(geoid, awater) %>% 
  
  # Classify surface water:
  
  mutate(
    surface_water = 
      ifelse(
        awater == 0,
        "no",
        "yes") %>% 
      fct_relevel("yes", "no")) %>% 
  
  # Unionize geometries:
  
  summarize(
    geometry = st_union(geometry),
    .by = surface_water) %>% 
  
  # Visualize the distribution with a map:
  
  tm_shape() +
  tm_polygons(col = "surface_water")

# Want to be sneaky? Use a custom color palette and reduce the border:

census %>% 
  select(geoid, awater) %>% 
  
  # Classify surface water:
  
  mutate(
    surface_water = 
      ifelse(
        awater == 0,
        "no",
        "yes") %>% 
      fct_relevel("yes", "no")) %>% 
  
  # Unionize geometries:
  
  summarize(
    geometry = st_union(geometry),
    .by = surface_water) %>% 
  
  # Visualize the distribution with a map:
  
  tm_shape() +
  tm_polygons(
    col = "surface_water",
    lwd = 0.25,
    palette = c( "#33bcff", "#C1E1C1"))


# factors applied: population density -------------------------------------

# We used case when to generate this plot of population density:

census %>% 
  mutate(
    population_density = 
      population/aland * 1E6,
    density_class = 
      case_when(
        population_density == 0 ~ "not populated",
        population_density <= median(population_density) ~ "populated",
        TRUE ~ "densely populated")) %>% 
  summarize(
    geometry = st_union(geometry),
    .by = density_class) %>% 
  tm_shape() +
  tm_polygons(col = "density_class")

# Use fct_relevel() to change the order of the classes:

census %>% 
  mutate(
    population_density = 
      population/aland * 1E6,
    density_class = 
      case_when(
        population_density == 0 ~ "not populated",
        population_density <= median(population_density) ~ "populated",
        TRUE ~ "densely populated") %>% 
      fct_relevel(
        "not populated",
        "populated",
        "densely populated")) %>% 
  summarize(
    geometry = st_union(geometry),
    .by = density_class) %>% 
  tm_shape() +
  tm_polygons(col = "density_class")

# And choose a sequential palette to display the results:

census %>% 
  mutate(
    population_density = 
      population/aland * 1E6,
    density_class = 
      case_when(
        population_density == 0 ~ "not populated",
        population_density <= median(population_density) ~ "populated",
        TRUE ~ "densely populated") %>% 
      fct_relevel(
        "not populated",
        "populated",
        "densely populated")) %>% 
  summarize(
    geometry = st_union(geometry),
    .by = density_class) %>% 
  tm_shape() +
  tm_polygons(
    col = "density_class",
    palette = c("#bebebe", "#fbec5d", "#d22b2b"))

# stringr interlude, detecting strings ------------------------------------

# Does the pattern "r" occur in the string "gray"?

str_detect(string = 'gray', pattern = 'r')

# We can detect multiple symbols:

str_detect(string = 'gray', pattern = 'gr')

# ... but the symbols are evaluated in order:

str_detect(string = 'gray', pattern = 'gry')

# Detect strings in a vector with more than one value:

example_df %>% 
  mutate(
    test_fruit = str_detect(fruit, pattern = "rr"))

# Negated detection:

example_df %>% 
  mutate(
    test_fruit = !str_detect(fruit, pattern = "rr"))

# Use str_detect to subset data to matching patterns:

example_df %>% 
  filter(
    str_detect(fruit, pattern = "rr"))

# ... or negated matching patterns:

example_df %>% 
  filter(
    !str_detect(fruit, pattern = "rr"))

# Use with `if_else()` to classify data into two classes:

example_df %>% 
  mutate(
    fruit_classes = 
      if_else(
        str_detect(fruit, pattern = "rr"),
        "contains rr",
        "does not contain rr"))

# When used with the negation operator, `!`, can use `mutate`, `if_else`, and
# `str_detect` to classify based on strings do not contain "rr":

example_df %>% 
  mutate(
    fruit_classes = 
      if_else(
        !str_detect(fruit, pattern = "rr"),
        "does not contain rr",
        "contains rr"))

# Use with `case_when` for more than two classes:

example_df %>% 
  mutate(
    fruit_classes = 
      case_when(
        str_detect(fruit, pattern = "rr") ~ "contains rr",
        str_detect(fruit, pattern = "ap") ~ "contains ap",
        TRUE ~ "does not contain rr or ap"))

# Doing so can help illustrate the order-of-operations in a case_when
# statement:

example_df %>% 
  mutate(
    fruit_classes = 
      case_when(
        str_detect(fruit, pattern = "rr") ~ "contains rr",
        str_detect(fruit, pattern = "bl") ~ "contains bl",
        TRUE ~ "does not contain rr or bl"))

# The error above can be avoided by providing a more careful logical test:

example_df %>% 
  mutate(
    fruit_classes = 
      case_when(
        str_detect(fruit, pattern = "rr") &
          str_detect(fruit, pattern = "bl") ~ "contains rr and bl",
        str_detect(fruit, pattern = "rr") ~ "contains rr",
        str_detect(fruit, pattern = "bl") ~ "contains bl",
        TRUE ~ "does not contain rr or bl"))

# stringr interlude metacharacters ----------------------------------------

# Start of strings, ^

example_df %>% 
  filter(
    str_detect(fruit, pattern = "^a"))

# End of strings, $

example_df %>% 
  filter(
    str_detect(fruit, pattern = "y$"))

# %in% statements for multiple character values:

example_df %>% 
  filter(
    fruit %in% c("banana", "blackberry", "boysenberry"))

# It can *sometimes* be easier to use regex with the or metacharacter, |

example_df %>% 
  filter(
    str_detect(fruit, "banana|blackberry|boysenberry"))

# You can simplify this further by *carefully* reducting the symbols:

example_df %>% 
  filter(
    str_detect(fruit, "ba|kb|bo"))

# You can combine statements as well. Here, I'll subset to berries that
# start with "b" but are not bananas, blackberries, or boysenberries:

example_df %>% 
  filter(!str_detect(fruit, "ba|kb|bo"),
         str_detect(fruit, "^b"))

# stringr interlude, replacing strings ------------------------------------

# Subset fruit to those that contain the pattern "berry":

example_df %>% 
  filter(
    str_detect(fruit, "berry"))

# For true berries, replace the type of berry with "true ":

example_df %>% 
  filter(
    str_detect(fruit, "berry")) %>% 
  mutate(
    fruit = 
      fruit %>% 
      str_replace("bil|blue", "true "))

# For aggregate berries, replace the type of berry with "aggregate ":

example_df %>% 
  filter(
    str_detect(fruit, "berry")) %>% 
  mutate(
    fruit = 
      fruit %>% 
      str_replace("bil|blue", "true ") %>% 
      str_replace("black|boysen", "aggregate "))


# stringr interlude, removing strings -------------------------------------

# Have another look at the rows that contain the pattern "berry":

example_df %>% 
  filter(
    str_detect(fruit, "berry"))

# Remove the pattern "berry":

example_df %>% 
  filter(
    str_detect(fruit, "berry")) %>% 
  mutate(
    fruit = str_remove(fruit, "berry"))

# Remove the pattern " berry" from the classified berry data frame:

example_df %>% 
  filter(
    str_detect(fruit, "berry")) %>% 
  mutate(
    fruit = 
      fruit %>% 
      str_replace("bil|blue", "true ") %>% 
      str_replace("black|boysen", "aggregate ") %>% 
      str_remove(" berry"))


# stringr, strings for labels ---------------------------------------------

# String to sentence case:

example_df %>% 
  mutate(fruit = str_to_sentence(fruit))

# String to title case:

example_df %>% 
  mutate(fruit = str_to_title(fruit))

# stringr applied: nlcd ---------------------------------------------------

# Numeric key columns often don't communicate much to our readers:

nlcd_key %>% 
  select(id:name) %>% 
  mutate(
    class = 
      if_else(
        id %in% 21:23,
        "developed",
        "undeveloped"))

# Searching for and replacing based on partial string matching does an
# excellent job of communicating!

nlcd_key %>% 
  select(id:name) %>% 
  mutate(
    class = 
      if_else(
        str_detect(name, "Developed"),
        "developed",
        "undeveloped"))

# stringr applied: crime classification -----------------------------------

# We used case_when() to fix weird symbols in the `offense` field:

crime %>% 
  
  # Reclassify "assault w/dangerous weapon" and "sex abuse":
  
  mutate(
    offense_classified = 
      case_when(
        offense == "assault w/dangerous weapon" ~ "assault",
        offense == "sex abuse" ~ "sex_abuse",
        TRUE ~ offense)) %>% 
  distinct(offense, offense_classified)

# We can also repair with stringr -- first remove " w/dangerous weapon":

crime %>% 
  
  # Reclassify "assault w/dangerous weapon" and "sex abuse":
  
  mutate(
    offense_classified = 
      offense %>% 
      str_remove(" w/dangerous weapon")) %>% 
  distinct(offense, offense_classified)

# ... and pipe the results into str_replace() to remove the space:

crime %>% 
  
  # Reclassify "assault w/dangerous weapon" and "sex abuse":
  
  mutate(
    offense_classified = 
      offense %>% 
      str_remove(" w/dangerous weapon") %>% 
      str_replace(" ", "_")) %>% 
  distinct(offense, offense_classified)

# raster classification, categorical --------------------------------------

# Ye olde nlcd data frame:

nlcd_key %>% 
  select(id:name)

# Classify into developed and undeveloped land:

nlcd_key %>% 
  select(id:name) %>% 
  mutate(
    class = 
      if_else(
        str_detect(name, "Developed"), 1, 0))

# Set "Open water" to NA

nlcd_key %>% 
  select(id:name) %>% 
  mutate(
    class = 
      case_when(
        name == "Open water" ~ NA,
        str_detect(name, "Developed") ~ 1,
        TRUE ~ 0))

# Remove the `name` field:

nlcd_key %>% 
  select(id:name) %>% 
  mutate(
    class = 
      case_when(
        name == "Open water" ~ NA,
        str_detect(name, "Developed") ~ 1,
        TRUE ~ 0)) %>% 
  select(!name)

# Convert to a matrix and globally assign:

rcl_nlcd <-
  nlcd_key %>% 
  select(id:name) %>% 
  mutate(
    class = 
      case_when(
        name == "Open water" ~ NA,
        str_detect(name, "Developed") ~ 1,
        TRUE ~ 0)) %>% 
  select(!name) %>% 
  as.matrix()

# Reclassify the nlcd raster:

terra::classify(nlcd, rcl_nlcd)

# Plot the results

terra::classify(nlcd, rcl_nlcd) %>% 
  tm_shape() +
  tm_raster()

# Convert to a categorical raster:

nlcd %>% 
  terra::classify(rcl_nlcd) %>% 
  terra::categories(
    value = 
      data.frame(
        from = 0:1, 
        to = c("Undeveloped", "Developed"))) %>% 
  tm_shape() +
  tm_raster()

# Change name of the raster layer:

nlcd %>% 
  terra::classify(rcl_nlcd) %>% 
  terra::categories(
    value = 
      data.frame(
        from = 0:1, 
        to = c("Undeveloped", "Developed"))) %>% 
  setNames("Land use") %>% 
  tm_shape() +
  tm_raster()

# raster classification, continuous rasters -------------------------------

# Reclass matrices require three columns, from, to, and becomes:

tribble(
  ~ from, ~ to, ~ becomes,
  0,      60,     0,
  60,     100,    1) 

# Convert to a matrix and assign forest_rcl to the global environment:

forest_rcl <-
  tribble(
    ~ from, ~ to, ~ becomes,
    0,      60,     0,
    60,     100,    1) %>% 
  as.matrix()

# To determine the behavior at boundaries, start with a toy dataset:

example_df %>% 
  select(numbers)

# Use the function cut() and a toy dataset to determine what happens at 
# boundaries:

example_df %>% 
  select(numbers) %>% 
  mutate(
    class =
      cut(numbers, 
          breaks = c(0, 60, 100),
          include.lowest = TRUE, 
          right = TRUE))

# Examine the behavior with all potential combinations of the above:

example_df %>% 
  select(numbers) %>% 
  mutate(
    class =
      cut(numbers, 
          breaks = c(0, 60, 100),
          include.lowest = TRUE, 
          right = TRUE))

example_df %>% 
  select(numbers) %>% 
  mutate(
    class =
      cut(numbers, 
          breaks = c(0, 60, 100),
          include.lowest = TRUE, 
          right = FALSE))

example_df %>% 
  select(numbers) %>% 
  mutate(
    class =
      cut(numbers, 
          breaks = c(0, 60, 100),
          include.lowest = FALSE, 
          right = TRUE))

example_df %>% 
  select(numbers) %>% 
  mutate(
    class =
      cut(numbers, 
          breaks = c(0, 60, 100),
          include.lowest = FALSE, 
          right = FALSE))

# Apply the right combo of `include.lowest = TRUE/FALSE`and `right =
# TRUE/FALSE` to reclassify the raster:

canopy %>% 
  terra::classify(
    forest_rcl,
    include.lowest = TRUE,
    right = FALSE)

# We can use the categories function to convert the resultant object to a
# categorical raster:

canopy %>% 
  terra::classify(
    forest_rcl,
    include.lowest = TRUE,
    right = FALSE) %>% 
  terra::categories(
    value = 
      data.frame(
        from = 0:1, 
        to = c("Not forested", "Forested")))

# ... and setNames to assign a name to our layer:

canopy %>% 
  terra::classify(
    forest_rcl,
    include.lowest = TRUE,
    right = FALSE) %>% 
  terra::categories(
    value = 
      data.frame(
        from = 0:1, 
        to = c("Not forested", "Forested"))) %>% 
  setNames("Land cover")

# Plot the results:

canopy %>% 
  terra::classify(
    forest_rcl,
    include.lowest = TRUE,
    right = FALSE) %>% 
  terra::categories(
    value = 
      data.frame(
        from = 0:1, 
        to = c("Not forested", "Forested"))) %>% 
  setNames("Land cover") %>% 
  tm_shape() +
  tm_raster(palette = "Greens",
            alpha = 0.6)

# lying with tmap ---------------------------------------------------------

# The classification scheme for continuous data in tmap is an important
# decision:

census %>% 
  tm_shape() +
  tm_polygons(
    col = "awater",
    palette = "Blues")

# The style chosen yield vastly different results!

census %>% 
  tm_shape() +
  tm_polygons(
    col = "awater",
    style = "kmeans",
    palette = "Blues")

census %>% 
  tm_shape() +
  tm_polygons(
    col = "awater",
    style = "sd",
    palette = "Blues")




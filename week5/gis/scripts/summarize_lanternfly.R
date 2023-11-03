
# setup -------------------------------------------------------------------

library(lubridate)
library(tidyverse)

# Load iNaturalist observations of spotted lanternflies:

spotted_lanternfly <-
  read_rds('C:/Users/User/Documents/Week1/gis/gis/data/raw/spotted_lanternfly.rds')

# group and summarize data by one variable --------------------------------

spotted_lanternfly %>% 
  select(!c(place_guess,
            image_url,
            description))

# Quality grade:

spotted_lanternfly %>% 
  group_by(quality_grade) %>% 
  summarize(n = n())

# Which states have the most spotted lanternfly observations?

spotted_lanternfly %>% 
  group_by(state) %>% 
  summarize(n = n()) %>% 
  arrange(
    desc(n)) %>% 
  slice(1:5)

# Now you! Which users have submitted the most spotted lanternfly 
# observations?

spotted_lanternfly %>% 
  group_by(user) %>% 
  summarize(n = n()) %>% 
  arrange(
    desc(n)) %>% 
      slice(1:10)

# Group and summarize data by year and month ------------------------------

# How many observations per year?

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = 'identity')
+
  theme_bw()

# How many states had observations per year?

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarize(
    n = 
      state %>% 
      unique() %>% 
      length()) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = 'identity')
+
  theme_bw()

# How many observations per month?

spotted_lanternfly %>% 
  group_by(
    month = month(datetime,
                  label = TRUE)) %>% 
  summarize(
    n = n()) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_bar(stat = 'identity')
+
  theme_bw()

# How many observations by month and year?

spotted_lanternfly %>% 
  group_by(
    month = 
      round_date(datetime, 'month')) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_bar(stat = 'identity')
+
  theme_bw()

# Now you! Generate a plot of the number of observers by year and month:

spotted_lanternfly %>% 
  group_by(
    month = 
      round_date(datetime, 'month')) %>% 
  summarize(
    n = 
      user %>%
      unique() %>% 
      length()) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_bar(stat = 'identity')
+
  theme_bw()


# other summary functions -------------------------------------------------

# Latitudinal range expansion, northern boundary:

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarize(latitude = max(latitude)) %>% 
  ggplot(aes(x = year, y = latitude)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Now you! Generate a plot that shows the western limits of the spotted
# lanternfly observations for each year:

spotted_lanternfly%>% 
  group_by(year = year(datetime)) %>% 
  summarize(longitude = min(longitude)) %>% 
  ggplot(aes(x = year, y = longitude)) +
  geom_point() +
  geom_line() +
  theme_bw()

# multiple summaries ------------------------------------------------------

# Latitudinal range expansion, combined:

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarize(
    southern = min(latitude),
    northern = max(latitude)) %>% 
  pivot_longer(
    southern:northern,
    names_to = "limits",
    values_to = "latitude"
  ) %>% 
  ggplot(
    aes(x = year,
        y = latitude,
        color = limits)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Now you! Has the range of spotted lanternfly expanded longitudinally?

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarize(
    western = min(longitude),
    eastern = max(longitude)) %>% 
  pivot_longer(
    western:eastern,
    names_to = "limits",
    values_to = "longitude"
  ) %>% 
  ggplot(
    aes(x = year,
        y = longitude,
        color = limits)) +
  geom_point() +
  geom_line() +
  theme_bw()


# group and summarize data by multiple variables --------------------------

spotted_lanternfly %>% 
  group_by(
    year = year(datetime),
    state) %>% 
  summarize(n = n(),
            .groups = 'drop') %>% 
  ggplot(
    aes(x = year,
        y = n)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~state) +
  theme_bw()






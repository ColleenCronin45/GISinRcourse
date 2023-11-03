
# Tidyverse addendum ------------------------------------------------------


library(tidyverse)

weather <-
  read_rds("C:/Users/User/Documents/Week1/gis/gis/data/processed/weather_tidy.rds")

class(weather)

str(weather)

weather$observations

observations <-
  weather$observations %>% 
  mutate(
    precip = as.numeric(precip),
    snow = as.numeric(snow),
    temperature_min = as.numeric(temperature_min),
    temperature_max = as.numeric(temperature_max)
  )
   
summary(observations)       

nrow(observations)

observations %>% 
  filter(!is.na(precip))

observations %>% 
  drop_na(precip)

# Use !is.na() to filter out NAs in precip and subset to days with more 
# than 50 mm of precipitation:

observations %>% 
  filter(
    !is.na(precip),
    precip > 50)

# Use drop_na() to filter out NAs in precip and subset to days with more 
# than 50 mm of precipitation:

observations %>% 
  drop_na(precip) %>% 
  filter(precip > 50)

# Subset to days with more than 50 mm of precipitation:

observations %>% 
  filter(precip > 50)

# Explicitly remove NA before calculating the mean of precip:

observations %>% 
  drop_na(precip) %>%
  pull(precip) %>% 
  mean()

# Remove NA while calculating the mean of precip:

observations %>% 
  pull(precip) %>% 
  mean(na.rm = TRUE)

# Summarize precipitation by station using group_by() to group the data:

observations %>% 
  group_by(station) %>% 
  summarize(
    precip = mean(precip, na.rm = TRUE))

# Summarize precipitation by station using .by to group the data:

observations %>% 
  summarize(
    precip = mean(precip, na.rm = TRUE),
    .by = station)

# Use group_by to subset the data to the records with the highest 
# precipitation for each weather station:

observations %>% 
  group_by(station) %>% 
  filter(precip == max(precip, na.rm = TRUE))%>% 
  ungroup()

# Use .by to subset the data to the records with the highest 
# precipitation for each weather station:

observations %>% 
  filter(
    precip == max(precip, na.rm = TRUE),
    .by = station)

observations %>% 
  select(station) %>% 
  distinct()

observations %>% 
  distinct(station)

observations %>% 
  select(station, date, precip)

# Selecting just 3 columns and also changing precip from mm to in:

observations %>% 
  select(station, date, precip) %>% 
  mutate(precip = precip * 3.94E-3)

#Doing the same thing with transmute:

observations %>% 
  transmute(
    station,
    date,
    precip = precip * 3.94E-3)

#Using mutate and .keep:

observations %>% 
  mutate(
    station,
    date,
    precip = precip * 3.94E-3,
    .keep = "none")


# best practices in R coding ----------------------------------------------

library(tidyverse)

#Making a silly data frame:
C=rep(c('hello','world'),each=2)
d=c('apple','apricot','avocado','banana')
Mysilly.Data.Frame=data.frame(a=1:4,b=5:8,C,d)

# Objects that are used in the creation of a parent object and not used again should not be assigned to the global environment:

Mysilly.Data.Frame = data.frame(a = 1:4, b = 5:8, C = rep(c('hello', 'world'), each = 2), d = c('apple', 'apricot', 'avocado', 'banana'))

# some more rules applied:

my_silly_data_frame <- 
  data.frame(a = 1:4, b = 5:8, c = rep(c('hello', 'world'), each = 2), 
             d = c('apple', 'apricot', 'avocado', 'banana'))

# Each formal of a function (i.e., argument) lives on its own line:

my_silly_data_frame <- 
  data.frame(a = 1:4, 
             b = 5:8, 
             c = rep(c('hello',
                       'world'),
                     each = 2), 
             d = c('apple',
                   'apricot',
                   'avocado',
                   'banana'))

#There should be no more than one call to a named function per line of code:

my_silly_data_frame <- 
  data.frame(a = 1:4, 
             b = 5:8, 
             c = rep(
               c('hello',
                 'world'),
               each = 2), 
             d = c('apple',
                   'apricot',
                   'avocado',
                   'banana'))

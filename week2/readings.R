library(nycflights13)
library(tidyverse)
library(Lahman)

nycflights13::flights

filter(flights, month == 1, day == 1)

jan1 <- filter(flights, month == 1, day == 1)

(dec25 <- filter(flights, month == 12, day == 25))

near(sqrt(2) ^ 2,  2)

filter(flights, month == 11 | month == 12)

nov_dec <- filter(flights, month %in% c(11, 12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))

filter(flights, arr_delay <= 120, dep_delay <= 120)

# exercises

filter(flights, arr_delay >= 120)

filter(flights, dest == HOU)

nycflights13::flights

filter(flights) %>%
  unique(dest)

filter(flights, month == 7 | month == 8 | month == 9)

arrange(flights, year, month, day)

arrange(flights, desc(dep_delay))

df <- tibble(x = c(5, 2, NA))

arrange(df, desc(x))

#exercises

arrange(df, desc(is.na(x)))

arrange(flights, arr_delay)

arrange(flights, desc(distance/air_time)) %>% 
          select(distance, air_time)

arrange(flights, distance)

arrange(flights, desc(distance)) %>% 
  select(year, month, day, distance)

# Select all columns between year and day (inclusive)
select(flights, year:day)

# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

rename(flights, tail_num = tailnum)

select(flights, time_hour, air_time, everything())

#exercises

select(flights, dep_time, dep_delay, arr_time, arr_delay)

select(flights, starts_with("dep"), starts_with("arr"))

select(flights, ends_with("time"), ends_with("delay"))

select(flights, dep_time, dep_delay, dep_time)

?any_of

select(flights, contains("TIME", ignore.case = FALSE))

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)

transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)       

transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

# exercises

transmute(flights,
          dep_time,
          hour = dep_time %/% 100* 60,
          minute = dep_time %% 100,
          min_since_midnight = hour + minute
)

transmute(flights,
          air_time,
          arr_time,
          dep_time,
          difference = arr_time - dep_time
)

# Could not figure this one out 5.5:
transmute(flights,
          dep_time,
          arr_time,
          air_time,
          new_dep = dep_time %/% 100* 60 + dep_time %% 100,
          new_arr = arr_time %/% 100* 60 + arr_time %% 100,
          difference = new_arr - new_dep
)

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

#average delay by/grouped by date:

by_day <- group_by(flights, year, month, day)

summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

# explore the relationship between the distance and average delay for each location

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# group, summarise, filter

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

# using na.rm

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))

# remove cancelled flights

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

# scatterplot number of flights v. average delay

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

# filter out groups with the fewest observations, so 
# you can see less of the extreme variation in the smallest groups

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)

batters %>% 
  arrange(desc(ba))

# not sure why object 'arr_delay' is not found
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]))

# Why is distance to some destinations more variable than to others?

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))    

# When do the first and last flights leave each day?

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  ) 

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)

# to provide a weight variable: 
# Ex: use this to “count” (sum) the total number of miles a plane flew:

not_cancelled %>% 
  count(tailnum, wt = distance)

# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_prop = mean(arr_delay > 60))

# grouping by multiple variables

daily <- group_by(flights, year, month, day)

(per_day   <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))

(per_year  <- summarise(per_month, flights = sum(flights)))

# ungrouping

daily %>% 
  ungroup() %>%             
  # no longer grouped by date
  summarise(flights = n())  
# all flights

# exercises 5.6.7

# TIDY DATA READING

# Compute rate per 10,000

table1 %>% 
  mutate(rate = cases / population * 10000)

# Compute cases per year

table1 %>% 
  count(year, wt = cases)

# Visualise changes over time

library(ggplot2)

ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

# using pivot_longer

table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

# joining the tables

tidy4a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
tidy4b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")
left_join(tidy4a, tidy4b)

# using pivot_wider

table2 %>%
  pivot_wider(names_from = type, values_from = count)

# exercises

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

# separate and unite

table3 %>% 
  separate(rate, into = c("cases", "population"))

table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")

# turn a character column into a numerical one when separating

table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)

# separating years into two 2-digit columns

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)

# uniting

table5 %>% 
  unite(new, century, year)

table5 %>% 
  unite(new, century, year, sep = "")

# exercises

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"))

?separate
?extract

# Missing values

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

# make implicit missing values explicit

stocks %>% 
  pivot_wider(names_from = year, values_from = return)

# another way:

stocks %>% 
  complete(year, qtr)

#  to turn explicit missing values implicit:

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(
    cols = c(`2015`, `2016`), 
    names_to = "year", 
    values_to = "return", 
    values_drop_na = TRUE
  )

# PIPING

library(magrittr)


# setup -------------------------------------------------------------------


install.packages("nycflights13")

library(nycflights13)

library(tidyverse)

nycflights13::flights

jan1 <- filter(flights, month == 1)

(dec25 <- filter(flights, month == 12, day == 25))

filter(flights, month == 11 | month == 12)

nov_dec <- filter(flights, month %in% c(11, 12))

# find flights not delayed by more than 2 hours

filter(flights, !(arr_delay > 120 | dep_delay > 120))

filter(flights, arr_delay <= 120, dep_delay <= 120)

# filter excludes FALSE and NA values

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)

filter(df, is.na(x) | x > 1)

# exercises

filter(flights, arr_delay >= 120)

filter(flights, dest == IAH | dest == HOU)

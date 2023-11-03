library(tidyverse)

x1 <- c("Dec", "Apr", "Jan", "Mar")

# could be typos though:

x2 <- c("Dec", "Apr", "Jam", "Mar")

# making factors will allow you to sort:

# make levels first:

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

# make a factor:

y1 <- factor(x1, levels = month_levels)

sort(y1)

# any values not in the set will be silently converted to NA:

y2 <- factor(x2, levels = month_levels)

y2

# If you want a warning, you can use readr::parse_factor():

y2 <- parse_factor(x2, levels = month_levels)

# If you omit the levels, they’ll be taken from the data in alphabetical order:

factor(x1)

# Sometimes you’d prefer that the order of the levels match the order of the first appearance in the data. You can do that when creating the factor by setting levels to unique(x), or after the fact, with fct_inorder():

f1 <- factor(x1, levels = unique(x1))

f1

f2 <- x1 %>% factor() %>% fct_inorder()

f2

# to access the set of valid levels directly:

levels(f2)

# General Social Survey

forcats::gss_cat

?gss_cat

# one way to see the levels is with count:

gss_cat %>%
  count(race)

# or with a bar chart:

ggplot(gss_cat, aes(race)) +
  geom_bar()

# force ggplot to display levels with no values:

ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

# exercises:

gss_cat %>%
  count(rincome)

ggplot(gss_cat, aes(rincome)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

gss_cat %>%
  count(relig)

gss_cat %>%
  count(partyid)

# Modifying Factor Order

# explore the average number of hours watching tv across religions:

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

# reordering the levels of relig using fct_reorder(). the default is median:

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

# using mutate instead of aes:

relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()

# how average age varies across reported income level:

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n())

ggplot(rincome_summary, aes(age,fct_reorder(rincome, age))) + geom_point()

#  pull “Not applicable” to the front with the other special levels:

ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

# reordering the factor by the y values associated with the largest x values (so the colors line up with the legend):

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

# order levels in bar charts:

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()

# Modifying Factor Levels

gss_cat %>% count(partyid)

# Let’s tweak the levels to be longer and use a parallel construction:

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) %>%
  count(partyid)

# To combine groups, you can assign multiple old levels to the same new level:

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%
  count(partyid)

# If you want to collapse a lot of levels, fct_collapse() is a useful variant of fct_recode(). For each new variable, you can provide a vector of old levels:

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)

# Sometimes you just want to lump together all the small groups to make a plot or table simpler. That’s the job of fct_lump():

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)

# use the n parameter to specify how many groups (excluding other) we want to keep:

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)


# Lovelace - Rasterization ------------------------------------------------

library(sf)

cycle_hire_osm = spData::cycle_hire_osm
cycle_hire_osm_projected = st_transform(cycle_hire_osm, "EPSG:27700")
raster_template = terra::rast(ext(cycle_hire_osm_projected),resolution = 1000,
                       crs = st_crs(cycle_hire_osm_projected)$wkt)

# 3 approaches to rasterization

# First, we create a raster representing the presence or absence of cycle hire points (known as presence/absence rasters):

ch_raster1 = terra::rasterize(cycle_hire_osm_projected, raster_template)

# fun = "length" can be used, in this case to count the number of cycle hire points in each grid cell:

ch_raster2 = terra::rasterize(cycle_hire_osm_projected, raster_template, 
                       fun = "length")

# to find the capacity in each grid cell:

ch_raster3 = terra::rasterize(cycle_hire_osm_projected, raster_template, 
                       field = "capacity", fun = sum, na.rm = TRUE)

# rasterization of lines

#After casting the polygon objects into a multilinestring, a template raster is created with a resolution of a 0.5 degree:

california = dplyr::filter(us_states, NAME == "California")
california_borders = st_cast(california, "MULTILINESTRING")
raster_template2 = terra::rast(ext(california), resolution = 0.5,
                        crs = st_crs(california)$wkt)

# By default "touch" is FALSE, but when changed to TRUE – all cells that are touched by a line or polygon border get a value:

california_raster1 = rasterize(california_borders, raster_template2,
                               touches = TRUE)

# Compare it to a polygon rasterization, with touches = FALSE by default, which selects only raster cells whose centroids are inside the selector polygon:



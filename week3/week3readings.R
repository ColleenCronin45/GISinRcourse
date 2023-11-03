
# setup -------------------------------------------------------------------

library(tidyverse)
library(nycflights13)

airlines

airports

planes

weather

# count primary keys to ensure they are unique identifiers

planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)

# mutating joins

# narrowing the dataset:

flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2

# add full airline name from airlines to flights2:

flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

# doing the same as above using mutate:

flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

# Understanding Joins

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

# inner join:

x %>% 
  inner_join(y, by = "key")

# one table has duplicate keys:

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)

left_join(x, y, by = "key")

# both tables have duplicate keys:

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

left_join(x, y, by = "key")

# natural join with no key specified:

flights2 %>% 
  left_join(weather)

# left join using by:

flights2 %>% 
  left_join(planes, by = "tailnum")

# joining tables where the keys have different names:

flights2 %>% 
  left_join(airports, c("dest" = "faa"))

flights2 %>% 
  left_join(airports, c("origin" = "faa"))

# exercises:

flights %>% 
  group_by(dest) %>% 
  mean(arr_delay)

airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

# find top 10 most popular destinations:

top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)

top_dest

# find each flight that went to one of those destinations:

flights %>% 
  filter(dest %in% top_dest$dest)

# match the the 10 days with highest average delays back to the flights:

flights %>% 
  semi_join(top_dest)

# after joining flights and planes, are there many flights that don't have a match in planes:

flights %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)

# exercises:

flights %>% 
  count(tailnum) %>% 
  filter(n > 100)

anti_join(flights, airports, by = c("dest" = "faa"))

anti_join(airports, flights, by = c("faa" = "dest"))

# SET OPERATORS

df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)

df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
)

intersect(df1, df2)

union(df1, df2)

setdiff(df1, df2)

setdiff(df2, df1)


# setup for Lovelace reading 2.2-2.4 --------------------------------------

install.packages("sf")
install.packages("terra")
install.packages("spData")
install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")

# this one didn't work for me:

remotes::install_github("r-tmap/tmap@v4")

vignette(package = "sf") # see which vignettes are available

vignette("sf1")          # an introduction to the package

world <- st_read(system.file("shapes/world.shp", package = "spData"))


class(world)

names(world)

plot(world)

summary(world["lifeExp"])

#  return an object containing only the first two rows and the first three columns:

world_mini = world[1:2, 1:3]

world_mini

world_dfr = st_read(system.file("shapes/world.shp", package = "spData"))

world_tbl = read_sf(system.file("shapes/world.shp", package = "spData"))

class(world_dfr)

class(world_tbl)

library(sp)
world_sp = as(world, "Spatial") # from an sf object to sp
# sp functions ...
world_sf = st_as_sf(world_sp)   # from sp to sf

# Basic Maps

plot(world[3:6])

plot(world["pop"])

# filter countries in Asia and combines them into a single feature:

world_asia = world[world$continent == "Asia", ]

asia = st_union(world_asia)

# plotting Asia over the world:

plot(world["pop"], reset = FALSE)

plot(asia, add = TRUE, col = "red")

# more plotting:

plot(world["continent"], reset = FALSE)

cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)

plot(st_geometry(world_cents), add = TRUE, cex = cex)

# mapping India:

india = world[world$name_long == "India", ]

plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)

plot(st_geometry(world_asia), add = TRUE)

# combining attributes and geometry:

lnd_point = st_point(c(0.1, 51.5))       # sfg object

lnd_geom = st_sfc(lnd_point, crs = "EPSG:4326")    # sfc object

lnd_attrib = data.frame(                           # data.frame object
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)

lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)    # sf object

lnd_sf

class(lnd_sf)

# creating XY (2D coordinates), XYZ (3D coordinates) and XYZM (3D with an additional variable, typically measurement accuracy) point types:

st_point(c(5, 2))                 # XY point
#> POINT (5 2)
st_point(c(5, 2, 3))              # XYZ point
#> POINT Z (5 2 3)
st_point(c(5, 2, 1), dim = "XYM") # XYM point
#> POINT M (5 2 1)
st_point(c(5, 2, 3, 1))           # XYZM point
#> POINT ZM (5 2 3 1)

# use matrices in the case of multipoint and linestrings:

# the rbind function simplifies the creation of matrices

multipoint_matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))

st_multipoint(multipoint_matrix)

linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))

st_linestring(linestring_matrix)

# use lists for the creation of multilinestrings, (multi-)polygons and geometry collections:

polygon_list = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))

st_polygon(polygon_list)

# polygon with a hole:

polygon_border = rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))

polygon_hole = rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))

polygon_with_hole_list = list(polygon_border, polygon_hole)

st_polygon(polygon_with_hole_list)

multilinestring_list = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)),                         rbind(c(1, 2), c(2, 4)))

st_multilinestring((multilinestring_list))

geometrycollection_list = list(st_multipoint(multipoint_matrix),                              st_linestring(linestring_matrix))

st_geometrycollection(geometrycollection_list)

#to combine two simple features into one object with two features, we can use st_sfc:

point1 = st_point(c(5, 2))

point2 = st_point(c(1, 3))

points_sfc = st_sfc(point1, point2)

points_sfc

# usually sfc and sfg objects will have the same geometry type:
polygon_list1 = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))

polygon1 = st_polygon(polygon_list1)

polygon_list2 = list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))

polygon2 = st_polygon(polygon_list2)

polygon_sfc = st_sfc(polygon1, polygon2)

st_geometry_type(polygon_sfc)

# create an sfc object from sfg objects with different geometry types:

point_multilinestring_sfc = st_sfc(point1, multilinestring1)

st_geometry_type(point_multilinestring_sfc)

# checking the crs:

st_crs(points_sfc)

# Set the CRS with an identifier referring to an 'EPSG' CRS code:
points_sfc_wgs = st_sfc(point1, point2, crs = "EPSG:4326")

st_crs(points_sfc_wgs) # print CRS 

# using sfheaders:

# create an sfg object:

v = c(1, 1)

v_sfg_sfh = sfheaders::sfg_point(obj = v)

v_sfg_sfh # printing without sf loaded

v_sfg_sf = st_point(v)

print(v_sfg_sf) == print(v_sfg_sfh)

# set the crs:

df_sf = sfheaders::sf_polygon(obj = df)

st_crs(df_sf) = "EPSG:4326"

# Is S2 turned on:

sf_use_s2()

# creating buffers with the engine turned off:

india_buffer_with_s2 = st_buffer(india, 1)

sf_use_s2(FALSE)

#> Spherical geometry (s2) switched off
#> 
india_buffer_without_s2 = st_buffer(india, 1)

# turn S2 back on:

sf_use_s2(TRUE)

## RASTERS

# create a spatraster object:

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")

my_rast = rast(raster_filepath)

class(my_rast)

# mapmaking:

plot(my_rast)

# read in a raster file:

single_raster_file = system.file("raster/srtm.tif", package = "spDataLarge")

single_rast = rast(raster_filepath)

# creating a raster from scratch:

new_raster = rast(nrows = 6, ncols = 6, 
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = 1:36)

# The SpatRaster class also handles multiple layers, which typically correspond to a single multispectral satellite file or a time-series of rasters:

multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")

multi_rast = rast(multi_raster_file)

multi_rast

# retrieves the number of layers stored:

nlyr(multi_rast)

# select layers:

multi_rast3 = subset(multi_rast, 3)

multi_rast4 = subset(multi_rast, "landsat_4")

# combine multiple spatraster objects into one:

multi_rast34 = c(multi_rast3, multi_rast4)


# Lovelace 4.2.4 ----------------------------------------------------------

# create points randomly scattered over the Earth:

set.seed(2018) # set seed for reproducibility

(bb = st_bbox(world)) # the world's bounds

random_df = data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]),
  y = runif(n = 10, min = bb[2], max = bb[4])
)  

random_points = random_df |> 
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") # set coordinates and CRS

# use spatial subsetting to create world_random, which contains only countries that contain random points:

world_random = world[random_points, ]

nrow(world_random)

# join world that has attribute data with random points:

random_joined = st_join(random_points, world["name_long"])

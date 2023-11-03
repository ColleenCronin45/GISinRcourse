library(tidyverse)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# calculate the median: copy and paste method:

median(df$a)
#> [1] -0.2457625
median(df$b)
#> [1] -0.2873072
median(df$c)
#> [1] -0.05669771
median(df$d)
#> [1] 0.1442633

# For Loop:

output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}

output

# Unlike 1:length(1), seq_along() does the right thing with a vector of length 0:

y <- vector("double", 0)

seq_along(y)

1:length(y)

# exercises:

#1

output <- vector("double", ncol(mtcars))

for (i in seq_along(mtcars)) {    
  output[[i]] <- mean(mtcars[[i]])
}

output

#2

output <- vector("character", ncol(nycflights13::flights))

for (i in seq_along(nycflights13::flights)) {    
  output[[i]] <- typeof(nycflights13::flights[[i]])
}

output

# For loops to modify an existing function:

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

##

for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

# Looping over names. If you’re creating named output, make sure to name the results vector like so:

results <- vector("list", length(x))
names(results) <- names(x)

#Iteration over the numeric indices is the most general form, because given the position you can extract both the name and the value:
  
  for (i in seq_along(x)) {
    name <- names(x)[[i]]
    value <- x[[i]]
  }

# Unknown output length:

# Solving by progressinvely growing the vector:

means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)

# A better solution: save the results in a list, then combine into a single vector after the loop:

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)

str(unlist(out))

#I’ve used unlist() to flatten a list of vectors into a single vector.

#Unknown sequence length - while loop:

for (i in seq_along(x)) {
  # body
}

# Equivalent to
i <- 1
while (i <= length(x)) {
  # body
  i <- i + 1 
}

# how many tries it takes to get three heads in a row:

flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}

flips

# For loops vs. functionals:

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# for loop to compute the mean of every column:

output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}

output

# if you are going to do this frequently, then extract it into a function:

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}

# If you want median and sd instead of mean:

col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  output
}
col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  output
}

# If you see this:

f1 <- function(x) abs(x - mean(x)) ^ 1
f2 <- function(x) abs(x - mean(x)) ^ 2
f3 <- function(x) abs(x - mean(x)) ^ 3

# you'd reduce it to this:

f <- function(x, i) abs(x - mean(x)) ^ i

# applying this rule to the problem above:

col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}

col_summary(df, median)

col_summary(df, mean)

# Using map function instead of for loops for the mean, median, sd problem above:

map_dbl(df, mean)

map_dbl(df, median)

map_dbl(df, sd)

# with the pipe:

df %>% map_dbl(df, mean)

df %>% map_dbl(df, median)

df %>% map_dbl(df, sd)

# unlike col_summary(), map preserves names:

z <- list(x = 1:3, y = 4:5)
map_int(z, length)

# SHORTCUTS: The following example splits up the dataset into 3 pieces (one for each value of cylinder) and fits the same linear model to each:

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))

# a one-sided function for an anonymous function:

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

# extracting a summary statistic like R^2:

models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)

# extracting named components using a string:

models %>% 
  map(summary) %>% 
  map_dbl("r.squared")

# using an integer to extract elements by position:

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)

# Using safely() with log example:

safe_log <- safely(log)
str(safe_log(10))

str(safe_log("a"))

# safely is designed to work with map:

x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)

# using transpose:

y <- y %>% transpose()
str(y)

# with errors, you’ll either look at the values of x where y is an error, or work with the values of y that are ok:

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]

y$result[is_ok] %>% flatten_dbl()

# possibly: 

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

# quietly:

x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

# mapping over multiple arguments:

mu <- list(5, 10, -3)
mu %>% 
  map(rnorm, n = 5) %>% 
  str()

# to vary the sd:

sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()

# using map2 to iterate over two vectors in parallel:

map2(mu, sigma, rnorm, n = 5) %>% str()

# Note that the arguments that vary for each call come before the function; arguments that are the same for every call come after.

# Like map(), map2() is just a wrapper around a for loop:
  
  map2 <- function(x, y, f, ...) {
    out <- vector("list", length(x))
    for (i in seq_along(x)) {
      out[[i]] <- f(x[[i]], y[[i]], ...)
    }
    out
  }

  #pmap for a list of arguments:
  
  n <- list(1, 3, 5)
  args1 <- list(n, mu, sigma)
  args1 %>%
    pmap(rnorm) %>% 
    str()
  
# it's better to name the arguments:
  
  args2 <- list(mean = mu, sd = sigma, n = n)
  args2 %>% 
    pmap(rnorm) %>% 
    str()
  
# the arguments are the same length, so store them in a dataframe:
  
  params <- tribble(
    ~mean, ~sd, ~n,
    5,     1,  1,
    10,     5,  3,
    -3,    10,  5
  )
  params %>% 
    pmap(rnorm)
  
# invoking different functions - varying the function itself:
  
  f <- c("runif", "rnorm", "rpois")
  param <- list(
    list(min = -1, max = 1), 
    list(sd = 5), 
    list(lambda = 10)
  )
  
# you can use invoke_map() to vary the function itself:
  
  invoke_map(f, param, n = 5) %>% str()

# use tribble() to make creating these matching pairs a little easier:
  
  sim <- tribble(
    ~f,      ~params,
    "runif", list(min = -1, max = 1),
    "rnorm", list(sd = 5),
    "rpois", list(lambda = 10)
  )
  sim %>% 
    mutate(sim = invoke_map(f, params, n = 10))  
  
# simple example of walk:
  
  x <- list(1, "a", 3)
  
  x %>% 
    walk(print)

# use pwalk() to save each file to the corresponding location on disk:
  
library(ggplot2)

  plots <- mtcars %>% 
    split(.$cyl) %>% 
    map(~ggplot(., aes(mpg, wt)) + geom_point())
  paths <- stringr::str_c(names(plots), ".pdf")
  
  pwalk(list(paths, plots), ggsave, path = tempdir()) 
  
# keep() and discard() keep elements of the input where the predicate is TRUE or FALSE respectively:

    iris %>% 
    keep(is.factor) %>% 
    str()
  
  iris %>% 
    discard(is.factor) %>% 
    str()

# some() and every() determine if the predicate is true for any or for all of the elements.
  
  x <- list(1:5, letters, list(10))
  
  x %>% 
    some(is_character)
  
  x %>% 
    every(is_vector)
  
# detect() finds the first element where the predicate is true; detect_index() returns its position.
  
  x <- sample(10)
  x
  
  x %>% 
    detect(~ . > 5)
  
  x %>% 
    detect_index(~ . > 5)
  
# head_while() and tail_while() take elements from the start or end of a vector while a predicate is true:
  
  x %>% 
    head_while(~ . > 5)
  
  x %>% 
    tail_while(~ . > 5)
  
# Reduce:
  
  dfs <- list(
    age = tibble(name = "John", age = 30),
    sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
    trt = tibble(name = "Mary", treatment = "A")
  )
  
  dfs %>% reduce(full_join)

# reduce to find the intersection:
  
  vs <- list(
    c(1, 3, 5, 6, 10),
    c(1, 2, 3, 7, 8, 10),
    c(1, 2, 3, 4, 8, 9, 10)
  )
  
  vs %>% reduce(intersect)

# accumulate: 
  
  x <- sample(10)
  x

  x %>% accumulate(`+`)

# Lovelace ----------------------------------------------------------------
  
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
  
install.packages("remotes")

remotes::install_github("r-tmap/tmap")
  library(tmap)    # for static and interactive maps
library(tmaptools)
  library(leaflet) # for interactive maps
  library(ggplot2) # tidyverse data visualization package 

nz_elev = rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))

# Static Maps:

# Add fill layer to nz shape
tm_shape(nz) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders() 

# tmap can store maps as objects:

map_nz = tm_shape(nz) + tm_polygons()
class(map_nz)

# you can add layers:

map_nz1 = map_nz +
  tm_shape(nz_elev) + tm_raster(alpha = 0.7)

# mapping the water:

nz_water = st_union(nz) |>
  st_buffer(22200) |> 
  st_cast(to = "LINESTRING")
map_nz2 = map_nz1 +
  tm_shape(nz_water) + tm_lines()

# adding high points:

map_nz3 = map_nz2 +
  tm_shape(nz_height) + tm_symbols()

# creating a metaplot:

tmap_arrange(map_nz1, map_nz2, map_nz3)

# changing the default aesthetics:

ma1 = tm_shape(nz) + tm_polygons(fill = "red")
ma2 = tm_shape(nz) + tm_polygons(fill = "red", fill_alpha = 0.3)
ma3 = tm_shape(nz) + tm_polygons(col = "blue")
ma4 = tm_shape(nz) + tm_polygons(lwd = 3)
ma5 = tm_shape(nz) + tm_polygons(lty = 2)
ma6 = tm_shape(nz) + tm_polygons(fill = "red", fill_alpha = 0.3,
                                 col = "blue", lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)

# tmap will not accept a numeric vector:

plot(st_geometry(nz), col = nz$Land_area)  # works
tm_shape(nz) + tm_fill(col = nz$Land_area) # fails

# it requires a character string describing the vector:

tm_shape(nz) + tm_fill(fill = "Land_area")

# value breaks:

tm_shape(nz) + tm_polygons(fill = "Median_income")

tm_shape(nz) + tm_polygons(fill = "Median_income",
                           fill.scale = tm_scale(breaks = c(0, 30000, 40000, 50000)))
tm_shape(nz) + tm_polygons(fill = "Median_income",
                           fill.scale = tm_scale(n = 10))
tm_shape(nz) + tm_polygons(fill = "Median_income",
                           fill.scale = tm_scale(values = "BuGn"))

# fill colors:

tm_shape(nz) + 
  tm_polygons("Median_income", fill.scale = tm_scale(values = "greens"))
tm_shape(nz) + 
  tm_polygons("Median_income", fill.scale = tm_scale(values = "yl_gn_bu"))

# divergent colors:

tm_shape(nz) + 
  tm_polygons("Median_income",
              fill.scale = tm_scale_continuous(values = "pu_gn_div", 
                                               midpoint = 28000))

# Legends:

legend_title = expression("Area (km"^2*")")
map_nza = tm_shape(nz) +
  tm_polygons(fill = "Land_area", fill.legend = tm_legend(title = legend_title))

# Legend location and position:

map_nza2 = tm_shape(nz) +
  tm_polygons(
    fill = "Land_area",
    fill.legend = tm_legend(
      title = legend_title,
      orientation = "landscape",
      position = tm_pos_out("center", "bottom")
    )
  )

# Layout:

map_nz + 
  tm_graticules() +
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scalebar(breaks = c(0, 100, 200), text.size = 1, position = c("left", "top")) +
  tm_title("New Zealand")

map_nz + tm_layout(scale = 4)
map_nz + tm_layout(bg.color = "lightblue")
map_nz + tm_layout(frame = FALSE)

# Facets:

urb_1970_2030 = urban_agglomerations |> 
  filter(year %in% c(1970, 1990, 2010, 2030))

tm_shape(world) +
  tm_polygons() +
  tm_shape(urb_1970_2030) +
  tm_symbols(fill = "black", col = "white", size = "population_millions") +
  tm_facets_wrap(by = "year", nrow = 2)

# Inset Maps:

# define the region of interest:

nz_region = st_bbox(c(xmin = 1340000, xmax = 1450000,
                      ymin = 5130000, ymax = 5210000),
                    crs = st_crs(nz_height)) |> 
  st_as_sfc()

# create a basemap:

nz_height_map = tm_shape(nz_elev, bbox = nz_region) +
  tm_raster(col.scale = tm_scale_continuous(values = "YlGn"),
            col.legend = tm_legend(position = c("left", "top"))) +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 1) +
  tm_scalebar(position = c("left", "bottom"))

# inset map creation:

nz_map = tm_shape(nz) + tm_polygons() +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 0.1) + 
  tm_shape(nz_region) + tm_borders(lwd = 3) +
  tm_layout(bg.color = "lightblue")

# Determining the aspect ratio:

library(grid)
norm_dim = function(obj){
  bbox = st_bbox(obj)
  width = bbox[["xmax"]] - bbox[["xmin"]]
  height = bbox[["ymax"]] - bbox[["ymin"]]
  w = width / max(width, height)
  h = height / max(width, height)
  return(unit(c(w, h), "snpc"))
}
main_dim = norm_dim(nz_region)
ins_dim = norm_dim(nz)

# Next, knowing the aspect ratios, we need to specify the sizes and locations of our two maps:

main_vp = viewport(width = main_dim[1], height = main_dim[2])

# making the inset twice as small as the main map:

ins_vp = viewport(width = ins_dim[1] * 0.5, height = ins_dim[2] * 0.5,
                  x = unit(1, "npc") - unit(0.5, "cm"), y = unit(0.5, "cm"),
                  just = c("right", "bottom"))

# combining the two maps:

grid.newpage()
print(nz_height_map, vp = main_vp)
pushViewport(main_vp)
print(nz_map, vp = ins_vp)

## noncontiguous inset maps

# set the coordinate system for the US:

us_states_map = tm_shape(us_states, projection = "EPSG:2163") + tm_polygons() + 
  tm_layout(frame = FALSE)

# create the two maps for HI and AK:

hawaii_map = tm_shape(hawaii) +
  tm_polygons() + 
  tm_title("Hawaii") +
  tm_layout(frame = FALSE, bg.color = NA, 
            title.position = c("LEFT", "BOTTOM"))
alaska_map = tm_shape(alaska) +
  tm_polygons() + 
  tm_title("Alaska") +
  tm_layout(frame = FALSE, bg.color = NA)

# combine and arrage the three maps:

us_states_map
print(hawaii_map, vp = grid::viewport(0.35, 0.1, width = 0.2, height = 0.1))
print(alaska_map, vp = grid::viewport(0.15, 0.15, width = 0.3, height = 0.3))

# animated maps:

urb_anim = tm_shape(world) + tm_polygons() + 
  tm_shape(urban_agglomerations) + tm_symbols(size = "population_millions") +
  tm_facets(by = "year", nrow = 1, ncol = 1, free.coords = FALSE)

# combine the maps and save as a gif:

tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 25)

# Interactive Maps:

tmap_mode("view")
map_nz

# change basemap:

map_nz + tm_basemap(server = "OpenTopoMap")

# interactive maps also work with facets:

world_coffee = left_join(world, coffee_data, by = "name_long")
facets = c("coffee_production_2016", "coffee_production_2017")
tm_shape(world_coffee) + tm_polygons(facets) + 
  tm_facets_wrap(nrow = 1, sync = TRUE)

# switch back to static:

tmap_mode("plot")

# Using Mapview instead of Tmap

install.packages("mapview")

mapview::mapview(nz)

library(mapview)
oberfranken = subset(franconia, district == "Oberfranken")
trails |>
  st_transform(st_crs(oberfranken)) |>
  st_intersection(oberfranken) |>
  st_collection_extract("LINESTRING") |>
  mapview(color = "red", lwd = 3, layer.name = "trails") +
  mapview(franconia, zcol = "district") +
  breweries

# googleway package is another option

# Mapdeck
## The package uses Mapbox access tokens, which you must register for before using the package. Note that the following block assumes the access token is stored in your R environment as MAPBOX=your_unique_key. This can be added with usethis::edit_r_environ().

library(mapdeck)
set_token(Sys.getenv("MAPBOX"))
crash_data = read.csv("https://git.io/geocompr-mapdeck")
crash_data = na.omit(crash_data)
ms = mapdeck_style("dark")
mapdeck(style = ms, pitch = 45, location = c(0, 52), zoom = 4) |>
  add_grid(data = crash_data, lat = "lat", lon = "lng", cell_size = 1000,
           elevation_scale = 50, colour_range = hcl.colors(6, "plasma"))

# Leaflet

pal = colorNumeric("RdYlBu", domain = cycle_hire$nbikes)
leaflet(data = cycle_hire) |> 
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircles(col = ~pal(nbikes), opacity = 0.9) |> 
  addPolygons(data = lnd, fill = FALSE) |> 
  addLegend(pal = pal, values = ~nbikes) |> 
  setView(lng = -0.1, 51.5, zoom = 12) |> 
  addMiniMap()

## Shiny

library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset 
ui = fluidPage(
  sliderInput(inputId = "life", "Life expectancy", 49, 84, value = 80),
  leafletOutput(outputId = "map")
)
server = function(input, output) {
  output$map = renderLeaflet({
    leaflet() |> 
      # addProviderTiles("OpenStreetMap.BlackAndWhite") |>
      addPolygons(data = world[world$lifeExp < input$life, ])})
}
shinyApp(ui, server)

# Other mapping packages

# combining rasters and vectors with sf and terra:

g = st_graticule(nz, lon = c(170, 175), lat = c(-45, -40, -35))
plot(nz_water, graticule = g, axes = TRUE, col = "blue")
terra::plot(nz_elev / 1000, add = TRUE, axes = FALSE)
plot(st_geometry(nz), add = TRUE)

# ggplot2

library(ggplot2)
g1 = ggplot() + geom_sf(data = nz, aes(fill = Median_income)) +
  geom_sf(data = nz_height) +
  scale_x_continuous(breaks = c(170, 175))
g1

# interactive mapping with ggplot2:

install.packages("plotly")

plotly::ggplotly(g1)

# ggspatial allows combining rasters and vectors:

install.packages("ggspatial")

library(ggspatial)
ggplot() + 
  layer_spatial(nz_elev) +
  geom_sf(data = nz, fill = NA) +
  annotation_scale() +
  scale_x_continuous(breaks = c(170, 175)) +
  scale_fill_continuous(na.value = NA)

# cartogram:

install.packages("cartogram")

library(cartogram)

# contiguous area cartograms:

nz_carto = cartogram_cont(nz, "Median_income", itermax = 5)
tm_shape(nz_carto) + tm_polygons("Median_income")

#non-contiguous area cartograms:

us_states2163 = st_transform(us_states, "EPSG:2163")
us_states2163_ncont = cartogram_ncont(us_states2163, "total_pop_15")
us_states2163_dorling = cartogram_dorling(us_states2163, "total_pop_15")
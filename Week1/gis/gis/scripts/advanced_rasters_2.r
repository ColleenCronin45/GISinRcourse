# Spatial autocorrelation!

# setup -------------------------------------------------------------------

library(ape)
library(pgirmess)
library(spdep)
library(gstat)
library(sf)
library(tmap)
library(tidyverse)

tmap_mode('view')

# Census shapefile:

census <- 
  st_read('data/raw/shapefiles/dc_census.geojson') %>% 
  st_transform(crs = 5070) %>% 
  set_names(
    names(.) %>% 
      tolower())

# The extent of Washington DC:

dc_extent <-
  c(xmin = 1609777,
    xmax = 1630412,
    ymin = 1912781,
    ymax = 1937182)

# Path to rasters:

raster_files <-
  list.files(
    'data/raw/rasters',
    pattern = 'tif$')

# Load rasters:

rasters <-
  file.path(
    'data/raw/rasters',
    raster_files) %>% 
  map(
    ~ terra::rast(.x)) %>% 
  set_names(
    str_remove(raster_files, '.tif'))

# Now you! In the code block above, crop impervious surface and canopy dc_extent
# and store the output as a raster stack.

# Remove unneeded files

rm(raster_files, dc_extent)

# sampling points ---------------------------------------------------------

# Generate regular point samples:

samples <- 
  rasters %>% 
  terra::spatSample(
    size = 1200,
    method= 'regular', 
    as.point = TRUE) %>% 
  st_as_sf()

# Now you! Generate a tmap that shows sampling points, canopy cover, and 
# impervious surface:



# testing spatial dependence, method 1 ------------------------------------

# Note: This method uses ape

# Generate a distance matrix, example:

distance_matrix_example <- 
  samples %>% 
  slice(1:4) %>% 
  terra::vect() %>% 
  terra::distance(.) %>% 
  as.matrix()

# Calculate the inverse of the distance matrix:

inverse_distance_matrix_example <- 
  1/distance_matrix_example

# Set diagonals to zero:

diag(inverse_distance_matrix_example) <- 0

# With all of the data:

distance_matrix <- 
  samples %>% 
  terra::vect() %>% 
  terra::distance(.) %>%
  as.matrix()

inverse_distance_matrix <- 
  1/distance_matrix

diag(inverse_distance_matrix) <- 0

# Calculate Moran's I with the ape package:

ape::Moran.I(
  samples$canopy_cover, 
  inverse_distance_matrix,
  alternative = 'greater')

# testing spatial dependence, method 2 ------------------------------------

# Note: This method uses the spdep package. 

# Remove NA values from census:

census_no_na <-
  census %>% 
  filter(!is.na(income))

# Define neighbors:

neighbors <- 
  census_no_na %>% 
  spdep::poly2nb()

# Weights for our neighbors:

neighbor_weights <-
  spdep::nb2listw(neighbors)

# Calculate Moran's I:

spdep::moran(
  x = census_no_na$income,
  listw = neighbor_weights, 
  n  = length(neighbor_weights$neighbours), 
  S0 = Szero(neighbor_weights)) %>% 
  pluck('I')

# Monte Carlo Moran's I test:

mc_test <- 
  spdep::moran.mc(
    x = census_no_na$income,
    listw = neighbor_weights, 
    nsim = 10000, 
    alternative = 'greater')

# Plot the test:

plot(mc_test)

# Now you! using census_no_na, conduct a Monte Carlo Moran's I test of incomes
# using a single chained analysis:

census_no_na

# assessing the scale of autocorrelation: Moran correlogram ---------------

# Plot correlogram in pgirmess:

pgirmess::correlog(
  coords = 
    census_no_na %>% 
    st_centroid() %>% 
    st_coordinates(),
  z = census_no_na$income,
  method = 'Moran',
  nbclass = 15) %>% 
  plot()

abline(h = 0, lty = 'dashed')

# Now you! Use pgirmess::correlog to examine the scale at which canopy cover
# is spatially autocorrelated:



# assessing the scale of autocorrelation: semivariogram -------------------

income_variogram <- 
  census_no_na %>% 
  gstat::variogram(income ~ 1, data = .)

plot(
  income_variogram, 
  pch = 19,
  col = 'black')

# Provide starting values to fit a variogram model:

model_variogram_starts <-
  gstat::vgm(
    psill = 1.5E9,
    model = 'Sph',
    nugget = 4E8,
    range = 5000)

fitted_variogram <-
  gstat::fit.variogram(
    income_variogram,
    model_variogram_starts)

plot(income_variogram, 
     model = fitted_variogram,
     pch = 19,
     col = 'black')

# Now you! Generate a variogram model of canopy cover values:

canopy_variogram <- 
  gstat::variogram(canopy_cover ~ 1, data = samples)

# when is spatial autocorrelation a problem? ------------------------------

# Join census to sampling data and subset to relevant locations (not water!):

samples_with_income <-
  samples %>% 
  st_join(
    census %>% 
      select(income)) %>% 
  filter(
    !is.na(income),
    !(canopy_cover == 0 & impervious_surface == 0))

simple_model <- 
  lm(canopy_cover ~ income, 
     data = samples_with_income)

summary(simple_model)

samples_with_income %>% 
  ggplot(aes(x = income, y = canopy_cover)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x)

samples_with_income$residuals <-
  simple_model$residuals

# Is there evidence of spatial autocorrelation in the residuals?

pgirmess::correlog(
  coords = 
    st_coordinates(samples_with_income),
  z = samples_with_income$residuals,
  method = 'Moran',
  nbclass = 15) %>% 
  plot()

abline(h = 0, lty = 'dashed')

# Now you! Determine the minimum distance between two different sample points
# in sampling:

samples

# interpolation - spatial autocorrelation is useful! ----------------------

# Blank raster:

blank_stars <-
  rasters$canopy_cover %>%  
  terra::aggregate(8) %>%
  stars::st_as_stars() %>% 
  set_names('canopy_cover')

# Inverse-distance-weighting (IDW) model:

idw_model <-
  gstat(formula = canopy_cover ~ 1, 
        data = samples) %>% 
  
  # IDW prediction (returned as SpatRaster):
  
  predict(newdata = blank_stars) %>% 
  select(var1.pred) %>% 
  terra::rast()

# Plot the IDW:

tm_shape(rasters$canopy_cover) +
  tm_raster(palette = 'Greens') + 
  
  idw_model %>% 
  tm_shape(name = 'idw') +
  tm_raster(
    title = 'idw_prediction',
    palette = 'Greens') +
  
  tm_shape(samples) +
  tm_dots(size = 0.005)

# Kriging model:

krige_model <- 
  gstat(formula = canopy_cover ~ 1, 
        model = fitted_canopy_variogram, 
        data = samples) %>% 
  predict(newdata =  blank_stars) %>% 
  select(var1.pred) %>% 
  terra::rast()

# Plotting Kriging results

tm_shape(rasters$canopy_cover) +
  tm_raster(palette = 'Greens') +
  
  krige_model %>% 
  tm_shape(name = 'krige') +
  tm_raster(
    title = 'krige_prediction',
    palette = 'Greens') +
  
  tm_shape(samples) +
  tm_dots(size = 0.005)

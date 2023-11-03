# Advanced rasters I: Distance, density, scale, and sampling (urban heat
# islands)

# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)
library(spatstat)

source('scripts/source_script_crime_and_equity.R')

# Set tmap mode for the whole document:

tmap_mode('view')

# Set ggplot mode for the whole document:

theme_set(
  theme_bw())

# Read in maximum July temperatures, 30-year average:

tmax_start <-
  terra::rast('data/raw/rasters/dc_tmax_july.tif') %>% 
  terra::project(
    terra::crs(rasters)
  ) %>% 
  terra::crop(rasters)

# Quick look:

tm_shape(tmax_start) +
  tm_raster() +
  
  tm_shape(dc_land) +
  tm_polygons()

# resampling rasters ------------------------------------------------------

# Resample tmax_start to the same resolution as rasters and mask:

tmax <-
  tmax_start %>% 
  terra::resample(rasters) %>% 
  terra::mask(rasters$canopy)

# Explore land cover and tmax:

rasters$imp %>% 
  tm_shape(name = 'Impervious surface') +
  tm_raster(
    title = 'Percent impervious',
    style = 'cont',
    palette = 'YlOrRd') +
  
  rasters$canopy %>% 
  tm_shape(name = 'Canopy cover') +
  tm_raster(
    title = 'Percent canopy cover',
    style = 'cont',
    palette = 'Greens') +
  
  tmax %>% 
  tm_shape(name = 'Maximum July temperature') +
  tm_raster(
    title = 'Maximum July temperature (C)',
    style = 'cont',
    palette = 'Spectral')

# Now you! Resample the canopy cover layer to the same resolution tmax_raw:

canopy_resampled <-
  rasters$canopy %>% 
  terra::resample(tmax_start)

# Mask tmax_start to canopy_resampled:

tmax_masked <-
  tmax_start %>% 
  terra::mask(canopy_resampled) 

# Plot the resampled canopy cover and maximum temperature rasters:

canopy_resampled %>% 
  tm_shape(name = 'Canopy cover') +
  tm_raster(
    title = 'Percent canopy cover',
    style = 'cont',
    palette = 'Greens') +
  
  tmax_masked %>% 
  tm_shape(name = 'Maximum July temperature') +
  tm_raster(
    title = 'Maximum July temperature (C)',
    style = 'cont',
    palette = '-Spectral')

# Now you! The raster dem_dc has a resolution of 10m. Resample the raster
# to the same resolution of rasters$land:

rasters$dem <- 
  dem_dc %>% 
  terra::resample(rasters$land)

# Plot the digital elevation model:

rasters$dem %>% 
  tm_shape() +
  tm_raster(palette = terrain.colors(n = 10),
            alpha = 0.6)

# Slope:

slope <- 
  terra::terrain(
    rasters$dem,
    v = 'slope',
    unit = "radians")

# Aspect:

aspect <- 
  terra::terrain(
    rasters$dem,
    v = 'aspect',
    unit = "radians")

# Hillshade:

hillshade <-
  terra::shade(
    slope,
    aspect,
    angle = 45,
    direction = 135)

# Plot DEM, now with hillshade!

hillshade %>% 
  tm_shape() +
  tm_raster(
    palette = 
      gray.colors(
        n = 7, 
        start = 0, 
        end = 1),
    style = 'cont',
    alpha = 0.9,
    legend.show = FALSE) +
  
  rasters$dem %>% 
  tm_shape(name = 'Elevation') +
  tm_raster(
    title = 'Elevation (m)',
    palette = terrain.colors(n = 10),
    alpha = .7) 

# sampling interlude ------------------------------------------------------

# Convert resampled canopy and masked tmax to a raster stack:

list(canopy_resampled, tmax_masked) %>% 
  terra::rast() %>% 
  
  # Sample (and extract!):
  
  terra::spatSample(
    size = 200,
    na.rm = TRUE,
    method = 'random') %>% 
  as_tibble()

# A (non-spatial) ggplot scatterplot that displays the relationship between 
# canopy cover (canopy_resampled, x-axis) and maximum July temperatures 
# (tmax_masked; y-axis):

# Sample (and extract!):

list(canopy_resampled, 
     tmax_masked) %>% 
  terra::rast() %>% 
  
  # Sample (and extract!):
  
  terra::spatSample(
    size = 200,
    na.rm = TRUE,
    method = 'random') %>% 
  as_tibble() %>% 
  
  # Plot the output:
  
  ggplot(
    aes(x = canopy, y = tmax)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x)

# Now you! Modify the data being piped into the plot below to display the
# relationship between canopy cover (canopy_resampled, x-axis) and maximum July
# temperatures (tmax_masked; y-axis):

list(
  elevation =
    rasters$dem %>% 
    terra::resample(tmax_masked),
  tmax_masked) %>% 
  terra::rast() %>% 
  
  # Sample (and extract!):
  
  terra::spatSample(
    size = 200,
    na.rm = TRUE,
    method = 'random') %>% 
  as_tibble() %>% 
  
  # Plot the output:
  
  ggplot(
    aes(x = elevation, y = tmax)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x)

# rasterize ---------------------------------------------------------------

# Now you! Modify the below such that it generates a reclassified raster where
# developed land = 1 and all other values = NA:

nlcd_developed <-
  nlcd_key %>% 
  transmute(
    id,
    becomes = 
      if_else(
        id %in% 21:24,
        1,
        NA_real_)) %>% 
  as.matrix() %>% 
  terra::classify(rasters$nlcd, rcl = .) %>% 
  terra::resample(canopy_resampled)

# Plot developed land:

nlcd_developed %>% 
  tm_shape() +
  tm_raster(palette = 'orange')

# Now you! Modify nlcd_developed above, resampling the raster to same
# resolution as canopy_resampled.

# Rasterize with stats:

n_trees <-
  dc_street_trees %>% 
  st_transform(
    crs = st_crs(nlcd_developed)) %>% 
  terra::rasterize(
    nlcd_developed,
    fun = length,
    background = 0)

# Maximum July temperatures:

tmax_masked %>% 
  terra::mask(nlcd_developed) %>% 
  tm_shape(name = 'Maximum July temperature') +
  tm_raster(
    title = 'Maximum July temperature (C)',
    style = 'cont',
    palette = '-Spectral') +
  
  # Add number of street trees per 800 m x 800 m raster cell:
  
  n_trees %>% 
  terra::mask(nlcd_developed) %>% 
  tm_shape(name = 'Number of street trees') +
  tm_raster(
    title = 'n street trees',
    style = 'cont',
    palette = 'Greens') 

# Now you! Modify the below such that the total tree crown area per raster cell
# is returned (see ?terra::rasterize):

crown_area <-
  dc_street_trees %>% 
  st_transform(crs = st_crs(nlcd_developed)) %>% 
  terra::rasterize(
    nlcd_developed,
    field = 'crown_area',
    fun = length,
    background = 0)

# Plot tmax, masked to nlcd_developed:

tmax_masked %>% 
  terra::mask(nlcd_developed) %>% 
  tm_shape(name = 'Maximum July temperature') +
  tm_raster(
    title = 'Maximum July temperature (C)',
    style = 'cont',
    palette = '-Spectral') +
  
  # Add crown area of street trees per 800 m x 800 m raster cell:
  
  crown_area %>% 
  terra::mask(nlcd_developed) %>% 
  tm_shape(name = 'Street tree coverage') +
  tm_raster(
    title = 'Crown area',
    style = 'cont',
    palette = 'Greens')

# Now you! Modify the below to generate a (non-spatial) ggplot scatterplot that
# displays the relationship between crown area (x-axis) and maximum July
# temperatures (tmax_masked; y-axis):

list(
  crown = crown_area,
  tmax = tmax_masked) %>% 
  terra::rast() %>% 
  terra::mask(nlcd_developed) %>% 
  
  # Sample 200 points (and extract!):
  
  terra::spatSample(
    size = 200,
    na.rm = TRUE,
    method = 'random') %>% 
  as.tibble() %>% 
  
  # Your answer here!
  
  # Plot the output:
  
  ggplot(
    aes(x = crown, 
        y = tmax)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x)

# density -----------------------------------------------------------------

# Projected DC street trees:

trees_coords <- 
  dc_street_trees %>% 
  st_transform(
    crs = st_crs(rasters$land)) %>% 
  st_coordinates()

# Convert dc_land to an "owin" object (object window for point patterns):

dc_owin <-
  spatstat.geom::as.owin(census)

# Convert trees_coords to a point pattern (ppp) object:

trees_ppp <-
  spatstat.geom::ppp(
    x = trees_coords[, 1], 
    y = trees_coords[, 2], 
    window = dc_owin)

# Convert the points to density:

tree_density <-
  stats::density(trees_ppp, bw = 'nrd')

# Use the stars package to create a "stars" raster:

density_stars <-
  stars::st_as_stars(tree_density) %>% 
  st_set_crs(
    st_crs(dc_land))

# Now you! Reproduce the above code in a single chained analysis:

density_stars<-
  dc_street_trees %>% 
  
# Projected DC street trees as coordinates:
  
  st_transform(crs = st_crs(rasters$land)) %>% 
  st_coordinates() %>% 
  
  #convert trees_coords to a point pattern (ppp) object:
  
  spatstat.geom::ppp(
    x + .[, 1],
    y = .[, 2],
    window = spatstat.geom::as.owin(census)) %>% 
  
  # convert the points to density:
  
  stats::density(bw = 'nrd') %>% 

#use the stars packageto create a "stars" raster:

stars::st_as_stars() %>% 
  st_set_crs(
    st_crs(dc_land))

# Plot the data:

tm_shape(density_stars,
         name = 'Street trees') +
  tm_raster(title = 'Street tree density',
            style = 'kmeans',
            n = 10,
            palette = 'YlGn')

# Convert to a raster object:

density_terra <-
  density_stars %>% 
  terra::rast()

names(density_terra) <- 
  'street_tree_dens'

# Plot the relationship between street tree density and July maximum
# temperatures: 

list(
  street_trees = 
    density_terra %>% 
    terra::resample(tmax_masked),
  tmax = tmax_masked) %>% 
  terra::rast() %>% 
  terra::mask(nlcd_developed) %>% 
  
  # Sample 200 points (and extract!):
  
  terra::spatSample(
    size = 200,
    na.rm = TRUE,
    method = 'random') %>% 
  as_tibble() %>% 
  
  # Plot the output:
  
  ggplot(
    aes(x = street_trees, y = tmax)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x)

# distance ----------------------------------------------------------------

# Calculate distance to water:

water_distance <-
  water %>% 
  terra::vect() %>% 
  terra::distance(rasters$land, .) %>% 
  terra::mask(rasters$land)

# Resample water to the tmax_masked 

water_distance_resampled <-
  water_distance %>% 
  terra::resample(tmax_masked)

# Plot tmax layer, masked to nlcd_developed:

tmax_masked %>% 
  tm_shape(name = 'Maximum July temperature') +
  tm_raster(
    title = 'Maximum July temperature (C)',
    style = 'cont',
    palette = '-Spectral') +
  
  # Add water distance:
  
  tm_shape(water_distance_resampled,
           name = 'Distance to water') + 
  tm_raster(
    title = 'Distance to water',
    style = 'cont',
    palette = '-Spectral')

# Now you! Add a trendline to the plot below:

list(
  distance_to_water = water_distance_resampled,
  tmax = tmax_masked) %>% 
  terra::rast() %>% 
  
  # Sample 200 points (and extract!):
  
  terra::spatSample(
    size = 200,
    na.rm = TRUE,
    method = 'random') %>% 
  as_tibble() %>% 
  
  # Plot the output:
  
  ggplot(
    aes(x = distance_to_water, y = tmax)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x)

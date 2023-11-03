# Memory and shapefiles code

# setup -------------------------------------------------------------------

library(bench)
library(lobstr)
library(sf)
library(tmap)
library(tidyverse)

# Load the source script:

source("gis/scripts/source_script.r")

# Set the ggplot theme for our entire session:

theme_set(
  theme_void())

# Set the tmap_mode for our entire session:

tmap_mode("view")

# Remove functions we will not use from the global environment:

rm(lonlat_to_utm, universal_plot_theme)

# read and pre-process data -----------------------------------------------

shapes <- 
  
  # Define a vector of file paths to read in:
  
  file.path(
    "data/raw/shapefiles",
    c("states.shp", 
      "counties.geojson",
      "census.shp",
      "sick_birds.geojson")) %>% 
  
  # Read and pre-process files:
  
  map(
    ~ st_read(.x, quiet = TRUE) %>% 
      clean_names() %>% 
      st_transform(crs = 5070)) %>% 
  
  # Assign names to each list item:
  
  set_names("states", 
            "counties",
            "census", 
            "birds")

# Remove clean_names from the global environment:

rm(clean_names)

# A single point collected in EPSG 4326 and transformed to EPSG 5070:

classroom <-
  tibble(
    x = -77.075212,
    y = 38.90677) %>% 
  st_as_sf(coords = c("x", "y"), 
           crs = 4326) %>% 
  st_transform(5070)

# memory evaluation tools -------------------------------------------------

# The memory allocated to counties and states and census:

obj_size(shapes$counties)

obj_size(shapes$states)

obj_size(shapes$census)

# Benchmarking data reading operation for counties:

mark(
  st_read("data/raw/shapefiles/counties.geojson", quiet = TRUE),
  iterations = 5)

# Subset benchmark output to columns of interest:

mark(
  st_read("data/raw/shapefiles/counties.geojson", quiet = TRUE),
  iterations = 5)  %>% 
  select(median, mem_alloc)

# Benchmarking data reading operation for states:

mark(
  st_read("data/raw/shapefiles/states.shp", quiet = TRUE),
  iterations = 5) %>% 
  select(median, mem_alloc)

# Benchmarking data reading operation for census:

mark(
  st_read("data/raw/shapefiles/census.shp", quiet = TRUE),
  iterations = 5) %>% 
  select(median, mem_alloc)

# tabular subsets, subset fields ------------------------------------------

# Get character vector of variable names for counties:

names(shapes$counties)

# Subset `shapes$counties` to the fields geoid (primary key) and state and 
# assign `counties` to the global environment:

counties <- 
  shapes$counties %>% 
  select(geoid, state_name)

# Look at the amount of memory allocated to the original and reduced 
# shapefiles:

obj_size(shapes$counties)

obj_size(counties)

# Subset `shapes$states` to the fields of interest and assign `states` to
# the global environment:

states <-
  shapes$states %>% 
  select(geoid:region)

# Look at the amount of memory allocated to the original and reduced 
# shapefiles:

obj_size(shapes$states)

obj_size(states)

# Subset `shapes$census` to the fields of interest and assign `census` to
# the global environment:

census <- 
  shapes$census %>%
  select(geoid, population)

# Look at the amount of memory allocated to the original and reduced 
# shapefiles:

obj_size(shapes$census)

obj_size(census)

# Free up memory by removing `shapes$census`, `shapes$states`, and `shapes$counties` from the global environment:

shapes$census <- NULL

shapes$states <- NULL

shapes$counties <- NULL

# tabular subsets, drop geometries ----------------------------------------

# Memory allocated to states:

obj_size(states)

# Drop geometries using tidyverse tools:

states %>% 
  as_tibble() %>% 
  select(!geometry)

# Drop geometries using sf tools:

states %>% 
  st_drop_geometry() %>% 
  as_tibble()

# Evaluate the memory allocated to states after dropping geometries:

obj_size(
  states %>% 
    st_drop_geometry() %>% 
    as_tibble())

# How many features are present in the states shapefile?

nrow(states)

# What type of geometry is the states shapefile?

st_geometry_type(
  states,
  by_geometry = FALSE)

# How many nodes are in the states shapefile?

mapview::npts(states)

# How many nodes are in the counties shapefile?

mapview::npts(counties)

# How many nodes are in the census shapefile?

mapview::npts(census)

# tabular subset, subset features -----------------------------------------

# Get character vector of variable names for states:

names(states)

# Get a character vector of the unique region names in states:

states %>% 
  distinct(region) %>% 
  pull()

# Plot states in the Norteast[sic] and South regions:

states %>% 
  filter(region %in% c("Norteast", "South")) %>% 
  ggplot() +
  geom_sf()

# Plot states, other than Oklahoma and Texas, in the Norteast[sic] and South
# regions:

states %>% 
  filter(region %in% c("Norteast", "South"),
         !name %in% c("Oklahoma", "Texas")) %>% 
  ggplot() +
  geom_sf()

# Subset states to features in the Norteast[sic] and South regions where the
# name of the state is not Oklahoma or Texas and globally assign the name
# states_east to the resultant object:

states_east <- 
  states %>% 
  filter(region %in% c("Norteast", "South"),
         !name %in% c("Oklahoma", "Texas"))

# Compare the memory allocated to states and states_east:

obj_size(states)

obj_size(states_east)

# Subset counties to just those in the District of Columbia, Maryland, and
# Virginia:

counties_dmv <-
  counties %>% 
  filter(state_name %in% c("District of Columbia", "Maryland", "Virginia"))

# Compare the memory allocated to counties and counties_dmv:

obj_size(counties)

obj_size(counties_dmv)

# Free up memory by removing counties from your global environment:

rm(counties)

# Use the function ls() to view current global assignments:

ls()

# spatial aggregation -----------------------------------------------------

# Combine the conterminous US into a single polygon:

conterminous_us <-
  states %>% 
  st_union(is_coverage = TRUE)

# Plot the conterminous US:

conterminous_us %>% 
  ggplot() +
  geom_sf()

# Compare the memory allocated to states and the conterminous US:

obj_size(states)

obj_size(conterminous_us)

# Observe the number of nodes in the states and the conterminous US 
# shapefiles:

mapview::npts(states)

mapview::npts(conterminous_us)

# Combine counties geometries (counties_dmv) to generate a new shapefile
# of the states DC, MD, and VA:

states_dmv <-
  counties_dmv %>% 
  summarize(
    geometry = st_union(geometry),
    .by = state_name)

# Compare the memory allocated to states_dmv and counties_dmv:

obj_size(states_dmv)

obj_size(counties_dmv)

# Compare the number of nodes in states_dmv and counties_dmv:

mapview::npts(states_dmv)

mapview::npts(counties_dmv)

# Make map of the conterminous US and states_dmv:

conterminous_us %>% 
  ggplot() +
  geom_sf(fill = "#eaeaea") +
  geom_sf(
    data = states_dmv,
    fill = "#fdda0d")

# Free up memory by removing conterminous_us from your global environment:

rm(conterminous_us)

# Use the function ls() to view current global assignments:

ls()

# simplify spatial polygons -----------------------------------------------

# Subset states_dmv to the state of Virginia and assign to the globally 
# assign wth the name virginia:

virginia <-
  states_dmv %>% 
  filter(state_name == "Virginia")

# Plot virginia:

virginia %>% 
  ggplot() +
  geom_sf()

# Plot virginia from the states object:

states %>% 
  filter(name == "Virginia") %>% 
  ggplot() +
  geom_sf()

# Combare the number of nodes in virginia and the states object subset to
# Virginia:

mapview::npts(virginia)

states %>% 
  filter(name == "Virginia") %>% 
  mapview::npts()

# Compare the memory allocated to virginia and the states object subset to
# Virginia:

obj_size(virginia)

states %>% 
  filter(name == "Virginia") %>% 
  obj_size()

# simplifying single features ---------------------------------------------

# Plot virginia, simplified such that no nodes are closer than 2km:

virginia %>% 
  st_simplify(dTolerance = 2000) %>% 
  ggplot() +
  geom_sf()

# Plot the original and simplified virginia objects within a single, 
# faceted plot:

virginia %>% 
  mutate(shape = "fine resolution") %>% 
  bind_rows(
    virginia %>% 
      st_simplify(dTolerance = 2000) %>% 
      mutate(shape = "coarse resolution")) %>% 
  ggplot() +
  geom_sf() +
  facet_wrap(~ shape)

# Compare the memory allocated to the original and simplified virginia 
# shapefiles:

obj_size(virginia)

virginia %>% 
  st_simplify(dTolerance = 2000) %>% 
  obj_size()

# Compare the number of points in the original and simplified virginia 
# shapefiles:

mapview::npts(virginia)

virginia %>% 
  st_simplify(dTolerance = 2000) %>% 
  mapview::npts()

# Transform virginia to the unprojected CRS EPSG 4326:

virginia_4326 <-
  virginia %>% 
  st_transform(crs = 4326)

# Compare execution time between projected and unprofected CRSs:

mark(
  virginia %>% 
    st_simplify(dTolerance = 2000)) %>% 
  select(median, mem_alloc)

mark(
  virginia_4326 %>% 
    st_simplify(dTolerance = 2000)) %>% 
  select(median, mem_alloc)

# Remove global assignments no longer needed:

rm(virginia, virginia_4326)

# Use the function ls() to view current global assignments:

ls()

# simplifying multiple features -------------------------------------------

# Simplify states such that no nodes are closer than 10km:

states %>% 
  st_simplify(dTolerance = 10000) %>% 
  ggplot() +
  geom_sf()

# Look more closely by subsetting the resultant object to a single region:

states %>% 
  st_simplify(dTolerance = 10000) %>% 
  filter(region == 'Norteast') %>% 
  ggplot() +
  geom_sf()

# Watch what happens at extreme minimum node distances:

states %>% 
  st_simplify(dTolerance = 1E5) %>% 
  ggplot() +
  geom_sf()

# Use rmapshaper::ms_simplify to simplify shapefiles that contain multiple
# features. Here, we keep 50% of the nodes in the states shapefile:

states %>% 
  rmapshaper::ms_simplify(keep = 0.5) %>% 
  ggplot() +
  geom_sf()

# Here, we keep 5% of the nodes in the states shapefile:

states %>% 
  rmapshaper::ms_simplify(keep = 0.05) %>% 
  ggplot() +
  geom_sf()

# This has a measurable effect on the amount of memory allocated to 
# objects:

obj_size(states)

states %>% 
  rmapshaper::ms_simplify(keep = 0.5) %>% 
  obj_size()

states %>% 
  rmapshaper::ms_simplify(keep = 0.05) %>% 
  obj_size()

# Remove global assignments no longer needed:

rm(states)

# Use the function ls() to view current global assignments:

ls()

# spatial joins, mutating -------------------------------------------------

# Join a polygon to a point, spatially:

classroom %>% 
  st_join(states_east)

# st_join is a mutating join for shapefiles:

counties_dmv %>% 
  st_join(
    states_east %>% 
      select(name))

# But do the state names always match?

counties_dmv %>% 
  st_join(
    states_east %>% 
      select(name)) %>% 
  filter(state_name != name)

# Observe the number of rows in the original and joined shapefiles:

nrow(counties_dmv)

counties_dmv %>% 
  st_join(
    states_east %>% 
      select(name)) %>% 
  nrow()

# Maintain only the features with the greatest area of overlap:

counties_dmv %>% 
  st_join(
    states_east %>% 
      select(name),
    largest = TRUE)

# Verify that this worked:

counties_dmv %>% 
  st_join(
    states_east %>% 
      select(name),
    largest = TRUE) %>% 
  filter(state_name != name) %>% 
  nrow()

# Maintain only matching features (inner join):

census %>% 
  st_join(
    states_dmv %>% 
      filter(state_name == "Maryland"),
    left = FALSE)

# Plot the results:

census %>% 
  st_join(
    states_dmv %>% 
      filter(state_name == "Maryland"),
    left = FALSE) %>% 
  ggplot() +
  geom_sf(
    aes(fill = population),
    color = NA)

# st_intersection, create a buffer around classroom point:

classroom_buffer <-
  classroom %>% 
  st_buffer(dist = 2000)

# View the buffer as a tmap:

tm_shape(classroom_buffer) + 
  tm_borders(
    col = "red",
    lwd = 2)

# What happens when we try to use st_join to subset these data?

census %>% 
  st_join(
    classroom_buffer, 
    left = FALSE) %>% 
  
  # Add census layer to map:
  
  tm_shape() + 
  tm_polygons(
    col = "population",
    palette = "viridis",
    alpha = 0.4) +
  
  # Add buffer layer as an overlay:
  
  tm_shape(classroom_buffer) + 
  tm_borders(
    col = "red",
    lwd = 2)

# st_intersection subsets shapes to only nodes within the source shape:

census %>% 
  st_intersection(classroom_buffer) %>% 
  
  # Add census layer to map:
  
  tm_shape() + 
  tm_polygons(
    col = "population",
    palette = "viridis",
    alpha = 0.4) +
  
  # Add buffer layer as an overlay:
  
  tm_shape(classroom_buffer) + 
  tm_borders(
    col = "red",
    lwd = 2)

# Remove global assignments no longer needed:

rm(classroom, counties_dmv, states_east)

# Use the function ls() to view current global assignments:

ls()

# spatial joins, filtering ------------------------------------------------

# View sick birds and the distance buffer around the Georgetown classroom:

tm_shape(shapes$birds) +
  tm_dots() +
  
  tm_shape(classroom_buffer) + 
  tm_borders(
    col = "red",
    lwd = 2)

# st_filter works great for subsetting points to those inside of polygon
# shapefiles!

shapes$birds %>% 
  st_filter(classroom_buffer) %>% 
  tm_shape() +
  tm_dots() +
  
  tm_shape(classroom_buffer) + 
  tm_borders(
    col = "red",
    lwd = 2)

# The same is not the case for subsetting to polygons shapes to those 
# inside of other polygons:

census %>% 
  st_filter(
    states_dmv %>% 
      filter(state_name == "District of Columbia")) %>% 
  tm_shape() +
  tm_polygons(
    col = "population",
    palette = "viridis",
    alpha = 0.4) +
  
  states_dmv %>% 
  filter(state_name == "District of Columbia") %>% 
  tm_shape() + 
  tm_borders(
    col = "red",
    lwd = 2)

# Instead, I suggest using st_intersection, but removing all of the fields
# therein:

census %>% 
  st_intersection(
    states_dmv %>% 
      filter(state_name == "District of Columbia")) %>% 
  tm_shape() +
  tm_polygons(
    col = "population",
    palette = "viridis",
    alpha = 0.4) +
  
  states_dmv %>% 
  filter(state_name == "District of Columbia") %>% 
  tm_shape() + 
  tm_borders(
    col = "red",
    lwd = 2)

# You could mimic a true filtering join by removing all fields:

census %>% 
  st_intersection(
    states_dmv %>% 
      filter(state_name == "District of Columbia") %>% 
      select(!everything())) %>% 
  tm_shape() +
  tm_polygons(
    col = "population",
    palette = "viridis",
    alpha = 0.4) +
  
  states_dmv %>% 
  filter(state_name == "District of Columbia") %>% 
  tm_shape() + 
  tm_borders(
    col = "red",
    lwd = 2)

# Joins & the CRS, transform the census and classroom_buffer shapefiles to EPSG 4326:

census_4326 <-
  census %>% 
  st_transform(crs = 4326)

classroom_buffer_4326 <-
  classroom_buffer %>% 
  st_transform(crs = 4326)

# Compare the length of time and memory allocate to st_join with both
# shapefiles:

mark(
  census %>% 
    st_join(
      classroom_buffer, 
      left = FALSE)) %>% 
  select(median, mem_alloc)

mark(
  census_4326 %>% 
    st_join(
      classroom_buffer_4326, 
      left = FALSE)) %>% 
  select(median, mem_alloc)

# Running st_intersection with projected and unprojected data:

mark(
  census %>% 
    st_intersection(classroom_buffer)) %>% 
  select(median, mem_alloc)

mark(
  census_4326 %>% 
    st_intersection(classroom_buffer_4326)) %>% 
  select(median, mem_alloc)

# Remove global assignments no longer needed:

rm(census_4326,
   classroom_buffer,
   classroom_buffer_4326,
   shapes)

# Use the function ls() to view current global assignments:

ls()

# summarizing -------------------------------------------------------------

# Join states_dmv to census:

census_states <- 
  census %>% 
  st_join(
    states_dmv %>% 
      select(state_name),
    largest = TRUE)

# How long and how much memory does it take to summarize as a shapefile?

mark(
  census_states %>% 
    summarize(
      population = sum(population, na.rm = TRUE),
      .by = state_name)) %>% 
  select(median, mem_alloc)

# How long and how much memory does it take to summarize as a tibble?

mark(
  census_states %>% 
    as_tibble() %>% 
    summarize(
      population = sum(population, na.rm = TRUE),
      .by = state_name)) %>% 
  select(median, mem_alloc)



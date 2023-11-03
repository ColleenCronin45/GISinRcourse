# Source script for categorization tutorial:

# a simple example dataset ------------------------------------------------

example_df <-
  tibble(
    numbers = seq(0, 100, by = 10),
    fruit = fruit[1:11])

example_df_with_na <-
  example_df %>% 
  mutate(
    numbers = na_if(numbers, 20),
    fruit = na_if(fruit, "bell pepper"))

# data loading, read tabular data -----------------------------------------

# Classification scheme for the National Land Cover Dataset:

nlcd_key <- 
  read_rds('data/processed/birds_cicadas_lc.rds') %>% 
  pluck('nlcd_key')

# Violent crimes in the District of Columbia:

crime <- 
  read_csv("data/raw/dc_crimes.csv") %>% 
  
  # Subset to violent crimes:
  
  filter(offense_group == "violent") %>% 
  
  # Subset to variables of interest:
  
  select(id, offense, longitude:latitude) %>% 
  
  # Convert to an sf point shapefile:
  
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326) %>% 
  
  # Transform to EPSG 32618 (UTM Zone 18N; our target CRS for mapping):
  
  st_transform(32618)

# data loading, rasters ---------------------------------------------------

# Read in rasters:

rasters <- 
  list.files(
    "data/raw/rasters",
    pattern = "tif",
    full.names = TRUE) %>% 
  map(
    ~ terra::rast(.x)) %>% 
  set_names("canopy", "imp", "nlcd")

# data loading, shapefiles ------------------------------------------------

# Census data for the District of Columbia (the will be the shapefile used
# for cropping and masking:

census_start <- 
  st_read("data/raw/shapefiles/dc_census.geojson",
          quiet = TRUE) %>% 
  
  # Clean names:
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  
  # Transform to EPSG 9001 (same CRS as the canopy cover raster):
  
  st_transform(
    st_crs(rasters$canopy))

# Point data for sick birds:

birds <- 
  st_read("data/raw/shapefiles/sick_birds.geojson",
          quiet = TRUE) %>% 
  
  # Transform to EPSG 32618 (UTM Zone 18N):
  
  st_transform(32618)

# raster and census data pre-processing -----------------------------------

# Crop and mask rasters to the census_start shapefile:

rasters %>% 
  map(
    
    # Crop to the extent of census_start:
    
    ~ terra::crop(.x, census_start) %>% 
      
      # Mask to the shape of census_start:
      
      terra::mask(census_start) %>% 
      
      # Transform to EPSG 32618 (UTM Zone 18N):
      
      terra::project("EPSG:32618",
                     method = "mode")) %>% 
  
  # Send the raster names to the global environment:
  
  list2env(.GlobalEnv)

# Transform the projection of census_start to EPSG 32618:

census <-
  census_start %>% 
  st_transform(32618)

# Note: The above might not seem parsimonious, but I waited to re-project the
# rasters after cropping because this reduced the size of the files and thus
# made the re-projection process much faster.

# clean the global environment --------------------------------------------

# Remove original raster list and census_start:

rm(rasters, census_start)


# Applications of the purrr package!

# setup -------------------------------------------------------------------

library(lubridate)
library(sf)
library(tmap)
library(tidyverse)

# Set plot theme:

theme_set(
  new = theme_bw())

# Assign a read directory:

read_dir <-
  'data/processed/lanternfly_tmap'

# Load iNaturalist observations of spotted lanternflies (Note:  These data were
# recorded in EPSG 4326):

spotted_lanternfly <-
  read_dir %>% 
  file.path('lanternfly_proc.rds') %>% 
  read_rds()

# Now you! Modify the code block above such that it produces a shapefile with
# the CRS EPSG 5070.

# Now you! Modify the code below with a map statement that will read in the files
# counties_east.geojson and states_east.geojson and store them as a list.

shapes <-
  list.files(read_dir)

# Now you! Use a spatial join to subset the counties to just those that contain
# lanternflies:

shapes$counties

# Repeat the process with states:




# Now you! Use purrr::map to subset "shapes" to polygons that contain
# observations of spotted lanternfly. Assign the resultant object to the global
# environment with the name "fly_shapes":

fly_shapes <-
  shapes

# Now you! Calculate the total number of observations in fly_shapes$counties by
# geoid:

spotted_lanternfly

# Repeat the process with states:



# Now you! Use purrr::map to calculate the total number of lanternfly
# observations by county and state:



# Now you! Plot the number of lanternfly observations per county. Include
# shapes$states as a background layer for the plot:



# Plot the number of lanternfly observations by state:



# Now you! Use purrr::map to plot the number of lanternfly observations by
# county and states. Include shapes$states as a background layer for the
# plot:



# Now you! Plot the number of observations by county in 2019:



# Now you! Plot the number of observations by county in from 2015 to 2022 (there
# will be many separate plots):



# Now you! Add year as the title of each plot.





# Set scale limits:



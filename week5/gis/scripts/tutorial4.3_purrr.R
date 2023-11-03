
# setup -------------------------------------------------------------------

library(tidyverse)

# Read in the data:

portal <- 
  read_csv('data/raw/portal.csv')

# Calculate mean hund foot length in base R:

mean(
  portal[portal$species == 'Neotoma albigula', ]$hindfoot_length,
  na.rm = TRUE)

# calculate the above in tidyverse:

portal %>% 
  filter(species == 'Neotoma albigula') %>% 
  pull(hindfoot_length) %>% 
  mean(na.rm = TRUE)

# Using maps for iteration:

portal_spp <-
  unique(portal$species)

my_map <-
  map(
    
    # Sequence statement:
    
    1:length(portal_spp),
    function(i) {
      
      # Split the data:
      
      # Split the data:
      
      species_subset <-
        filter(portal, species == portal_spp[i])
      
      # Output per iteration:
      
      tibble(
        species = portal$species[i],
        # Apply a function:
        
        value = 
          mean(
            species_subset$hindfoot_length,
            na.rm = TRUE))
    })

# Combine the output:

bind_rows(my_map)

# Set i equal to three to test:

i <-
  3

portal_spp[i]

# Using map_dfr:

portal_spp <-
  unique(portal$species)

map_dfr(
  
  # Sequence statement:
  
  1:length(portal_spp),
  function(i) {
    
    # Split the data:
    
    species_subset <-
      filter(portal, species == portal_spp[i])
    
    # Output per iteration:
    
    tibble(
      species = portal$species[i],
      
      # Apply a function:
      
      value = 
        mean(
          species_subset$hindfoot_length,
          na.rm = TRUE))
  })

# Specify variables rather than using indexing:

portal_spp <-
  unique(portal$species)

map_dfr(
  
  # Sequence statement:
  
  portal_spp,
  function(x) {
    
    # Split the data:
    
    species_subset <-
      filter(portal, species == x)
   
    # Output per iteration:
    
     tibble(
      species = x,
      
      # Apply a function:
      
      value = 
        mean(
          species_subset$hindfoot_length,
          na.rm = TRUE))
  })

# without assigning an object to the environment:

map_dfr(
  
  # Sequence statement:
  
  unique(portal$species),
  function(x) {
    
    # Split the data:
    
    species_subset <-
      filter(portal, species == x)
    
    # Output per iteration:
    
    tibble(
      species = x,
      
      # Apply a function:
      
      value = 
        mean(
          species_subset$hindfoot_length,
          na.rm = TRUE))
  })

# Removing the intermediate assigned object as well:

map_dfr(
  
  # Sequence statement:
  
  unique(portal$species),
  function(x) {
    
    # Output per iteration:
    
    tibble(
      species = x,
      value = 
        # Split the data:
        
        filter(portal, species == x) %>% 
        
        # Apply a function:
        
        pull(hindfoot_length) %>% 
        mean(na.rm = TRUE))
  })

# pipe the sequence statement into the map:

unique(portal$species) %>% 
  
  map_dfr(
    function(x) {
      
      # Output per iteration:
      
      tibble(
        species = x,
        value = 
          
          # Split the data:
          
          filter(portal, species == x) %>% 
          
          # Apply a function:
          
          pull(hindfoot_length) %>% 
          mean(na.rm = TRUE))
    })

# test you map:

x <-
  'Peromyscus leucopus'
x

filter(portal, species == x)

filter(portal, species == x) %>% 
  pull(hindfoot_length)

filter(portal, species == x) %>% 
  pull(hindfoot_length) %>% 
  mean(na.rm = TRUE)

tibble(
  species = x,
  value = 
    filter(portal, species == x) %>% 
    pull(hindfoot_length) %>% 
    mean(na.rm = TRUE))

# Test is complete, remove x:

rm(x)

# Replace function and x with ~ and .x:

unique(portal$species) %>% 
  
  map_dfr(
    ~ tibble(
      species = .x,
      value = 
        
        # Split the data:
        
        filter(portal, species == .x) %>% 
        
        # Apply a function:
        
        pull(hindfoot_length) %>% 
        mean(na.rm = TRUE)))

# Use group_by to simplify:

portal %>% 
  # Split the data:
  
  group_by(species) %>% 
  
  # Apply a function and combine the data:
  
  summarize(
    value = 
      mean(
        hindfoot_length, 
        na.rm = TRUE))

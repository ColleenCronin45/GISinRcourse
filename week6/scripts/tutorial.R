# My code for the tutorial: reading, exploring, and writing data

# setup -------------------------------------------------------------------

library(tidyverse)

# Read in country, populations, and CO2 data:

list2env(
  read_rds('C:/Users/User/Documents/Week1/gis/gis/data/raw/making_functions_and_avoiding_them.rds'),
  envir = .GlobalEnv)

multiply_by_two <- 
  function (x) {
    x*2
  }

formals(multiply_by_two)

body(multiply_by_two)

environment(multiply_by_two)

losing_a_y <- 
  function (x) {
    y <- x
    y*2
  }

losing_a_y(3)

y

losing_a_y <- 
  function (x) {
    y <<- x
    y*2
  }

losing_a_y(3)

y

countries[
  countries$country_name == "Mexico",]

countries[
  countries$country_name == "Mexico"]$land_area

countries$land_area[
  countries$country_name == "Mexico"]

land_area_mexico <-
  countries$land_area[
    countries$country_name == "Mexico"]

land_area_mexico

population_mexico_2000 <-
  populations_co2$population[
    populations_co2$country == "Mexico" &
      populations_co2$year == 2000]

population_mexico_2000

population_mexico_2000/land_area_mexico

population_mexico_2010 <-
  populations_co2$population[
    populations_co2$country == "Mexico" &
      populations_co2$year == 2010]

population_mexico_2010/land_area_mexico

land_area_honduras <-
  countries$land_area[
    countries$country_name == "Honduras"]

population_honduras_2010 <-
  populations_co2$population[
    populations_co2$country == "Honduras" &
      populations_co2$year == 2010]

population_honduras_2010/land_area_honduras

# Function to get the land area of a given country:
get_land_area <-
  function (country) {
    countries[
      countries$country_name == country,]$land_area
  }

get_land_area("Mexico")

# Function to get the population of a given country on a given year:

get_population_year <-
  function (country, year) {
    populations_co2[
      populations_co2$country == country &
        populations_co2$year == year,]$population
  }

get_population_year("Mexico", 2010)

get_population_year("Mexico", 2010)/get_land_area("Mexico")

# Function to calculate the population density:

get_population_density__year_3_steps <-
  function (country, year) {
    get_population_year(country, year)/get_land_area(country)
  }

get_population_density__year_3_steps(
  "Mexico",
  2010)

# Function to calculate the population density:

get_population_density_year_nested_functions <-
  function (country, year) {
    
    # Function to get the land area of a given country:
    
    get_land_area <-
      function (country) {
        countries[
          countries$country_name == country,]$land_area
      }
    # Function to get the population of a given country on a given year:
    
    get_population_year <-
      function (country, year) {
        populations_co2[
          populations_co2$country == country &
            populations_co2$year == year,]$population
      }    
   
     get_population_year(country, year)/get_land_area(country)
  }

get_population_density_year_nested_functions(
  "Mexico", 
  2010)    

# Function to calculate the population density:

get_population_density_year_mathy <-
  function (country, year) {
    populations_co2[
      populations_co2$country == country &
        populations_co2$year == year,]$population/
      countries[
        countries$country_name == country,]$land_area
  }

get_population_density_year_mathy(
  "Mexico", 
  2010)

# Function to calculate the population density:

get_population_density_year <-
  function (country, year) {

# Get the land area of a given country:
    
    land_area <-
      countries$land_area[
        countries$country_name == country]    
    
# Get the population of a given country on a given year:
    
    population <-
      populations_co2$population[
        populations_co2$country == country &
          populations_co2$year == year]
    
# Calculate population density
    
   density = population/land_area
   return(density)
  }

get_population_density_year("Mexico", 2010)    

max_pop_year <-
  function(year) {
    country_year <-
      populations_co2[
        populations_co2$year == 2015
     , ]
    max_country <-
      country_year$country[
        country_year$population == max(country_year$population, na.rm = TRUE) & 
        !is.na(country_year$population)
      ]
    return(max_country)
  }

max_pop_year(2015)

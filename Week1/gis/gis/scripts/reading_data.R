# Reading, exploring, and writing data
# Author: Brian S. Evans
# Date created: 2023-03-06
# Last modified on 2023-03-06

# In this video lesson, we will explore:
# - How to read data into R
# - Some initial data exploration steps
# - How to save files to your computer

# setup -------------------------------------------------------------------

# Load all libraries that will be used more than once:

library(tidyverse)

# List files in a directory:

list.files("C:/Users/User/Documents/Week1/gis/gis/data/raw", pattern = "txt")

# reading data ------------------------------------------------------------

# Read in a vector with read_lines():

read_lines("C:/Users/User/Documents/Week1/gis/gis/data/raw/four_instruments.txt", skip = 1)

# Read in tabular csv files with read_csv():

read_csv("C:/Users/User/Documents/Week1/gis/gis/data/raw/iris.csv")

# read.csv is less informative and just let's you look at all the data

# Table troubles ...

read_csv(
  "C:/Users/User/Documents/Week1/gis/gis/data/raw/API_EN.ATM.CO2E.PC_DS2_en_csv_v2_2764620.csv", 
  skip = 4,
  skip_empty_rows = FALSE)

# The RDS file:

read_rds("C:/Users/User/Documents/Week1/gis/gis/data/raw/iris.rds")

read_rds('C:/Users/User/Documents/Week1/gis/gis/data/raw/four_instruments.rds')

# Read an Excel file with the readxl package function read_xlsx:

readxl::excel_sheets('C:/Users/User/Documents/Week1/gis//gis/data/raw/portal.xlsx')

readxl::read_excel("C:/Users/User/Documents/Week1/gis/gis/data/raw/portal.xlsx", 
    sheet = "site_7",
    range = "A6:E34",
    col_names = 
      c("plot",
        "date",
        "family",
        "species",
        "weight"),
    col_types = 
      c("text", 
        "date", 
        "text", 
        "text", 
        "numeric"))

# Now you! Modify the code above such that the column names are Plot, Date
# collected, Family, Genus Species, and Weight.

# Now you! Modify the code above such that Date collected is read in as a date
# column.

# Now you! Modify the code above such that the column names are plot, date,
# family, species, and weight.


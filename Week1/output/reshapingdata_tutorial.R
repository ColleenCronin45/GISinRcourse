# My code for the tutorial: Reshaping Data Frames


# setup -------------------------------------------------------------------

library(tidyverse)

angola_ungulates_list <- 
  read_rds("C:/Users/User/Documents/Week1/gis/gis/data/raw/angola_ungulates.rds")

list2env(angola_ungulates_list, envir = .GlobalEnv)

rm(angola_ungulates_list)

# Odd-toed ungulates:

slice_head(perissodactyla)

# Even-toed ungulates:

slice_head(artiodactyla)

unique(perissodactyla$taxonomy)

unique(artiodactyla$taxonomy)

bind_rows(
  perissodactyla,
  artiodactyla)

#We can combine both list items when we read in the data using bind_rows

angola_ungulates <- 
  bind_rows(
    read_rds("C:/Users/User/Documents/Week1/gis/gis/data/raw/angola_ungulates.rds"))

angola_ungulates

rm(perissodactyla, artiodactyla)

angola_ungulates_date_fix <- 
  unite(
    angola_ungulates,
    col = "date",
    year:day,
    sep = "-")

rm(angola_ungulates)

angola_ungulates_spp_fix <- 
  unite(
    angola_ungulates_date_fix,
    col = "sci_name",
    genus:species,
    sep = " ")

rm(angola_ungulates_date_fix)

angola_ungulates_spp_fix

angola_ungulates_taxonomy_fix <- 
  separate(
    angola_ungulates_spp_fix,
    col = "taxonomy",
    into = c("class", "order", "family"),
    sep = "-")

angola_ungulates_taxonomy_fix

rm(angola_ungulates_spp_fix)

observations <- 
  select(
    angola_ungulates_taxonomy_fix,
    date:user_login,
    sci_name)

observations

taxonomy <- 
  select(
    angola_ungulates_taxonomy_fix,
    sci_name,
    class:family,
    common_name)

taxonomy

unique(taxonomy$class)

select(taxonomy, !class)

rm(angola_ungulates_taxonomy_fix)

taxonomy_distinct <-
  distinct(taxonomy)

taxonomy_distinct

left_join(
  observations,
  taxonomy_distinct,
  by = "sci_name")

rm(taxonomy)

families <- 
  left_join(
    select(observations, sci_name),
    select(
      taxonomy_distinct,
      sci_name, 
      family),
    by = "sci_name")

count(families, family)

wide_families <- 
  pivot_wider(
    count(families, family),
    names_from = family,
    values_from = n)

wide_families

pivot_longer(
  wide_families,
  cols = Bovidae:Suidae,
  names_to = "family",
  values_to = "n")

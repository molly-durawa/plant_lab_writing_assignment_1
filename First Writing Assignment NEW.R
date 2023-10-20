# ======================================== #
# Molly Durawa                             #
#                                          #
# Lab in Plant Evolution and Diversity     #
#                                          #
#      Code for writing assignment 1      #
# ======================================== #



### My idea: ================================================================================================================================================================
#
# I want to compare the species diversity in different neighborhoods in Manhattan and the Bronx.
#
# ===========================================================================================================================================================================

### Data sources
# https://data.cityofnewyork.us/City-Government/Borough-Boundaries/tqmj-j8zm 
# https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/pi5s-9p35 
# https://www.nyc.gov/site/planning/data-maps/open-data/census-download-metadata.page 

# Load necessary packages ---------------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(tmap)
library(sf)
library(raster)
library(mapview)
library(vegan)


# Store Tree Census csv in a variable ---------------------------------------------------------------------------------------------------------------------------------------

# first, need to set my directory to the folder with the tree data
setwd("/Users/mollydurawa/Downloads/Lab in Plant Evolution and Diversity/First Coding Assignment")

tree_data <-read.csv("2015StreetTreesCensus_TREES.csv")


# Data cleaning and organization -------------------------------------------------------------------------------------------------------------------------------------------

tree_data_for_shannon <- tree_data %>%
  dplyr::select(nta_name, spc_common, borocode, boroname, nta) %>%
  filter(spc_common != "") %>% # get rid of trees with no species name
  dplyr::group_by(spc_common, nta) %>%
  dplyr::summarize(count=n()) %>%
  dplyr::group_by(nta)
  
tree_data_for_shannon_new <- tree_data_for_shannon %>%
  dplyr::group_by(nta) %>%
  arrange(nta, .by_group=TRUE) %>%
  dplyr::mutate(nta_factor =as.factor(nta)) %>%
  dplyr::mutate(nta_numeric = as.numeric(nta_factor)) 

key_for_nta <- tree_data_for_shannon_new %>%
  distinct(nta_numeric, .keep_all = TRUE) %>%
  dplyr::select(!c(spc_common))

tree_data_for_shannon_new_2 <- tree_data_for_shannon_new %>%
  ungroup() %>%
  dplyr::select(nta_numeric, count, spc_common)


# put into format so i can calculate diversity
tree_data_for_shannon_recast <- 
  reshape::recast(tree_data_for_shannon_new_2, nta_numeric + variable ~ spc_common, id.var = c("spc_common", "nta_numeric")) %>%
  as.data.frame() %>%
  dplyr::select(!c(variable)) %>%
  replace(is.na(.), 0)  # make NAs 0 


# find diversity
diversity_by_nta_to_bind <- diversity(tree_data_for_shannon_recast, index="shannon") %>%
  as.data.frame() 

# bind diversity to df so we can see neighborhood diffs
diversity_by_nta <- cbind(diversity_by_nta_to_bind, key_for_nta) %>%
  mutate(NTACode = nta)

diversity_by_nta <- mutate(diversity_by_nta, diversity = .)

# find diversity by borough

tree_data_for_shannon_boro <- tree_data %>%
  dplyr::select(spc_common, borocode) %>%
  rename(boro_code = borocode) %>% # give it same name as map variable (see later)
  filter(spc_common != "") %>% # get rid of trees with no species name
  dplyr::group_by(spc_common, boro_code) %>% 
  dplyr::summarize(count=n())

tree_data_for_shannon_boro_recast <- 
  reshape::recast(tree_data_for_shannon_boro, boro_code + variable ~ spc_common, id.var = c("spc_common", "boro_code")) %>%
  as.data.frame() %>%
  dplyr::select(!c(variable)) %>%
  replace(is.na(.), 0)  # make NAs 0 

diversity_by_boro_to_bind <- diversity(tree_data_for_shannon_boro_recast, index="shannon") %>%
  as.data.frame() 

# bind diversity to df so we can see boro diffs
diversity_by_boro <- cbind(diversity_by_boro_to_bind, tree_data_for_shannon_boro)

diversity_by_boro <- diversity_by_boro %>%
  mutate(diversity_by_boro, diversity = .) %>% # give column name diversity
  mutate(diversity_by_boro, diversity = round(diversity,4)) %>%
  distinct(boro_code, .keep_all = TRUE) 




# Neighborhoods map -------------------------------------------------------------------------------------------------------------------------------------------------------

# make a variable that stores the map of nyc neighborhoods
neighborhoods <- shapefile("/Users/mollydurawa/Downloads/Lab in Plant Evolution and Diversity/First Coding Assignment/nynta2010_23c/nynta2010.shp")

mapview(neighborhoods) # view the map

# Boroughs map -------------------------------------------------------------------------------------------------------------------------------------------------------

# make a variable that stores the map of nyc neighborhoods
boros <- shapefile("/Users/mollydurawa/Downloads/Lab in Plant Evolution and Diversity/First Coding Assignment/Borough Boundaries/geo_export_2a98f7a2-5909-4187-ad46-dc2e4f487159.shp")



# Merge spatial data and tree census data -----------------------------------------------------------------------------


# create a CRS object:
new_crs <- CRS("+init=EPSG:4326")

# reproject neighborhood map
neighborhoods_new <- spTransform(neighborhoods, new_crs)

# make neighborhood map sf
neighborhoods_sf <- st_as_sf(neighborhoods_new)

# reproject boro map
boros_new <- spTransform(boros, new_crs)

# make boro map sf
boros_sf <- st_as_sf(boros_new)



# merge 
diversity_neighborhoods <- merge(diversity_by_nta, neighborhoods_sf, by="NTACode") %>%
  st_as_sf()

diversity_boros <- merge(diversity_by_boro, boros_sf, by="boro_code") %>%
  st_as_sf()

tree_map <- tm_shape(diversity_neighborhoods) +
  tm_polygons("diversity", 
              style="jenks", 
              palette="YlGn",
              title=paste("Tree species diversity",sep="\n")) + 
 # tm_text("NTAName", size = 0.2, remove.overlap=TRUE)  +
  tm_shape(diversity_boros) +
  tm_layout(fontface="plain") +
  tm_text("boro_name", size=0.7, fontface="bold") +
  tm_shape(diversity_boros) +
  tm_text("diversity", size=0.5, ymod=-0.5) +
  tm_credits(
    position="right",
    text=
    "Notes: Diversity was calculated using the Shannon index of                \nspecies diversity.\nDiversity by borough is indicated under the borough name.")



tree_map





################################################################################
# File Name: 01f_process_geocode_survey_data                                   #
#                                                                              #
# Purpose: Load and format geocoded survey data.                               #
# Steps:                                                                       #
#         1. Set-up script                                                     #
#         2. Load and format baseline survey geocode                           #
#         3. Load and format travel survey geocode                             #
#                                                                              #
# Project: Sri Lanka Spatial Aggregation                                       #
# Authors: Ronan Corgel                                                        #
################################################################################

####################
# 1. SET-UP SCRIPT #
####################

# Start with a clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggplot2)
library(reshape2)
library(lubridate)
library(assertr)

# Set the seed
set.seed(12345)

# Set directory 
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

##############################################
# 2. LOAD AND FORMAT BASELINE SURVEY GEOCODE #
##############################################

# Load baseline survey
baseline_survey <- read.csv('./raw/baseline_geolocation_20220621.csv')

# Format baseline survey data
# Two observations have missing geocode information
baseline_survey <- baseline_survey %>% 
  dplyr::select(c('idno', 'Longitude', 'Latitude', 'Precision')) %>%
  dplyr::rename('id' = 'idno',
                'res_long' = 'Longitude',
                'res_lat' = 'Latitude',
                'res_precision' = 'Precision')

# Fix geocode/data entry errors
# Symbols and text in numeric field
baseline_survey$res_long[baseline_survey$id == 5471121] <- '7.086160' # comma deleted
baseline_survey$res_lat[baseline_survey$id == 5447521] <- '79.846004' # cross symbol deleted

# Convert coordinates to numeric
baseline_survey <- baseline_survey %>% 
  mutate(res_long = as.numeric(res_long),
         res_lat = as.numeric(res_lat))

# Raw geocode data switched lat and long, switch to correct names
baseline_survey <- baseline_survey %>%
  dplyr::rename('res_lon' = 'res_lat', 
                'res_lat' = 'res_long')

# Assert not missing id
baseline_survey %>% assert(not_na, id)

# Save data
save(baseline_survey, file = './tmp/baseline_survey_dat.RData')

############################################
# 3. LOAD AND FORMAT TRAVEL SURVEY GEOCODE #
############################################

# Load travel survey
travel_survey <- read.csv('./raw/TSBaseline_20220621_combined.csv')

# Format travel survey data 
# Rename variables to a common naming convention
travel_survey <- travel_survey %>% 
  dplyr::rename('id' = 'idno',
                'name_loc.1' = 'name_loc1',
                'street_loc.1' = 'street_loc1',
                'village_loc.1' = 'village_loc1',
                'city_loc.1' = 'city_loc1',
                'district_loc.1' = 'district_loc1',
                'long_loc.1' = 'Location1_Longitude',
                'lat_loc.1' = 'Location1_latitude',
                'precision_loc.1' = 'Precision1',
                'name_loc.2' = 'name_loc2',
                'street_loc.2' = 'street_loc2',
                'village_loc.2' = 'village_loc2',
                'city_loc.2' = 'city_loc2',
                'district_loc.2' = 'district_loc2',
                'long_loc.2' = 'Location2_Longitude',
                'lat_loc.2' = 'Location2_Latitude',
                'precision_loc.2' = 'Precision2',
                'name_loc.3' = 'name_loc3',
                'street_loc.3' = 'street_loc3',
                'village_loc.3' = 'village_loc3',
                'city_loc.3' = 'city_loc3',
                'district_loc.3' = 'district_loc3',
                'long_loc.3' = 'Location3_Longitude',
                'lat_loc.3' = 'Location3_Latitude',
                'precision_loc.3' = 'Precision3',
                'name_loc.4' = 'name_loc4',
                'street_loc.4' = 'street_loc4',
                'village_loc.4' = 'village_loc4',
                'city_loc.4' = 'city_loc4',
                'district_loc.4' = 'district_loc4',
                'long_loc.4' = 'Location4_longitude',
                'lat_loc.4' = 'Location4_Latitude',
                'precision_loc.4' = 'Precision4',
                'name_loc.5' = 'name_loc4.1',
                'street_loc.5' = 'street_loc5',
                'village_loc.5' = 'village_loc5',
                'city_loc.5' = 'city_loc5',
                'district_loc.5' = 'district_loc5',
                'long_loc.5' = 'Location5_longitude',
                'lat_loc.5' = 'Location5_latitude',
                'precision_loc.5' = 'Precision5',
                'name_loc.6' = 'name_loc6',
                'street_loc.6' = 'street_loc6',
                'village_loc.6' = 'village_loc6',
                'city_loc.6' = 'city_loc6',
                'district_loc.6' = 'district_loc6',
                'long_loc.6' = 'Location6_longitude',
                'lat_loc.6' = 'Location6_latitude',
                'precision_loc.6' = 'Precision6',
                'name_loc.7' = 'name_loc7',
                'street_loc.7' = 'street_loc7',
                'village_loc.7' = 'village_loc7',
                'city_loc.7' = 'city_loc7',
                'district_loc.7' = 'district_loc7',
                'long_loc.7' = 'Location7_longitude',
                'lat_loc.7' = 'Location7_latitude',
                'precision_loc.7' = 'Precision7',
                'name_loc.8' = 'name_loc8',
                'street_loc.8' = 'street_loc8',
                'village_loc.8' = 'village_loc8',
                'city_loc.8' = 'city_loc8',
                'district_loc.8' = 'district_loc8',
                'long_loc.8' = 'Location8_longitude',
                'lat_loc.8' = 'Location8_latitude',
                'precision_loc.8' = 'Precision8',
                'name_loc.9' = 'name_loc9',
                'street_loc.9' = 'street_loc9',
                'village_loc.9' = 'village_loc9',
                'city_loc.9' = 'city_loc9',
                'district_loc.9' = 'district_loc9',
                'long_loc.9' = 'Location9_longitude',
                'lat_loc.9' = 'Location9_Latitude',
                'precision_loc.9' = 'Precision9',
                'name_loc.10' = 'name_loc10',
                'street_loc.10' = 'street_loc10',
                'village_loc.10' = 'village_loc10',
                'city_loc.10' = 'city_loc10',
                'district_loc.10' = 'district_loc10',
                'long_loc.10' = 'Location10_longitude',
                'lat_loc.10' = 'Location10_latitude',
                'precision_loc.10' = 'Precision10')

# Reshape survey data from wide to long
# Each row is a single trip location
travel_survey_long  <- travel_survey %>%
  melt(id.vars = c('id', 'dt_survey', 'redcap_event_name')) %>%
  # Create trip number variable
  mutate(trip_number = as.numeric(str_sub(variable, -1, -1)),
         trip_number = ifelse(trip_number == 0, 10, trip_number),
         variable = ifelse(trip_number != 10, 
                           str_sub(variable, 1, str_length(variable)-2),
                           str_sub(variable, 1, str_length(variable)-3))) %>%
  dcast(id + dt_survey + redcap_event_name + trip_number ~ variable) %>%
  # Filter out blank rows (not filled out)
  mutate(drop = ifelse(is.na(long_loc) & is.na(precision_loc) & trip_number > 1, 1, 0)) %>%
  filter(drop == 0) %>%
  mutate(long_loc = as.numeric(long_loc),
         lat_loc = as.numeric(lat_loc)) %>%
  # Select only variables of interest and rename for merging
  # Location vars will be used to check accuracy of the merge
  dplyr::select(c(id, trip_number, district_loc, city_loc, village_loc, 
                  street_loc, name_loc, lat_loc, long_loc, precision_loc))

# Confirm no person is dropped
length(unique(travel_survey$id))
verify(travel_survey_long, length(unique(id)) == 299)

# Fix geocode/data entry errors
# Entered correct numbers in incorrect columns
travel_survey_long$precision_loc[travel_survey_long$id == 5981821 & travel_survey_long$trip_number == 3] <- 2.000000 # re-arrange values
travel_survey_long$district_loc[travel_survey_long$id == 5981821 & travel_survey_long$trip_number == 3] <- ""        # re-arrange values
travel_survey_long$long_loc[travel_survey_long$id == 5981821 & travel_survey_long$trip_number == 3] <- 7.084143      # re-arrange values
travel_survey_long$lat_loc[travel_survey_long$id == 5981821 & travel_survey_long$trip_number == 3] <- 79.995830      # re-arrange values

travel_survey_long$long_loc[travel_survey_long$id == 7449221 & travel_survey_long$trip_number == 1] <- 7.116171 # set at village railway station
travel_survey_long$lat_loc[travel_survey_long$id == 7449221 & travel_survey_long$trip_number == 1] <- 79.887042 # set at village railway station
travel_survey_long$precision_loc[travel_survey_long$id == 7449221 & travel_survey_long$trip_number == 1] <- 999 # change precision to indicate not address

# Entered long in lat column
travel_survey_long$long_loc[travel_survey_long$id == 3454121 & travel_survey_long$trip_number == 2] <- 7.093475 # exact location
travel_survey_long$lat_loc[travel_survey_long$id == 3454121 & travel_survey_long$trip_number == 2] <- 79.997532 # exact location

travel_survey_long$long_loc[travel_survey_long$id == 7480021 & travel_survey_long$trip_number == 1] <- 7.248057 # set to city centroid
travel_survey_long$lat_loc[travel_survey_long$id == 7480021 & travel_survey_long$trip_number == 1] <- 79.899603 # set to city centroid
travel_survey_long$precision_loc[travel_survey_long$id == 7480021 & travel_survey_long$trip_number == 1] <- 999 # change precision to indicate not address

# Point located in India, but location in Sri Lanka
travel_survey_long$long_loc[travel_survey_long$id == 3082720 & travel_survey_long$trip_number == 1] <- 7.467868 # set to city centroid
travel_survey_long$lat_loc[travel_survey_long$id == 3082720 & travel_survey_long$trip_number == 1] <- 80.622954 # set to city centroid
travel_survey_long$precision_loc[travel_survey_long$id == 3082720 & travel_survey_long$trip_number == 1] <- 999 # change precision to indicate not address

# Raw geocode data switched lat and long, switch to correct names
travel_survey_long <- travel_survey_long %>%
  dplyr::rename('lon_loc' = 'lat_loc', 
                'lat_loc' = 'long_loc')

# Save data
save(travel_survey_long, file = './tmp/travel_survey_dat.RData')

################################################################################
################################################################################

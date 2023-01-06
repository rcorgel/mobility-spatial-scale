################################################################################
# File Name: 01h_merge_survey_data                                             #
#                                                                              #
# Purpose: Merge geocoded baseline and travel survey data with combined        #
#          survey data.                                                        #
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Merge resident geocode and combined survey                        #
#         3. Merge travel geocode and combined survey                          #
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
library(writexl)
library(ggmap)
library(readxl)

# Set the seed
set.seed(12345)

# Set directory 
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

#################################################
# 2. MERGE RESIDENT GEOCODE AND COMBINED SURVEY #
#################################################

# Load data to merge
load('./tmp/combined_survey_dat.RData')
load('./tmp/baseline_survey_dat.RData')

# Create merge variables
combined_survey_long$in_survey_res <- 1
baseline_survey$in_geocode_res <- 1

# Merge on baseline geocode
survey_dat <- full_join(combined_survey_long, baseline_survey, by = c('id'))

# Create a merge variable to keep track of where every observation came from
survey_dat$res_merge <- ifelse(is.na(survey_dat$in_geocode_res), 'from survey only', 'matched with geocode')
survey_dat$res_merge <- ifelse(is.na(survey_dat$in_survey_res), 'from geocode only', survey_dat$res_merge)

# Separate data to geocode
survey_res_geocode <- survey_dat %>%
  filter(res_merge == 'from survey only')
survey_dat <- survey_dat %>%
  filter(res_merge == 'matched with geocode' | res_merge == 'from geocode only')

# Geocode unmerged observations
# Create address variable based on location data availability
survey_res_geocode$geocode_address <- paste(survey_res_geocode$res_village, 
                                            survey_res_geocode$res_city,
                                            survey_res_geocode$res_district,
                                            'Sri Lanka', sep = ', ')

survey_res_geocode$geocode_address <- ifelse(survey_res_geocode$res_village == '', 
                                     paste(survey_res_geocode$res_city, 
                                           survey_res_geocode$res_district,
                                           'Sri Lanka', sep = ', '),
                                     survey_res_geocode$geocode_address)

# Replace precision variable
# 999 - geocoded to city/village level using google api
survey_res_geocode$res_precision <- 999

# Geocode
register_google(key = 'AIzaSyDmhLWCw5G-wzKS472eXaUAHU-d4RdaLAQ')
survey_res_geocode <- mutate_geocode(survey_res_geocode, location = geocode_address, output = 'latlona')

# Replace with geocode
survey_res_geocode$res_lat <- survey_res_geocode$lat 
survey_res_geocode$res_lon <- survey_res_geocode$lon 

# Remove variables from geocode
survey_res_geocode <- survey_res_geocode %>% 
  dplyr::select(-c('geocode_address', 'lat', 'lon', 'address'))

# Append to survey data
# Any survey data that was not in the resident geocode and has complete address
# information is geocoded and appended
survey_dat <- rbind(survey_dat, survey_res_geocode)

# Assert unique ids are the same after merge process
length(unique(c(baseline_survey[, 1], combined_survey_long[, 1])))
verify(survey_dat, length(unique(id)) == 317)

###############################################
# 3. MERGE TRAVEL GEOCODE AND COMBINED SURVEY #
###############################################

# Load data to merge
load('./tmp/travel_survey_dat.RData')

###########
# ROUND 1 #
###########

# Remove leading and trailing blanks and make all lower case to ease merging process
survey_dat <- survey_dat %>% 
  mutate(id = tolower(trimws(id, which = 'both'))) %>%
  mutate(name_loc = tolower(trimws(name_loc, which = 'both'))) %>%
  mutate(street_loc = tolower(trimws(street_loc, which = 'both'))) %>%
  mutate(village_loc = tolower(trimws(village_loc, which = 'both'))) %>%
  mutate(city_loc = tolower(trimws(city_loc, which = 'both'))) %>%
  mutate(district_loc = tolower(trimws(district_loc, which = 'both')))

travel_survey_long <- travel_survey_long %>% 
  mutate(id = tolower(trimws(id, which = 'both'))) %>%
  mutate(name_loc = tolower(trimws(name_loc, which = 'both'))) %>%
  mutate(street_loc = tolower(trimws(street_loc, which = 'both'))) %>%
  mutate(village_loc = tolower(trimws(village_loc, which = 'both'))) %>%
  mutate(city_loc = tolower(trimws(city_loc, which = 'both'))) %>%
  mutate(district_loc = tolower(trimws(district_loc, which = 'both')))

# Create merge variables
survey_dat$in_survey_loc <- 1
travel_survey_long$in_geocode_loc <- 1

# Merge on travel geocode
survey_dat_0 <- full_join(survey_dat, travel_survey_long, by = c('id', 'name_loc', 'trip_number',
                                                                 'street_loc', 'village_loc',
                                                                 'city_loc', 'district_loc'))

# Unmatched from geocode
geocode_dat_unmatch_1 <- survey_dat_0 %>%
  dplyr::filter(is.na(in_survey_loc)) %>%
  dplyr::select(c('id', 'name_loc', 'street_loc', 'village_loc', 'city_loc', 'district_loc', 
                  'trip_number', 'lat_loc', 'lon_loc', 'precision_loc'))

# Unmatched from survey
survey_dat_unmatch_1 <- survey_dat_0 %>%
  dplyr::filter(is.na(in_geocode_loc)) %>%
  dplyr::select(-c('lat_loc', 'lon_loc', 'precision_loc', 'in_survey_loc', 'in_geocode_loc'))

# Drop unmatched from matched data
survey_dat_0 <- survey_dat_0 %>% 
  dplyr::filter(!is.na(in_survey_loc)) %>%
  dplyr::filter(!is.na(in_geocode_loc)) %>%
  mutate(merge_loc = 'match round 1')

###########
# ROUND 2 #
###########

# Create merge variables
survey_dat_unmatch_1$in_survey_loc <- 1
geocode_dat_unmatch_1$in_geocode_loc <- 1

# Complete less restrictive merge on unmatched data
survey_dat_1 <- full_join(survey_dat_unmatch_1, geocode_dat_unmatch_1, 
                          by = c('id', 'trip_number', 'street_loc', 'village_loc',
                                 'city_loc', 'district_loc'))

# Unmatched from geocode
geocode_dat_unmatch_2 <- survey_dat_1 %>%
  dplyr::filter(is.na(in_survey_loc)) %>%
  dplyr::select(c('id', 'name_loc.y', 'street_loc', 'village_loc', 'city_loc', 'district_loc', 
                  'trip_number', 'lat_loc', 'lon_loc', 'precision_loc')) %>%
  dplyr::rename('name_loc' = 'name_loc.y')

# Unmatched from survey
survey_dat_unmatch_2 <- survey_dat_1 %>%
  dplyr::filter(is.na(in_geocode_loc)) %>%
  dplyr::select(-c('name_loc.y', 'lat_loc', 'lon_loc', 'precision_loc', 'in_survey_loc', 
                   'in_geocode_loc')) %>%
  dplyr::rename('name_loc' = 'name_loc.x')

# Drop unmatched from matched data
survey_dat_1 <- survey_dat_1 %>% 
  dplyr::filter(!is.na(in_survey_loc)) %>%
  dplyr::filter(!is.na(in_geocode_loc)) %>%
  dplyr::select(-c('name_loc.y')) %>%
  dplyr::rename('name_loc' = 'name_loc.x') %>%
  mutate(merge_loc = 'match round 2')

###########
# ROUND 3 #
###########

# Create merge variables
survey_dat_unmatch_2$in_survey_loc <- 1
geocode_dat_unmatch_2$in_geocode_loc <- 1

# Complete less restrictive merge on unmatched data
survey_dat_2 <- full_join(survey_dat_unmatch_2, geocode_dat_unmatch_2, 
                          by = c('id', 'trip_number', 'village_loc', 'city_loc', 
                                 'district_loc'))

# Unmatched from geocode
geocode_dat_unmatch_3 <- survey_dat_2 %>%
  dplyr::filter(is.na(in_survey_loc)) %>%
  dplyr::select(c('id', 'name_loc.y', 'street_loc.y', 'village_loc', 'city_loc', 'district_loc', 
                  'trip_number', 'lat_loc', 'lon_loc', 'precision_loc')) %>%
  dplyr::rename('name_loc' = 'name_loc.y', 
                'street_loc' = 'street_loc.y')

# Unmatched from survey
survey_dat_unmatch_3 <- survey_dat_2 %>%
  dplyr::filter(is.na(in_geocode_loc)) %>%
  dplyr::select(-c('name_loc.y', 'street_loc.y', 'lat_loc', 'lon_loc', 'precision_loc',
                   'in_survey_loc', 'in_geocode_loc')) %>%
  dplyr::rename('name_loc' = 'name_loc.x', 
                'street_loc' = 'street_loc.x')

# Drop unmatched from matched data
survey_dat_2 <- survey_dat_2 %>% 
  dplyr::filter(!is.na(in_survey_loc)) %>%
  dplyr::filter(!is.na(in_geocode_loc)) %>%
  dplyr::select(-c('name_loc.y', 'street_loc.y')) %>%
  dplyr::rename('name_loc' = 'name_loc.x', 
                'street_loc' = 'street_loc.x') %>%
  mutate(merge_loc = 'match round 3')

###########
# ROUND 4 #
###########

# Create merge variables
survey_dat_unmatch_3$in_survey_loc <- 1
geocode_dat_unmatch_3$in_geocode_loc <- 1
  
# Complete less restrictive merge on unmatched data
survey_dat_3 <- full_join(survey_dat_unmatch_3, geocode_dat_unmatch_3, 
                          by = c('id', 'trip_number', 'city_loc', 'district_loc'))

# Unmatched from geocode
geocode_dat_unmatch_4 <- survey_dat_3 %>%
  dplyr::filter(is.na(in_survey_loc)) %>%
  dplyr::select(c('id', 'name_loc.y', 'street_loc.y', 'village_loc.y', 'city_loc', 'district_loc', 
                  'trip_number', 'lat_loc', 'lon_loc', 'precision_loc')) %>%
  dplyr::rename('name_loc' = 'name_loc.y', 
                'street_loc' = 'street_loc.y',
                'village_loc' = 'village_loc.y')

# Unmatched from survey
survey_dat_unmatch_4 <- survey_dat_3 %>%
  dplyr::filter(is.na(in_geocode_loc)) %>%
  dplyr::select(-c('name_loc.y', 'street_loc.y', 'village_loc.y', 'lat_loc', 'lon_loc', 
                   'precision_loc', 'in_survey_loc', 'in_geocode_loc')) %>%
  dplyr::rename('name_loc' = 'name_loc.x', 
                'street_loc' = 'street_loc.x',
                'village_loc' = 'village_loc.x')

# Drop unmatched from matched data
survey_dat_3 <- survey_dat_3 %>% 
  dplyr::filter(!is.na(in_survey_loc)) %>%
  dplyr::filter(!is.na(in_geocode_loc)) %>%
  dplyr::select(-c('name_loc.y', 'street_loc.y', 'village_loc.y')) %>%
  dplyr::rename('name_loc' = 'name_loc.x', 
                'street_loc' = 'street_loc.x',
                'village_loc' = 'village_loc.x') %>%
  mutate(merge_loc = 'match round 4')

###########
# ROUND 5 #
###########

# Create merge variables
survey_dat_unmatch_4$in_survey_loc <- 1
geocode_dat_unmatch_4$in_geocode_loc <- 1

# Complete less restrictive merge on unmatched data
survey_dat_4 <- full_join(survey_dat_unmatch_4, geocode_dat_unmatch_4, 
                          by = c('trip_number', 'street_loc', 'village_loc', 
                                  'city_loc', 'district_loc'))

# Unmatched from geocode
geocode_dat_unmatch_5 <- survey_dat_4 %>%
  dplyr::filter(is.na(in_survey_loc)) %>%
  dplyr::select(c('id.y', 'name_loc.y', 'street_loc', 'village_loc', 'city_loc', 'district_loc', 
                  'trip_number', 'lat_loc', 'lon_loc', 'precision_loc')) %>%
  dplyr::rename('id' = 'id.y',
                'name_loc' = 'name_loc.y')

# Unmatched from survey
survey_dat_unmatch_5 <- survey_dat_4 %>%
  dplyr::filter(is.na(in_geocode_loc)) %>%
  dplyr::select(-c('id.y', 'name_loc.y', 'lat_loc', 'lon_loc', 'precision_loc', 
                   'in_survey_loc', 'in_geocode_loc')) %>%
  dplyr::rename('id' = 'id.x',
                'name_loc' = 'name_loc.x')

# Drop unmatched from matched data
survey_dat_4 <- survey_dat_4 %>% 
  dplyr::filter(!is.na(in_survey_loc)) %>%
  dplyr::filter(!is.na(in_geocode_loc)) %>%
  dplyr::select(-c('id.y', 'name_loc.y')) %>%
  dplyr::rename('id' = 'id.x',
                'name_loc' = 'name_loc.x') %>%
  mutate(merge_loc = 'match round 5 (diff ids)')

################
# MANUAL MATCH #
################

# Create merge variables
survey_dat_unmatch_5$in_survey_loc <- 1
geocode_dat_unmatch_5$in_geocode_loc <- 1

# Create row id variable
survey_dat_unmatch_5 <- dplyr::mutate(survey_dat_unmatch_5, row_id = row_number())

# Complete least restrictive merge
survey_dat_5 <- full_join(survey_dat_unmatch_5, geocode_dat_unmatch_5, 
                          by = c('id'))

# Manually set matches (after carefully examining data)
survey_dat_5$match <- 0
survey_dat_5$match <- ifelse(survey_dat_5$name_loc.x == 'kochchikade electrical', 
                             1, survey_dat_5$match)
survey_dat_5$match <- ifelse(survey_dat_5$name_loc.x == 'harelock city commercial devolpment project', 
                             1, survey_dat_5$match)
survey_dat_5$match <- ifelse(survey_dat_5$name_loc.x == '14th lane', 
                             1, survey_dat_5$match)
survey_dat_5$match <- ifelse(survey_dat_5$name_loc.x == 'mihindu paraya', 
                             1, survey_dat_5$match)
survey_dat_5$match <- ifelse(survey_dat_5$name_loc.x == 'kandy gold house', 
                             1, survey_dat_5$match)
survey_dat_5$match <- ifelse(survey_dat_5$street_loc.x == '26', 
                             1, survey_dat_5$match)
survey_dat_5$match <- ifelse(survey_dat_5$village_loc.x == 'uswetakeiyawa', 
                             1, survey_dat_5$match)

# Unmatched from geocode
geocode_dat_unmatch_6 <- geocode_dat_unmatch_5 %>%
  dplyr::filter(name_loc != 'kochchikade electrical') %>%
  dplyr::filter(name_loc != 'harelock city commercial devolpment project') %>%
  dplyr::filter(name_loc != '14th lane') %>%
  dplyr::filter(name_loc != 'mihindu paraya') %>%
  dplyr::filter(name_loc != 'kandy gold house') %>%
  dplyr::filter(street_loc != '26') %>%
  dplyr::filter(village_loc != 'uswetakeiyawa')
  

# Unmatched from survey
survey_dat_unmatch_5$village_loc[is.na(survey_dat_unmatch_5$village_loc)] <- ''
survey_dat_unmatch_5$name_loc[is.na(survey_dat_unmatch_5$name_loc)] <- ''
survey_dat_unmatch_5$street_loc[is.na(survey_dat_unmatch_5$street_loc)] <- ''
survey_dat_unmatch_5$district_loc[is.na(survey_dat_unmatch_5$district_loc)] <- ''
survey_dat_unmatch_5$city_loc[is.na(survey_dat_unmatch_5$city_loc)] <- ''
survey_dat_unmatch_6 <- survey_dat_unmatch_5 %>%
  dplyr::filter(name_loc != 'kochchikade electrical') %>%
  dplyr::filter(name_loc != 'harelock city commercial devolpment project') %>%
  dplyr::filter(name_loc != '14th lane') %>%
  dplyr::filter(name_loc != 'mihindu paraya') %>%
  dplyr::filter(name_loc != 'kandy gold house') %>%
  dplyr::filter(street_loc != '26') %>%
  dplyr::filter(village_loc != 'uswetakeiyawa')

# Drop unmatched from matched data
survey_dat_5 <- survey_dat_5 %>% 
  dplyr::filter(match == 1) %>%
  dplyr::select(-c('name_loc.y', 'street_loc.y', 'village_loc.y', 'row_id', 'city_loc.y', 
                   'district_loc.y', 'trip_number.y', 'match')) %>%
  dplyr::rename('name_loc' = 'name_loc.x', 
                'street_loc' = 'street_loc.x',
                'village_loc' = 'village_loc.x',
                'city_loc' = 'city_loc.x',
                'district_loc' = 'district_loc.x',
                'trip_number' = 'trip_number.x') %>%
  mutate(merge_loc = 'manual match')

####################################
# PROCESS UNMATCHED TRAVEL GEOCODE #
####################################

# Merge resident characteristics for unmatched travel survey observations
# Create dataset of resident information to merge to the travel information
merge_resident <- survey_dat %>%
  dplyr::select(-c('city_loc', 'days_loc___1', 'days_loc___2', 'days_loc___3', 'days_loc___4', 
                   'duration_loc', 'street_loc', 'district_loc', 'name_loc', 'village_loc', 
                   'freq_loc', 'times_loc___1', 'times_loc___2', 'times_loc___3', 'times_loc___4',
                   'purpose_loc', 'purpose_oth_loc', 'res_merge'))
merge_resident$id <- as.character(merge_resident$id)
merge_resident$id = tolower(trimws(merge_resident$id, which = 'both'))

# Left merge on travel geocode to pick up resident information
geocode_dat_unmatch_6 <- dplyr::mutate(geocode_dat_unmatch_6, row_id = row_number(),
                                       id = tolower(trimws(id, which = 'both')))
survey_dat_6 <- left_join(geocode_dat_unmatch_6, merge_resident, 
                          by = c('id'))
survey_dat_6 <- survey_dat_6 %>% 
  dplyr::select(-c('trip_number.y')) %>%
  dplyr::rename('trip_number' = 'trip_number.x') %>%
  distinct(row_id, .keep_all = TRUE) %>%
  mutate(merge_loc = 'no merge, travel geocode append',
         res_merge = 'no merge, travel geocode') %>%
  dplyr::select(-c('row_id'))

# Add columns to rbind later
survey_dat_6$days_loc___1 <- NA
survey_dat_6$days_loc___2 <- NA
survey_dat_6$days_loc___3 <- NA
survey_dat_6$days_loc___4 <- NA
survey_dat_6$times_loc___1 <- NA
survey_dat_6$times_loc___2 <- NA
survey_dat_6$times_loc___3 <- NA
survey_dat_6$times_loc___4 <- NA
survey_dat_6$duration_loc <- NA
survey_dat_6$freq_loc <- NA
survey_dat_6$purpose_loc <- NA
survey_dat_6$purpose_oth_loc <- NA
survey_dat_6$in_geocode_loc <- NA

#################################
# PROCESS UNMATCHED SURVEY DATA #
#################################

# Geocode un-merged survey
# Create address variable based on location data availability
survey_dat_unmatch_6$geocode_address <- paste(survey_dat_unmatch_6$village_loc, 
                                              survey_dat_unmatch_6$city_loc,
                                              survey_dat_unmatch_6$district_loc,
                                              'Sri Lanka', sep = ', ')

survey_dat_unmatch_6$geocode_address <- ifelse(survey_dat_unmatch_6$village_loc == '', 
                                             paste(survey_dat_unmatch_6$city_loc, 
                                                   survey_dat_unmatch_6$district_loc,
                                                   'Sri Lanka', sep = ', '),
                                             survey_dat_unmatch_6$geocode_address)

# Replace precision variable
# 999 - geocoded to city/village level using google api
# 1000 - not geocoded
survey_dat_unmatch_6$precision_loc <- 999
survey_dat_unmatch_6 <- survey_dat_unmatch_6 %>% 
  mutate(precision_loc = ifelse(geocode_address == ', , Sri Lanka', 1000, precision_loc))

# Geocode
register_google(key = 'AIzaSyDmhLWCw5G-wzKS472eXaUAHU-d4RdaLAQ')
survey_dat_7 <- mutate_geocode(survey_dat_unmatch_6, location = geocode_address, output = 'latlona')

# Replace with geocode
survey_dat_7$lat_loc <- survey_dat_7$lat 
survey_dat_7$lon_loc <- survey_dat_7$lon 

# Remove variables
survey_dat_7 <- survey_dat_7 %>% 
  dplyr::select(-c('geocode_address', 'lat', 'lon', 'address', 'row_id')) %>%
  mutate(merge_loc = 'no merge, survey append')

# Add columns to rbind later
survey_dat_7$in_geocode_loc <- NA

#######################################
# APPEND ALL MATCHED AND UNMATCH DATA #
#######################################

# Append merged and geocoded datasets
# Any survey data that was not in the travel geocode and has complete address
# information is geocoded and appended
# Any travel geocode trip that is not in the survey but has id information is
# merged to resident information and appended
survey_dat_merged <- rbind(survey_dat_0, survey_dat_1, survey_dat_2, survey_dat_3,
                    survey_dat_4, survey_dat_5, survey_dat_6, survey_dat_7)

# Confirm same number of people across data sets
ids_merged <- c(combined_survey_long$id, baseline_survey$id, travel_survey_long$id)
length(unique(ids_merged))
verify(survey_dat_merged, length(unique(id)) == 317)

# Rename vars for ArcGIS
# Data will be input into ArcGIS to obtain administrative level information
survey_dat_merged <- survey_dat_merged %>%
  dplyr::rename('lon_res' = 'res_lon',
                'lat_res' = 'res_lat')

# Save data
write.csv(survey_dat_merged, './tmp/survey_dat_merged.csv', row.names = FALSE)

################################################################################
################################################################################

################################################################################
# File Name: 01g_process_survey_data                                           #
#                                                                              #
# Purpose: Load and format combined survey data.                               #
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Load and format combined survey                                   #
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

######################################
# 2. LOAD AND FORMAT COMBINED SURVEY #
######################################

# Load travel survey
combined_survey <- read.csv('./raw/UnderstandingDengueS_DATA_2022-06-21_1100.csv')

# Format travel survey data 
# Rename variables to a common naming convention
combined_survey <- combined_survey %>% 
  dplyr::select(-c(sample_collected)) %>%
  dplyr::rename('id' = 'idno',
                'name_loc.1' = 'name_loc1',
                'street_loc.1' = 'street_loc1',
                'village_loc.1' = 'village_loc1',
                'city_loc.1' = 'city_loc1',
                'district_loc.1' = 'district_loc1',
                'days_loc___1.1' = 'days_loc1___1',
                'days_loc___2.1' = 'days_loc1___2',
                'days_loc___3.1' = 'days_loc1___3',
                'days_loc___4.1' = 'days_loc1___4',
                'freq_loc.1' = 'freq_loc1',
                'times_loc___1.1' = 'times_loc1___1',
                'times_loc___2.1' = 'times_loc1___2',
                'times_loc___3.1' = 'times_loc1___3',
                'times_loc___4.1' = 'times_loc1___4',
                'duration_loc.1' = 'duration_loc1',
                'purpose_loc.1' = 'purpose_loc1',
                'purpose_oth_loc.1' = 'purpose_oth_loc1',
                'name_loc.2' = 'name_loc2',
                'street_loc.2' = 'street_loc2',
                'village_loc.2' = 'village_loc2',
                'city_loc.2' = 'city_loc2',
                'district_loc.2' = 'district_loc2',
                'days_loc___1.2' = 'days_loc2___1',
                'days_loc___2.2' = 'days_loc2___2',
                'days_loc___3.2' = 'days_loc2___3',
                'days_loc___4.2' = 'days_loc2___4',
                'freq_loc.2' = 'freq_loc2',
                'times_loc___1.2' = 'times_loc2___1',
                'times_loc___2.2' = 'times_loc2___2',
                'times_loc___3.2' = 'times_loc2___3',
                'times_loc___4.2' = 'times_loc2___4',
                'duration_loc.2' = 'duration_loc2',
                'purpose_loc.2' = 'purpose_loc2',
                'purpose_oth_loc.2' = 'purpose_oth_loc2',
                'name_loc.3' = 'name_loc3',
                'street_loc.3' = 'street_loc3',
                'village_loc.3' = 'village_loc3',
                'city_loc.3' = 'city_loc3',
                'district_loc.3' = 'district_loc3',
                'days_loc___1.3' = 'days_loc3___1',
                'days_loc___2.3' = 'days_loc3___2',
                'days_loc___3.3' = 'days_loc3___3',
                'days_loc___4.3' = 'days_loc3___4',
                'freq_loc.3' = 'freq_loc3',
                'times_loc___1.3' = 'times_loc3___1',
                'times_loc___2.3' = 'times_loc3___2',
                'times_loc___3.3' = 'times_loc3___3',
                'times_loc___4.3' = 'times_loc3___4',
                'duration_loc.3' = 'duration_loc3',
                'purpose_loc.3' = 'purpose_loc3',
                'purpose_oth_loc.3' = 'purpose_oth_loc3',
                'name_loc.4' = 'name_loc4',
                'street_loc.4' = 'street_loc4',
                'village_loc.4' = 'village_loc4',
                'city_loc.4' = 'city_loc4',
                'district_loc.4' = 'district_loc4',
                'days_loc___1.4' = 'days_loc4___1',
                'days_loc___2.4' = 'days_loc4___2',
                'days_loc___3.4' = 'days_loc4___3',
                'days_loc___4.4' = 'days_loc4___4',
                'freq_loc.4' = 'freq_loc4',
                'times_loc___1.4' = 'times_loc4___1',
                'times_loc___2.4' = 'times_loc4___2',
                'times_loc___3.4' = 'times_loc4___3',
                'times_loc___4.4' = 'times_loc4___4',
                'duration_loc.4' = 'duration_loc4',
                'purpose_loc.4' = 'purpose_loc4',
                'purpose_oth_loc.4' = 'purpose_oth_loc4',
                'name_loc.5' = 'name_loc5',
                'street_loc.5' = 'street_loc5',
                'village_loc.5' = 'village_loc5',
                'city_loc.5' = 'city_loc5',
                'district_loc.5' = 'district_loc5',
                'days_loc___1.5' = 'days_loc5___1',
                'days_loc___2.5' = 'days_loc5___2',
                'days_loc___3.5' = 'days_loc5___3',
                'days_loc___4.5' = 'days_loc5___4',
                'freq_loc.5' = 'freq_loc5',
                'times_loc___1.5' = 'times_loc5___1',
                'times_loc___2.5' = 'times_loc5___2',
                'times_loc___3.5' = 'times_loc5___3',
                'times_loc___4.5' = 'times_loc5___4',
                'duration_loc.5' = 'duration_loc5',
                'purpose_loc.5' = 'purpose_loc5',
                'purpose_oth_loc.5' = 'purpose_oth_loc5',
                'name_loc.6' = 'name_loc6',
                'street_loc.6' = 'street_loc6',
                'village_loc.6' = 'village_loc6',
                'city_loc.6' = 'city_loc6',
                'district_loc.6' = 'district_loc6',
                'days_loc___1.6' = 'days_loc6___1',
                'days_loc___2.6' = 'days_loc6___2',
                'days_loc___3.6' = 'days_loc6___3',
                'days_loc___4.6' = 'days_loc6___4',
                'freq_loc.6' = 'freq_loc6',
                'times_loc___1.6' = 'times_loc6___1',
                'times_loc___2.6' = 'times_loc6___2',
                'times_loc___3.6' = 'times_loc6___3',
                'times_loc___4.6' = 'times_loc6___4',
                'duration_loc.6' = 'duration_loc6',
                'purpose_loc.6' = 'purpose_loc6',
                'purpose_oth_loc.6' = 'purpose_oth_loc6',
                'name_loc.7' = 'name_loc7',
                'street_loc.7' = 'street_loc7',
                'village_loc.7' = 'village_loc7',
                'city_loc.7' = 'city_loc7',
                'district_loc.7' = 'district_loc7',
                'days_loc___1.7' = 'days_loc7___1',
                'days_loc___2.7' = 'days_loc7___2',
                'days_loc___3.7' = 'days_loc7___3',
                'days_loc___4.7' = 'days_loc7___4',
                'freq_loc.7' = 'freq_loc7',
                'times_loc___1.7' = 'times_loc7___1',
                'times_loc___2.7' = 'times_loc7___2',
                'times_loc___3.7' = 'times_loc7___3',
                'times_loc___4.7' = 'times_loc7___4',
                'duration_loc.7' = 'duration_loc7',
                'purpose_loc.7' = 'purpose_loc7',
                'purpose_oth_loc.7' = 'purpose_oth_loc7',
                'name_loc.8' = 'name_loc8',
                'street_loc.8' = 'street_loc8',
                'village_loc.8' = 'village_loc8',
                'city_loc.8' = 'city_loc8',
                'district_loc.8' = 'district_loc8',
                'days_loc___1.8' = 'days_loc8___1',
                'days_loc___2.8' = 'days_loc8___2',
                'days_loc___3.8' = 'days_loc8___3',
                'days_loc___4.8' = 'days_loc8___4',
                'freq_loc.8' = 'freq_loc8',
                'times_loc___1.8' = 'times_loc8___1',
                'times_loc___2.8' = 'times_loc8___2',
                'times_loc___3.8' = 'times_loc8___3',
                'times_loc___4.8' = 'times_loc8___4',
                'duration_loc.8' = 'duration_loc8',
                'purpose_loc.8' = 'purpose_loc8',
                'purpose_oth_loc.8' = 'purpose_oth_loc8',
                'name_loc.9' = 'name_loc9',
                'street_loc.9' = 'street_loc9',
                'village_loc.9' = 'village_loc9',
                'city_loc.9' = 'city_loc9',
                'district_loc.9' = 'district_loc9',
                'days_loc___1.9' = 'days_loc9___1',
                'days_loc___2.9' = 'days_loc9___2',
                'days_loc___3.9' = 'days_loc9___3',
                'days_loc___4.9' = 'days_loc9___4',
                'freq_loc.9' = 'freq_loc9',
                'times_loc___1.9' = 'times_loc9___1',
                'times_loc___2.9' = 'times_loc9___2',
                'times_loc___3.9' = 'times_loc9___3',
                'times_loc___4.9' = 'times_loc9___4',
                'duration_loc.9' = 'duration_loc9',
                'purpose_loc.9' = 'purpose_loc9',
                'purpose_oth_loc.9' = 'purpose_oth_loc9',
                'name_loc.10' = 'name_loc10',
                'street_loc.10' = 'street_loc10',
                'village_loc.10' = 'village_loc10',
                'city_loc.10' = 'city_loc10',
                'district_loc.10' = 'district_loc10',
                'days_loc___1.10' = 'days_loc10___1',
                'days_loc___2.10' = 'days_loc10___2',
                'days_loc___3.10' = 'days_loc10___3',
                'days_loc___4.10' = 'days_loc10___4',
                'freq_loc.10' = 'freq_loc10',
                'times_loc___1.10' = 'times_loc10___1',
                'times_loc___2.10' = 'times_loc10___2',
                'times_loc___3.10' = 'times_loc10___3',
                'times_loc___4.10' = 'times_loc10___4',
                'duration_loc.10' = 'duration_loc10',
                'purpose_loc.10' = 'purpose_loc10',
                'purpose_oth_loc.10' = 'purpose_oth_loc10')

# Reshape survey data from wide to long
# Each row is a single trip location
combined_survey_long  <- combined_survey %>%
  melt(id.vars = c('id', 'dt_survey', 'redcap_event_name',	'init',	'dt_consent',	
                   'age',	'sex',	'res_name',	'res_street',	'res_village', 'res_city', 
                   'res_district', 'diag_test',	'dhf', 'dt_onset',	'dt_admit',	
                   'dt_sample', 'barcode', 'travel_obtained', 'travel_attendant', 
                   'baseline_survey_complete', 'travel_survey_complete')) %>%
  # Create trip number variable
  mutate(trip_number = as.numeric(str_sub(variable, -1, -1)),
         trip_number = ifelse(trip_number == 0, 10, trip_number),
         variable = ifelse(trip_number != 10, 
                           str_sub(variable, 1, str_length(variable)-2),
                           str_sub(variable, 1, str_length(variable)-3))) %>%
  dcast(id + dt_survey + trip_number + redcap_event_name	+ init + dt_consent	+ 
          age	+ sex	+ res_name + res_street	+ res_village	+ res_city	+ res_district +
          diag_test	+ dhf	+ dt_onset + dt_admit + dt_sample + barcode + 
          travel_obtained	+ travel_attendant + baseline_survey_complete + 
          travel_survey_complete ~ variable) %>%
  mutate(drop = ifelse(is.na(purpose_loc) & name_loc == '' & street_loc == '' & 
                         village_loc == '' & city_loc == '' & district_loc == '', 1, 0),
         drop = ifelse(travel_survey_complete == 0 & trip_number == 1, 0, drop)) %>%
  # Remove blank rows (example: only took 2 trips but 10 trips allowed (8 empty rows))
  filter(drop == 0) 

# Confirm no person is dropped
length(unique(combined_survey$id))
verify(combined_survey_long, length(unique(id)) == 316)

# Save data
save(combined_survey_long, file = './tmp/combined_survey_dat.RData')

################################################################################
################################################################################

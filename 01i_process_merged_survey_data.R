################################################################################
# File Name: 01i_process_merged_survey_data                                    #
# Purpose: Restrict merged survey data and add on geographical information.    #                                                     #
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Load and restrict merged survey data                              #
#         3. Spatial merge in ArcGIS to get ADM1, ADM2, ADM3 level data        #
#         4. Load spatial join data and merge to survey data                   #
#         5. Merge on administrative codes
#         6. Aggregate data to administrative levels 1, 2, 3                   #
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
library(readxl)

# Set the seed
set.seed(12345)

# Set directory 
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

###########################################
# 2. LOAD AND RESTRICT MERGED SURVEY DATA #
###########################################

# Load data
survey_dat <- read.csv('./tmp/survey_dat_merged.csv')

# Restrict data and remove variables from the merge process
survey_dat <- survey_dat %>%
  dplyr::select(-c('drop', 'in_survey_res', 'in_geocode_res', 'in_survey_loc', 'in_geocode_loc')) %>%
  dplyr::filter(precision_loc != 1000) %>%
  dplyr::filter(res_precision != 1000) %>%
  dplyr::filter(!is.na(lon_loc)) %>%
  dplyr::filter(!is.na(lat_loc)) %>%
  dplyr::filter(!is.na(lon_res)) %>%
  dplyr::filter(!is.na(lat_res))

# Create row_id variable
survey_dat <- dplyr::mutate(survey_dat, row_id = row_number())

# Save to import into ArcGIS
write.csv(survey_dat, './tmp/survey_dat_restricted.csv', row.names = FALSE)

#################################################################
# 3. SPATIAL MERGE IN ARCGIS TO GET ADM1, ADM2, ADM3 LEVEL DATA #
#################################################################

# 1. Loaded survey data into ArcGIS
# 2. Converted data to X-Y point on map for resident information
# 3. Converted data to X-Y point on map for location information
# 4. Loaded shp file for Sri Lanka administrative units
# 5. Completed spatial join for resident point to administrative unit
# 6. Completed spatial join for location point to administrative unit
# 7. Exported spatially joined resident point information
# 8. Exported spatially joined location point information

######################################################
# 4. LOAD SPATIAL JOIN DATA AND MERGE TO SURVEY DATA #
######################################################

# Load spatial join resident information
survey_res <- read_excel('./raw/survey_dat_res.xlsx')
# Select and rename variables
survey_res <- survey_res %>%
  dplyr::select(c('id', 'row_id', 'ADM3_EN', 'ADM2_EN', 'ADM1_EN')) %>%
  dplyr::rename('adm3_res' = 'ADM3_EN', 
                'adm2_res' = 'ADM2_EN',
                'adm1_res' = 'ADM1_EN')

# Merge resident admin units to survey data
survey_dat <- left_join(survey_dat, survey_res, by = c('id', 'row_id'))
          
# load spatial join location information
survey_loc <- read_excel('./raw/survey_dat_loc.xlsx')
# Select and rename variables
survey_loc <- survey_loc %>%
  dplyr::select(c('id', 'row_id', 'ADM3_EN', 'ADM2_EN', 'ADM1_EN')) %>%
  dplyr::rename('adm3_loc' = 'ADM3_EN', 
                'adm2_loc' = 'ADM2_EN',
                'adm1_loc' = 'ADM1_EN')

# Merge location admin units to survey data
survey_dat <- left_join(survey_dat, survey_loc, by = c('id', 'row_id'))

# Assert no missing admin data after merges
survey_dat %>% assert(not_na, 
                      adm3_res, adm2_res, adm1_res,
                      adm3_loc, adm2_loc, adm1_loc)

# Fill in missing age and sex values with those present for the person on a different row
survey_dat <- survey_dat %>%
  dplyr::group_by(id) %>%
  fill(sex, .direction = "downup") %>%
  fill(age, .direction = "downup") %>%
  dplyr::ungroup()

# Create trips variable
# Each line is a single trip
survey_dat$trip <- 1

####################################
# 5. MERGE ON ADMINISTRATIVE CODES #
####################################

######################################################
# 6. AGGREGATE DATA TO ADMINISTRATIVE LEVELS 1, 2, 3 #
######################################################

# Administrative Level 3
adm_3_survey_mobility_dat <- survey_dat %>% group_by(adm3_res, adm3_loc) %>%
  dplyr::mutate(trips = sum(trip)) %>%
  distinct(adm3_res, adm3_loc, adm2_res, adm2_loc, adm1_res, adm1_loc, trips, 
           .keep_all = FALSE) %>%
  ungroup() 

# Administrative Level 2
adm_2_survey_mobility_dat <- survey_dat %>% group_by(adm2_res, adm2_loc) %>%
  dplyr::mutate(trips = sum(trip)) %>%
  distinct(adm2_res, adm2_loc, adm1_res, adm1_loc, trips, 
           .keep_all = FALSE) %>%
  ungroup() 

# Administrative Level 1
adm_1_survey_mobility_dat <- survey_dat %>% group_by(adm1_res, adm1_loc) %>%
  dplyr::mutate(trips = sum(trip)) %>%
  distinct(adm1_res, adm1_loc, trips, 
           .keep_all = FALSE) %>%
  ungroup() 

# Save different admin levels of survey data
save(list = c('adm_3_survey_mobility_dat', 
              'adm_2_survey_mobility_dat', 
              'adm_1_survey_mobility_dat'), 
     file = './tmp/adm_survey_mobility_dat.RData')

################################################################################
################################################################################

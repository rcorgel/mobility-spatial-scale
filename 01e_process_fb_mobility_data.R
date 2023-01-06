################################################################################
# File Name: 01e_process_fb_mobility_data                                      #
#                                                                              #
# Purpose:   Load and format facebook data used in analyses.                   #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load and format Facebook data                                  #
#            3. Load ArcGIS crosswalk and merge to Facebook data               # 
#            4. Merge on administrative codes                                  #
#            5. Collapse data to different admin levels                        #
#                                                                              #
# Project:   Sri Lanka Spatial Aggregation                                     #
# Author:    Ronan Corgel                                                      #
################################################################################

####################
# 1. SET-UP SCRIPT #
####################

# Start with a clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(readxl)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

##############################
# 2. LOAD AND FORMAT FB DATA #
##############################

# Load tile data flows
fb_mobility_dat <- read_csv('./raw/FB_daily_8hr_flows_tiles_2020_22.csv')

# Load admin 1 data flows
fb_mobility_dat_adm_1 <- read_csv('./raw/FB_daily_8hr_flows_admin_2020_22.csv')

# Export distinct data to create crosswalk (needed to spatial merge in ArcGIS)
# Get distinct lat/lon pairs from origin and rename 
start <- fb_mobility_dat %>% dplyr::distinct(start_lat, start_lon, .keep_all = FALSE) %>%
  dplyr::rename('lon' = 'start_lon',
                'lat' = 'start_lat')
# Get distinct lat/lon pairs from destinations and rename
end <- fb_mobility_dat %>% dplyr::distinct(end_lat, end_lon, .keep_all = FALSE) %>%
  dplyr::rename('lon' = 'end_lon',
                'lat' = 'end_lat')
# Combine and get all distinct lat/lon pairs
total <- rbind(start, end)
total <- dplyr::distinct(total, lat, lon, .keep_all = FALSE) 

# Save to csv, for importation to ArcGIS
write.csv(total, file = './tmp/fb_shape_xwalk.csv', 
          row.names = FALSE)

# Perform spatial merge in ArcGIS to match tiles to admin levels

#################################################
# 3. LOAD ARCGIS CROSSWALK AND MERGE TO FB DATA #
#################################################

# Load crosswalk created from ArcGIS
merge_dat <- read_excel('./tmp/fb_shape_xwalk.xlsx') %>% 
  dplyr::select(c('lat', 'lon', 'ADM3_EN', 'ADM2_EN', 'ADM1_EN'))

# Merge on mobile phone data to shape file cross walk
mobility_shape_xwalk <- read.csv('./tmp/mobility_shape_xwalk.csv')
merge_dat <- left_join(merge_dat, mobility_shape_xwalk,
                       by = c('ADM3_EN' = 'adm_3_shape'))

# Merge to origin lat/lon
fb_mobility_dat <- left_join(fb_mobility_dat, merge_dat, c('start_lat' = 'lat', 'start_lon' = 'lon'))  
fb_mobility_dat <- fb_mobility_dat %>%
  dplyr::rename('adm_3_origin' = 'adm_3_mobility', 
                'adm_2_origin' = 'ADM2_EN',
                'adm_1_origin' = 'ADM1_EN')

# Merge to destination lat/lon
fb_mobility_dat <- left_join(fb_mobility_dat, merge_dat, c('end_lat' = 'lat', 'end_lon' = 'lon'))  
fb_mobility_dat <- fb_mobility_dat %>%
  dplyr::rename('adm_3_destination' = 'adm_3_mobility', 
                'adm_2_destination' = 'ADM2_EN',
                'adm_1_destination' = 'ADM1_EN')

# Summarize data
summary(fb_mobility_dat)

# Drop NA lat/lon (only 22 obs drop)
fb_mobility_dat <- fb_mobility_dat %>% 
  dplyr::filter(!is.na(start_lat)) %>%
  dplyr::filter(!is.na(start_lon)) %>%
  dplyr::filter(!is.na(end_lat)) %>%
  dplyr::filter(!is.na(end_lon))

# Check number of admin units
verify(fb_mobility_dat, length(unique(adm_3_origin)) < 340)        # only 337 in data
verify(fb_mobility_dat, length(unique(adm_3_destination)) < 340)   # only 337 in data
verify(fb_mobility_dat, length(unique(adm_2_origin)) == 25)
verify(fb_mobility_dat, length(unique(adm_2_destination)) == 25)
verify(fb_mobility_dat, length(unique(adm_1_origin)) == 9)
verify(fb_mobility_dat, length(unique(adm_1_destination)) == 9)

####################################
# 4. MERGE ON ADMINISTRATIVE CODES #
####################################

# Merge on admin codes
# Administrative Level 3
admin_xwalk <- read_csv('./tmp/admin_xwalk.csv')
adm_3_fb_mobility_dat <- left_join(adm_3_fb_mobility_dat, admin_xwalk[c('adm_3_code', 'adm_3')],
                                   by = c('adm_3_origin' = 'adm_3'))
adm_3_fb_mobility_dat <- left_join(adm_3_fb_mobility_dat, admin_xwalk[c('adm_3_code', 'adm_3')],
                                   by = c('adm_3_destination' = 'adm_3'))
adm_3_fb_mobility_dat <- adm_3_fb_mobility_dat %>% 
  dplyr::rename('adm_3_origin_code' = 'adm_3_code.x',
                'adm_3_destination_code' = 'adm_3_code.y')


##############################################
# 5. COLLAPSE DATA TO DIFFERENT ADMIN LEVELS #
##############################################

# Collapse to different admin levels (and day level)
# Calculate daily average trips between locations
# Admin level 3
adm_3_fb_mobility_dat <- fb_mobility_dat %>% group_by(Date, adm_3_origin, adm_3_destination) %>%
  dplyr::filter(!is.na(n_baseline)) %>%
  dplyr::mutate(trips = sum(n_baseline, na.rm = TRUE)) %>%
  distinct(Date, adm_3_origin, adm_3_destination, trips, adm_2_origin, adm_2_destination,
           adm_1_origin, adm_1_destination, .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(adm_3_origin, adm_3_destination) %>%
  dplyr::mutate(trips_avg = mean(trips)) %>%
  distinct(adm_3_origin, adm_3_destination, trips_avg, adm_2_origin, adm_2_destination,
           adm_1_origin, adm_1_destination, .keep_all = FALSE) %>%
  ungroup()

# Admin level 2
adm_2_fb_mobility_dat <- fb_mobility_dat %>% group_by(Date, adm_2_origin, adm_2_destination) %>%
  dplyr::filter(!is.na(n_baseline)) %>%
  dplyr::mutate(trips = sum(n_baseline, na.rm = TRUE)) %>%
  distinct(Date, trips, adm_2_origin, adm_2_destination, adm_1_origin, adm_1_destination, 
           .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(adm_2_origin, adm_2_destination) %>%
  dplyr::mutate(trips_avg = mean(trips)) %>%
  distinct(trips_avg, adm_2_origin, adm_2_destination,
           adm_1_origin, adm_1_destination, .keep_all = FALSE) %>%
  ungroup()

# Admin level 1
adm_1_fb_mobility_dat <- fb_mobility_dat_adm_1 %>% group_by(Date, start_polygon_name, end_polygon_name) %>%
  dplyr::filter(!is.na(n_baseline)) %>%
  dplyr::mutate(trips = sum(n_baseline, na.rm = TRUE)) %>%
  dplyr::rename('adm_1_origin' = 'start_polygon_name',
                'adm_1_destination' = 'end_polygon_name') %>%
  distinct(Date, trips, adm_1_origin, adm_1_destination, .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(adm_1_origin, adm_1_destination) %>%
  dplyr::mutate(trips_avg = mean(trips)) %>%
  distinct(trips_avg, adm_1_origin, adm_1_destination, .keep_all = FALSE) %>%
  ungroup()

# Save different admin levels of FB data
save(list = c('adm_3_fb_mobility_dat', 
              'adm_2_fb_mobility_dat', 
              'adm_1_fb_mobility_dat'), 
     file = './tmp/adm_fb_mobility_dat.RData')

################################################################################
################################################################################

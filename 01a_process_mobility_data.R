################################################################################
# File Name: 01a_process_mobility_data                                         #
#                                                                              #
# Purpose:   Load and format mobility data used in analyses.                   #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load and format mobility data                                  #
#            3. Adjust for February/March mobility data issue                  #
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
library(lubridate)
library(reshape)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

####################################
# 2. LOAD AND FORMAT MOBILITY DATA #
####################################

# Load and format November data
nov_mobility_dat <- read_csv('./raw/export_for_JHU_November_2021_V2.csv')
nov_mobility_dat <- nov_mobility_dat %>%
  dplyr::rename(date = batch_date) %>%
  mutate(origin_long = as.numeric(str_extract(origin_area_centroid, "\\d+\\.*\\d+")),
         origin_lat = as.numeric(str_extract(origin_area_centroid, "\\s(\\d+\\.*\\d+)")),
         destination_long = as.numeric(str_extract(destination_area_centroid, "\\d+\\.*\\d+")),
         destination_lat = as.numeric(str_extract(destination_area_centroid, "\\s\\d+\\.*\\d+")),
         month = month(date)) %>%
  dplyr::select(-c(origin_area_centroid, destination_area_centroid, devices))

# Load and format December data
dec_mobility_dat <- read_csv('./raw/export_for_JHU_December_2021_V2.csv')
dec_mobility_dat <- dec_mobility_dat %>%
  dplyr::rename(date = batch_date) %>%
  mutate(origin_long = as.numeric(str_extract(origin_area_centroid, "\\d+\\.\\d+")),
         origin_lat = as.numeric(str_extract(origin_area_centroid, "\\s(\\d+\\.\\d+)")),
         destination_long = as.numeric(str_extract(destination_area_centroid, "\\d+\\.\\d+")),
         destination_lat = as.numeric(str_extract(destination_area_centroid, "\\s\\d+\\.\\d+")),
         month = month(date)) %>%
  dplyr::select(-c(origin_area_centroid, destination_area_centroid, devices))

# Load and format January data
jan_mobility_dat <- read_csv('./raw/export_for_JHU_January_2022.csv')
jan_mobility_dat <- jan_mobility_dat %>%
  dplyr::rename(date = batch_date) %>%
  mutate(origin_long = as.numeric(str_extract(origin_area_centroid, "\\d+\\.\\d+")),
         origin_lat = as.numeric(str_extract(origin_area_centroid, "\\s(\\d+\\.\\d+)")),
         destination_long = as.numeric(str_extract(destination_area_centroid, "\\d+\\.\\d+")),
         destination_lat = as.numeric(str_extract(destination_area_centroid, "\\s\\d+\\.\\d+")),
         month = month(date)) %>%
  dplyr::select(-c(origin_area_centroid, destination_area_centroid, devices))

# Load and format February data
feb_mobility_dat <- read_csv('./raw/export_for_JHU_February_2022.csv')
feb_mobility_dat <- feb_mobility_dat %>%
  dplyr::rename(date = batch_date) %>%
  mutate(origin_long = as.numeric(str_extract(origin_area_centroid, "\\d+\\.\\d+")),
         origin_lat = as.numeric(str_extract(origin_area_centroid, "\\s(\\d+\\.\\d+)")),
         destination_long = as.numeric(str_extract(destination_area_centroid, "\\d+\\.\\d+")),
         destination_lat = as.numeric(str_extract(destination_area_centroid, "\\s\\d+\\.\\d+")),
         month = month(date)) %>%
  dplyr::select(-c(origin_area_centroid, destination_area_centroid))

# Load and format March data
mar_mobility_dat <- read_csv('./raw/export_for_JHU_March_2022.csv')
mar_mobility_dat <- mar_mobility_dat %>%
  dplyr::rename(date = batch_date) %>%
  mutate(origin_long = as.numeric(str_extract(origin_area_centroid, "\\d+\\.\\d+")),
         origin_lat = as.numeric(str_extract(origin_area_centroid, "\\s(\\d+\\.\\d+)")),
         destination_long = as.numeric(str_extract(destination_area_centroid, "\\d+\\.\\d+")),
         destination_lat = as.numeric(str_extract(destination_area_centroid, "\\s\\d+\\.\\d+")),
         month = month(date)) %>%
  dplyr::select(-c(origin_area_centroid, destination_area_centroid))

# Combine data into one data set
mobility_dat <- rbind(nov_mobility_dat, dec_mobility_dat, jan_mobility_dat, 
                      feb_mobility_dat, mar_mobility_dat)

# Merge on provinces (ADM 1)
district_province_xwalk <- 
  read_csv('./raw/district_province_xwalk.csv')
mobility_dat <- left_join(mobility_dat, district_province_xwalk, 
                          by = c('admin_level_2_origin' = 'district'))
mobility_dat <- left_join(mobility_dat, district_province_xwalk, 
                          by = c('admin_level_2_destination' = 'district'))

# Rename variables to match variable naming conventions
mobility_dat <- mobility_dat %>%
  dplyr::rename('adm_1_origin' = 'province.x',
         'adm_1_destination' = 'province.y',
         'adm_3_origin' = 'origin_area',
         'adm_3_destination' = 'destination_area',
         'adm_3_origin_code' = 'origin_area_code',
         'adm_3_destination_code' = 'destination_area_code',
         'adm_2_origin' = 'admin_level_2_origin',
         'adm_2_destination' = 'admin_level_2_destination',
         'adm_2_origin_code' = 'admin_level_2_origin_code',
         'adm_2_destination_code' = 'admin_level_2_destination_code',
         'trips' = 'people') 

# Replace missing data with unknown
mobility_dat$adm_1_origin <- 
  ifelse(is.na(mobility_dat$adm_1_origin), 
         '[unknown]', mobility_dat$adm_1_origin)
mobility_dat$adm_1_destination <- 
  ifelse(is.na(mobility_dat$adm_1_destination), 
         '[unknown]', mobility_dat$adm_1_destination)

# Drop unknown destination and/or origin
mobility_dat <- mobility_dat %>% 
  filter(adm_1_origin != '[unknown]') %>% 
  filter(adm_1_destination != '[unknown]') %>%
  filter(adm_3_origin != '[unknown]') %>% 
  filter(adm_3_destination != '[unknown]')

# Save data
save(mobility_dat, file = './tmp/mobility_dat.RData')

# Save a version without February and March
mobility_dat_nov_jan <- mobility_dat %>%
  filter(month != 2) %>%
  filter(month != 3)
save(mobility_dat_nov_jan, file = './tmp/mobility_dat_nov_jan.RData')

# Remove individual month mobility data data
rm(list = c('nov_mobility_dat', 'dec_mobility_dat', 'jan_mobility_dat', 
            'feb_mobility_dat', 'mar_mobility_dat', 'mobility_dat_nov_jan'))

####################################################
# 3. ADJUST FOR FEBRUARY/MARCH MOBILITY DATA ISSUE #
####################################################

# Due to issues with the data provider, the volume of trips in February and 
# March was significantly lower than the volume of trips in previous months.

# Quote from provider: "We faced data quality issues with the input data from 
# the Dialog Axiata, and therefore we, unfortunately, had to work with a subset 
# of the devices (~20%). This implies that some of the trips we were able to 
# observe in January (more than 5 devices per trip, or else trip is excluded) 
# were not observed in February, reducing the number of rows in the table. We 
# can’t extrapolate the number of people for the trips that are not observed in 
# February. The overall outcome of this issue is that there appear to be less 
# people traveling in February than in January; however, the distribution of 
# trips remains similar to that observed in January. It’s worth pointing out 
# that the density of the signals for February is as good as it was for January 
# and that the geographical distribution is also similar.”

# To correct for this, for each route, the January month average is compared to 
# the February and March averages. This ratio, serves as a multiplier on the 
# daily trip counts for each route in February and March, respectively.

# Load data 
load('./tmp/mobility_dat.RData')

# Calculate January, February, and March month average for all routes
# Some months will have NA as the average if no trips occurred during that month
mobility_dat <- mobility_dat %>% 
  group_by(month, adm_3_origin, adm_3_destination) %>%
  mutate(trips_month_avg = mean(trips, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(adm_3_origin, adm_3_destination) %>%
  mutate(trips_month_avg_jan = trips_month_avg[month == 1][1],
         trips_month_avg_feb = trips_month_avg[month == 2][1],
         trips_month_avg_mar = trips_month_avg[month == 3][1])

# Create new adjusted trips variable
mobility_dat <- mobility_dat %>% 
  mutate(trips_adj = ifelse(month == 2, 
                            trips * (trips_month_avg_jan/trips_month_avg_feb),
                            NA),        # for February
         trips_adj = ifelse(month == 3, 
                            trips * (trips_month_avg_jan/trips_month_avg_mar),
                            trips_adj), # for March
         trips_adj = ifelse(is.na(trips_adj), 
                            trips,
                            trips_adj), # for all other months
         trips_adj = ifelse(trips_adj < trips, 
                            trips,
                            trips_adj)) # if lower after adj, replace with observed

# Re-save full mobility data
save(mobility_dat, file = './tmp/mobility_dat.RData')

################################################################################
################################################################################

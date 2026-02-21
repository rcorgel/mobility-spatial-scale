################################################################################
# File Name: 01a_process_phone_mobility_data                                   #
#                                                                              #
# Purpose:   Load and format phone mobility data used in analyses.             #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load and format mobility data                                  #
#            3. Adjust for February/March mobility data issue                  #
#            4. Aggregate data to administrative levels 1, 2, 3                #
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
library(assertr)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project')

####################################
# 2. LOAD AND FORMAT MOBILITY DATA #
####################################

# Load and format November data
nov_mobility_dat <- read_csv('./raw/export_for_JHU_November_2021_V2.csv')
nov_mobility_dat <- nov_mobility_dat |>
  dplyr::rename(date = batch_date) |>
  mutate(origin_long = as.numeric(str_extract(origin_area_centroid, "\\d+\\.*\\d+")),
         origin_lat = as.numeric(str_extract(origin_area_centroid, "\\s(\\d+\\.*\\d+)")),
         destination_long = as.numeric(str_extract(destination_area_centroid, "\\d+\\.*\\d+")),
         destination_lat = as.numeric(str_extract(destination_area_centroid, "\\s\\d+\\.*\\d+")),
         month = month(date)) |>
  dplyr::select(-c(origin_area_centroid, destination_area_centroid, devices))

# Load and format December data
dec_mobility_dat <- read_csv('./raw/export_for_JHU_December_2021_V2.csv')
dec_mobility_dat <- dec_mobility_dat |>
  dplyr::rename(date = batch_date) |>
  mutate(origin_long = as.numeric(str_extract(origin_area_centroid, "\\d+\\.\\d+")),
         origin_lat = as.numeric(str_extract(origin_area_centroid, "\\s(\\d+\\.\\d+)")),
         destination_long = as.numeric(str_extract(destination_area_centroid, "\\d+\\.\\d+")),
         destination_lat = as.numeric(str_extract(destination_area_centroid, "\\s\\d+\\.\\d+")),
         month = month(date)) |>
  dplyr::select(-c(origin_area_centroid, destination_area_centroid, devices))

# Load and format January data
jan_mobility_dat <- read_csv('./raw/export_for_JHU_January_2022.csv')
jan_mobility_dat <- jan_mobility_dat |>
  dplyr::rename(date = batch_date) |>
  mutate(origin_long = as.numeric(str_extract(origin_area_centroid, "\\d+\\.\\d+")),
         origin_lat = as.numeric(str_extract(origin_area_centroid, "\\s(\\d+\\.\\d+)")),
         destination_long = as.numeric(str_extract(destination_area_centroid, "\\d+\\.\\d+")),
         destination_lat = as.numeric(str_extract(destination_area_centroid, "\\s\\d+\\.\\d+")),
         month = month(date)) |>
  dplyr::select(-c(origin_area_centroid, destination_area_centroid, devices))

# Load and format February data
feb_mobility_dat <- read_csv('./raw/export_for_JHU_February_2022.csv')
feb_mobility_dat <- feb_mobility_dat |>
  dplyr::rename(date = batch_date) |>
  mutate(origin_long = as.numeric(str_extract(origin_area_centroid, "\\d+\\.\\d+")),
         origin_lat = as.numeric(str_extract(origin_area_centroid, "\\s(\\d+\\.\\d+)")),
         destination_long = as.numeric(str_extract(destination_area_centroid, "\\d+\\.\\d+")),
         destination_lat = as.numeric(str_extract(destination_area_centroid, "\\s\\d+\\.\\d+")),
         month = month(date)) |>
  dplyr::select(-c(origin_area_centroid, destination_area_centroid))

# Load and format March data
mar_mobility_dat <- read_csv('./raw/export_for_JHU_March_2022.csv')
mar_mobility_dat <- mar_mobility_dat |>
  dplyr::rename(date = batch_date) |>
  mutate(origin_long = as.numeric(str_extract(origin_area_centroid, "\\d+\\.\\d+")),
         origin_lat = as.numeric(str_extract(origin_area_centroid, "\\s(\\d+\\.\\d+)")),
         destination_long = as.numeric(str_extract(destination_area_centroid, "\\d+\\.\\d+")),
         destination_lat = as.numeric(str_extract(destination_area_centroid, "\\s\\d+\\.\\d+")),
         month = month(date)) |>
  dplyr::select(-c(origin_area_centroid, destination_area_centroid))

# Combine data into one data set
mobility_dat <- rbind(nov_mobility_dat, dec_mobility_dat, jan_mobility_dat, 
                      feb_mobility_dat, mar_mobility_dat)
# Assert that all 5 months are in the data
mobility_dat %>% assert(in_set(c(1, 2, 3, 11, 12)), month)

# Merge on provinces and codes (ADM 1)
district_province_xwalk <- read_csv('./raw/district_province_xwalk.csv')
mobility_dat <- left_join(mobility_dat, district_province_xwalk, 
                          by = c('admin_level_2_origin' = 'district'))
mobility_dat <- left_join(mobility_dat, district_province_xwalk, 
                          by = c('admin_level_2_destination' = 'district'))

# Rename variables to match variable naming conventions
mobility_dat <- mobility_dat |>
  dplyr::rename('adm_1_origin' = 'province.x',
         'adm_1_destination' = 'province.y',
         'adm_1_origin_code' = 'province_code.x',
         'adm_1_destination_code' = 'province_code.y',
         'adm_3_origin' = 'origin_area',
         'adm_3_destination' = 'destination_area',
         'adm_3_origin_code' = 'origin_area_code',
         'adm_3_destination_code' = 'destination_area_code',
         'adm_2_origin' = 'admin_level_2_origin',
         'adm_2_destination' = 'admin_level_2_destination',
         'adm_2_origin_code' = 'admin_level_2_origin_code',
         'adm_2_destination_code' = 'admin_level_2_destination_code',
         'trips' = 'people') 

# Drop unknown destination and/or origin
# Confirm unknowns are a very low percentage (0.3%)
nrow(mobility_dat[mobility_dat$adm_3_origin == '[unknown]' | 
                    mobility_dat$adm_3_destination == '[unknown]',]) / nrow(mobility_dat)
# Complete drop
mobility_dat <- mobility_dat |>
  filter(adm_3_origin != '[unknown]') |>
  filter(adm_3_destination != '[unknown]')

# Assert admin variables are not missing
mobility_dat |> assert(not_na, 
                        adm_1_origin, adm_1_destination, 
                        adm_1_origin_code, adm_1_destination_code,
                        adm_2_origin, adm_2_destination, 
                        adm_2_origin_code, adm_2_destination_code,
                        adm_3_origin, adm_3_destination, 
                        adm_3_origin_code, adm_3_destination_code)

# Save a version without February and March (due to data issue)
phone_mobility_dat_nov_jan <- mobility_dat |>
  filter(month != 2) |>
  filter(month != 3)
saveRDS(phone_mobility_dat_nov_jan, './tmp/phone_mobility_dat_nov_jan.rds')

# Remove individual month mobility data
rm(list = c('nov_mobility_dat', 'dec_mobility_dat', 'jan_mobility_dat', 
            'feb_mobility_dat', 'mar_mobility_dat', 'phone_mobility_dat_nov_jan'))

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

# Calculate January, February, and March month average for all routes
# Some months will have NA as the average if no trips occurred during that month
mobility_dat <- mobility_dat |>
  group_by(month, adm_3_origin, adm_3_destination) |>
  mutate(trips_month_avg = mean(trips, na.rm = TRUE)) |>
  ungroup() %>%
  group_by(adm_3_origin, adm_3_destination) %>%
  mutate(trips_month_avg_jan = trips_month_avg[month == 1][1],
         trips_month_avg_feb = trips_month_avg[month == 2][1],
         trips_month_avg_mar = trips_month_avg[month == 3][1]) |>
  ungroup()

# Create new adjusted trips variable
# Rename to more explicit name
phone_mobility_dat <- mobility_dat |>
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

# Assert not missing trips_adj
phone_mobility_dat |> assert(not_na, trips_adj)

# Save full mobility data
saveRDS(phone_mobility_dat, './tmp/phone_mobility_dat.rds')

######################################################
# 4. AGGREGATE DATA TO ADMINISTRATIVE LEVELS 1, 2, 3 #
######################################################

# Collapse to different admin levels (and day level)
# Calculate daily average trips between locations

# Admin level 3
adm_3_phone_mobility_dat <- phone_mobility_dat |> 
  # Calculate average daily trips
  group_by(adm_3_origin, adm_3_destination) |>
  mutate(trips_avg = mean(trips_adj, na.rm = TRUE)) |>
  distinct(adm_3_origin, adm_3_destination, trips_avg, 
           adm_3_origin_code, adm_3_destination_code, 
           adm_2_origin, adm_2_destination, 
           adm_2_origin_code, adm_2_destination_code, 
           adm_1_origin, adm_1_destination, 
           adm_1_origin_code, adm_1_destination_code, .keep_all = FALSE) |>
  ungroup()

# Check number of units, should be number of adm 3 units
verify(adm_3_phone_mobility_dat, length(unique(adm_3_origin)) == 330)
verify(adm_3_phone_mobility_dat, length(unique(adm_3_destination)) == 330)

# Admin level 2
# Aggregate data to admin level 2
adm_2_phone_mobility_dat <- phone_mobility_dat |>                                
  group_by(adm_2_origin, adm_2_destination, date) |>
  mutate(trips_sum = sum(trips_adj)) |>
  distinct(adm_2_origin, adm_2_destination, date, trips_sum, 
           adm_2_origin_code, adm_2_destination_code, 
           adm_1_origin, adm_1_destination, 
           adm_1_origin_code, adm_1_destination_code, .keep_all = FALSE) |>
  ungroup() |>
  # Calculate average daily trips
  group_by(adm_2_origin, adm_2_destination) |>
  mutate(trips_avg = mean(trips_sum, na.rm = TRUE)) |>
  distinct(adm_2_origin, adm_2_destination, trips_avg, 
           adm_2_origin_code, adm_2_destination_code, 
           adm_1_origin, adm_1_destination, 
           adm_1_origin_code, adm_1_destination_code, .keep_all = FALSE) |>
  ungroup()

# Check number of units, should be number of adm 2 units
verify(adm_2_phone_mobility_dat, length(unique(adm_2_origin)) == 25)
verify(adm_2_phone_mobility_dat, length(unique(adm_2_destination)) == 25)

# Admin level 1
# Aggregate data to admin level 1
adm_1_phone_mobility_dat <- phone_mobility_dat |>                                
  group_by(adm_1_origin, adm_1_destination, date) |>
  mutate(trips_sum = sum(trips_adj)) |>
  distinct(adm_1_origin, adm_1_destination, date, trips_sum, 
           adm_1_origin_code, adm_1_destination_code, .keep_all = FALSE) |>
  ungroup() |>
  # Calculate average daily trips
  group_by(adm_1_origin, adm_1_destination) |>
  mutate(trips_avg = mean(trips_sum, na.rm = TRUE)) |>
  distinct(adm_1_origin, adm_1_destination, trips_avg, 
           adm_1_origin_code, adm_1_destination_code, .keep_all = FALSE) |>
  ungroup()

# Check number of units, should be number of adm 2 units
verify(adm_1_phone_mobility_dat, length(unique(adm_1_origin)) == 9)
verify(adm_1_phone_mobility_dat, length(unique(adm_1_destination)) == 9)

# Save various administrative levels of mobile phone data
saveRDS(adm_3_phone_mobility_dat, './tmp/adm_3_phone_mobility_dat.rds')
saveRDS(adm_2_phone_mobility_dat, './tmp/adm_2_phone_mobility_dat.rds')
saveRDS(adm_1_phone_mobility_dat, './tmp/adm_1_phone_mobility_dat.rds')

################################################################################
################################################################################

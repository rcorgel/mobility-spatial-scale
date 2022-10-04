################################################################################
# File Name: 01_format_mobility_data                                           #
# Purpose: Load and format all data used in analyses.                          #
# Steps:                                                                       # 
#       1. Set-up script                                                       #
#       2. Load and format mobility data                                       #
#       3. Adjust for February/March mobility data issue                       #
#       4. Load and format population data                                     #
#                                                                              #
# Project: Sri Lanka Spatial Aggregation                                       #
# Author: Ronan Corgel                                                         #
# Date: September 2022                                                         #
################################################################################

####################
# 1. SET-UP SCRIPT #
####################

# Start with a clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(lubridate)
library(geosphere)
library(reshape)
library(raster)
library(sf)
library(sp)
library(readxl)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution')

####################################
# 2. LOAD AND FORMAT MOBILITY DATA #
####################################

# Load and format November data
nov_mobility_dat <- read_csv('./raw/export_for_JHU_November_2021_V2.csv')
nov_mobility_dat <- nov_mobility_dat %>%
  dplyr::rename(date = batch_date) %>%
  mutate(origin_long = as.numeric(str_extract(origin_area_centroid, "\\d+\\.\\d+")),
         origin_lat = as.numeric(str_extract(origin_area_centroid, "\\s(\\d+\\.\\d+)")),
         destination_long = as.numeric(str_extract(destination_area_centroid, "\\d+\\.\\d+")),
         destination_lat = as.numeric(str_extract(destination_area_centroid, "\\s\\d+\\.\\d+")),
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
  dplyr::rename('admin_level_1_origin' = 'province.x',
         'admin_level_1_destination' = 'province.y',
         'admin_level_3_origin' = 'origin_area',
         'admin_level_3_destination' = 'destination_area',
         'admin_level_3_origin_code' = 'origin_area_code',
         'admin_level_3_destination_code' = 'destination_area_code',
         'trips' = 'people') 

# Replace missing data with unknown
mobility_dat$admin_level_1_origin <- 
  ifelse(is.na(mobility_dat$admin_level_1_origin), 
         '[unknown]', mobility_dat$admin_level_1_origin)
mobility_dat$admin_level_1_destination <- 
  ifelse(is.na(mobility_dat$admin_level_1_destination), 
         '[unknown]', mobility_dat$admin_level_1_destination)

# Drop unknown destination and/or origin
mobility_dat <- mobility_dat %>% 
  filter(admin_level_1_origin != '[unknown]') %>% 
  filter(admin_level_1_destination != '[unknown]')

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
  group_by(month, admin_level_3_origin, admin_level_3_destination) %>%
  mutate(trips_month_avg = mean(trips, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(admin_level_3_origin, admin_level_3_destination) %>%
  mutate(trips_month_avg_jan = trips_month_avg[month == 1][1],
         trips_month_avg_feb = trips_month_avg[month == 2][1],
         trips_month_avg_mar = trips_month_avg[month == 3][1])

# Create new adjusted trips variable
mobility_dat <- mobility_dat %>% 
  mutate(trips_adj = ifelse(month == 2, 
                            trips * (trips_month_avg_jan/trips_month_avg_feb),
                            NA), # for February
         trips_adj = ifelse(month == 3, 
                            trips * (trips_month_avg_jan/trips_month_avg_mar),
                            trips_adj),
         trips_adj = ifelse(is.na(trips_adj), 
                            trips,
                            trips_adj),
         trips_adj = ifelse(trips_adj < trips, 
                            trips,
                            trips_adj))

# Re-save full mobility data
save(mobility_dat, file = './tmp/mobility_dat.RData')

###############################################################################
###############################################################################




mobility_trips_tot <- mobility_dat %>% group_by(date) %>%
  mutate(trips_stays_tot = sum(trips),
         trips_stays_adj_tot = sum(trips_adj)) %>%
  distinct(date, trips_stays_tot, trips_stays_adj_tot, month, .keep_all = FALSE)

# Number
ggplot(mobility_trips_tot, aes(x = date, y = trips_stays_adj_tot)) +
  geom_line(color="#00BE67", alpha = 0.5) + 
  geom_point(color="#00BE67", alpha = 0.5) + theme_minimal() + 
  ylab('Total Number of Trips and Stays') +
  xlab('Date') + scale_y_continuous(limits = c(0, 50000000))
ggsave('./figs/trips_over_time.png', plot = last_plot(), height = 6, width = 8)


mobility_trips_adm2_tot <- mobility_dat %>% 
  filter(admin_level_2_origin != admin_level_2_destination) %>%
  group_by(date, admin_level_2_origin) %>%
  mutate(outward_trips_tot = sum(trips_adj)) %>%
  distinct(date, outward_trips_tot, month, 
           admin_level_2_origin, admin_level_1_origin, .keep_all = FALSE)

ggplot(mobility_trips_adm2_tot) +
  geom_line(aes(x = date, 
                y = outward_trips_tot, 
                color = admin_level_2_origin), alpha = 0.5) + 
  geom_point(aes(x = date, 
                 y = outward_trips_tot, 
                 color = admin_level_2_origin), alpha = 0.5, size = 0.25) + theme_minimal() + 
  ylab('Total Number of Outward Trips') +
  xlab('Date') + scale_y_continuous(limits = c(0, 2200000)) + 
  facet_wrap(~admin_level_1_origin, ncol = 5)
ggsave('./figs/trips_over_time_adm2_adj.png', plot = last_plot(), height = 6, width = 16)






mobility_dat_test <- mobility_dat %>% filter(admin_level_1_origin == 'Northern')
mobility_dat_test <- mobility_dat_test %>% 
  group_by(month, admin_level_3_origin, admin_level_3_destination) %>%
  mutate(trips_month_avg = mean(trips, na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by(admin_level_3_origin, admin_level_3_destination) %>%
  mutate(trips_month_avg_jan = trips_month_avg[month == 1][1],
         trips_month_avg_feb = trips_month_avg[month == 2][1],
         trips_month_avg_mar = trips_month_avg[month == 3][1])


mobility_dat_test <- mobility_dat_test %>% 
  mutate(trips_adj = ifelse(month == 2, 
                            trips * (trips_month_avg_jan/trips_month_avg_feb),
                            NA),
         trips_adj = ifelse(month == 3, 
                            trips * (trips_month_avg_jan/trips_month_avg_mar),
                            trips_adj),
         trips_adj = ifelse(is.na(trips_adj), 
                            trips,
                            trips_adj),
         trips_adj = ifelse(trips_adj < trips, 
                            trips,
                            trips_adj))


mobility_dat_test$trips_adj <- 
  ifelse(mobility_dat_test$month == 2, 
         mobility_dat_test$trips * (mobility_dat_test$trips_month_avg_jan / mobility_dat_test$trips_month_avg_feb), NA)
mobility_dat_test$trips_adj <- 
  ifelse(mobility_dat_test$month == 3, 
         mobility_dat_test$trips * (mobility_dat_test$trips_month_avg_jan / mobility_dat_test$trips_month_avg_mar), mobility_dat_test$trips_adj)
mobility_dat_test$trips_adj <- 
  ifelse(is.na(mobility_dat_test$trips_adj), 
         mobility_dat_test$trips, mobility_dat_test$trips_adj)



mobility_dat_test_test <- mobility_dat_test %>% 
  filter(admin_level_3_origin == 'Nanaddan' &  admin_level_3_destination == 'Baddegama')



mobility_dat$trips_month_avg[mobility_dat$month == 1][1]
mobility_dat$trips_month_avg[mobility_dat$month == 2]
mobility_dat$trips_month_avg[mobility_dat$month == 3]



mobility_dat <- mobility_dat %>% 
  group_by(month) %>%
  mutate(month_people_avg = mean(people))

# Number
mobility_trips_adm2_tot <- mobility_dat %>% 
  group_by(date, admin_level_2_destination) %>%
  mutate(outward_trips_tot = sum(people)) %>%
  distinct(date, outward_trips_tot, month, 
           admin_level_2_origin, admin_level_1_origin, .keep_all = FALSE)
# Relative
mobility_trips_adm2_tot <- mobility_trips_adm2_tot %>%
  group_by(month, admin_level_2_destination) %>%
  mutate(month_avg = mean(outward_trips_tot))
mobility_trips_adm2_tot <- mobility_trips_adm2_tot %>%
  group_by(admin_level_2_destination) %>%
  mutate(rel_outward_trips_tot = outward_trips_tot / month_avg[1])  

ggplot(mobility_trips_adm2_tot) +
  geom_line(aes(x = date, 
                y = rel_outward_trips_tot, 
                color = admin_level_2_origin), alpha = 0.5) + 
  geom_point(aes(x = date, 
                 y = rel_outward_trips_tot, 
                 color = admin_level_2_origin), alpha = 0.5, size = 0.25) + theme_minimal() + 
  ylab('Relative Number of Outward Trips (Nov Avg)') +
  xlab('Date') + scale_y_continuous(limits = c(0, 2)) + 
  facet_wrap(~admin_level_1_origin, ncol = 5)

# Number
mobility_trips_adm3_tot <- mobility_dat %>% 
  group_by(date, admin_level_3_origin) %>%
  mutate(outward_trips_tot = sum(people)) %>%
  distinct(date, outward_trips_tot, month, 
           admin_level_3_origin, admin_level_2_origin, 
           admin_level_1_origin, .keep_all = FALSE)
# Relative
mobility_trips_adm3_tot <- mobility_trips_adm3_tot %>%
  group_by(month, admin_level_3_origin) %>%
  mutate(month_avg = mean(outward_trips_tot))

mobility_trips_adm3_tot <- mobility_trips_adm3_tot %>%
  group_by(admin_level_3_origin) %>%
  mutate(rel_outward_trips_tot = outward_trips_tot / month_avg[1])  

ggplot(mobility_trips_adm3_tot) +
  geom_line(aes(x = date, 
                y = rel_outward_trips_tot, 
                color = admin_level_3_origin), alpha = 0.5) + 
  geom_point(aes(x = date, 
                 y = rel_outward_trips_tot, 
                 color = admin_level_3_origin), alpha = 0.5, size = 0.25) + theme_minimal() + 
  ylab('Relative Number of Outward Trips (Nov Avg)') +
  xlab('Date') + scale_y_continuous(limits = c(0, 2)) + 
  facet_wrap(~admin_level_2_origin, ncol = 5) + 
  theme(legend.position = "none", panel.background = element_rect(fill = 'white', colour = 'white'))
ggsave('./figs/trips_over_time_adm3_rel.png', plot = last_plot(), height = 20, width = 28)

# Route
# Number
mobility_trips_route_tot <- mobility_dat %>% 
  group_by(date, admin_level_3_origin, admin_level_3_destination) %>%
  mutate(trips_tot = sum(trips)) %>%
  distinct(date, trips_tot, month, 
           admin_level_3_origin, admin_level_2_origin, 
           admin_level_3_destination, admin_level_2_destination, 
           .keep_all = FALSE)
# Relative
mobility_trips_route_tot <- mobility_trips_route_tot %>%
  group_by(month, admin_level_3_origin, admin_level_3_destination) %>%
  mutate(month_avg = mean(trips_tot))
mobility_trips_route_tot <- mobility_trips_route_tot %>%
  group_by(admin_level_3_origin, admin_level_3_destination) %>%
  mutate(rel_trips_tot = trips_tot / month_avg[1])  

ggplot(mobility_trips_route_tot) +
  geom_line(aes(x = date, 
                y = rel_trips_tot), alpha = 0.2) + 
  geom_smooth(aes(x = date, 
                  y = rel_trips_tot)) + 
  ylab('Relative Number of Trips (Nov Avg)') +
  xlab('Date') + scale_y_continuous(limits = c(0, 5)) + 
  theme(legend.position = "none", panel.background = element_rect(fill = 'white', colour = 'white'))


######################################
# 4. LOAD AND FORMAT POPULATION DATA #
######################################

# Load population count data
# Downloaded from: https://hub.worldpop.org/geodata/summary?id=49763
population_dat <- raster('./raw/lka_ppp_2020_constrained.tif')

# Convert raster to data frame
population_dat <- as.data.frame(rasterToPoints(population_dat))
population_dat <- population_dat %>% 
  dplyr::rename('lat' = 'y',
                'long' = 'x')

# Save data to load in ArcGIS
write.csv(population_dat, './tmp/population_dat.csv', row.names = FALSE)

# Load administrative 3 map data
# Downloaded from: https://data.humdata.org/dataset/cod-ab-lka
polygon_dat <- read_sf(dsn = './raw/lka_adm_20220816_shp_test/', 
                       layer = 'lka_admbnda_adm3_slsd_20220816')
polygon_dat <- as.data.frame(polygon_dat)

# Perform spatial join in ArcGIS
# After loading each data set in ArcGIS, performed a spatial join matching the 
# centroid of each raster pixel to the closest administrative 3 unit within 1 km. 

# Load joined spatial data
population_dat <- read_excel('./tmp/population_dat_join.xlsx')

# Format and rename the joined data
population_dat <- population_dat %>% 
  dplyr::select(c('dist', 'lat', 'long', 'lka_ppp_2020_constrained', 'ADM3_EN', 
                  'ADM2_EN', 'ADM1_EN')) %>%
  dplyr::rename('distance_match' = 'dist',
                'long' = 'lat', # renamed incorrectly earlier
                'lat' = 'long', # renamed incorrectly earlier
                'population_2020_pixel' = 'lka_ppp_2020_constrained',
                'admin_level_3' = 'ADM3_EN',
                'admin_level_2' = 'ADM2_EN',
                'admin_level_1' = 'ADM1_EN') 

# Aggregate to the admin 3 level
admin_3_population_dat <- population_dat %>% 
  dplyr::group_by(admin_level_3) %>%
  mutate(population_2020 = sum(population_2020_pixel)) %>%
  distinct(admin_level_3, admin_level_2, admin_level_1, population_2020, 
           .keep_all = FALSE)
  
# Load xwalk to link population admin 3 units to mobile phone admin 3 units
population_mobile_xwalk <- read.csv('./raw/population_mobile_xwalk.csv')

# Merge on the xwalk
admin_3_population_dat <- left_join(admin_3_population_dat, population_mobile_xwalk,
                                    by = c('admin_level_3' = 'admin_level_3_population'))

# Re-aggregate to the admin level 3 in the mobile phone data
# Aggregate to the admin 3 level
admin_3_population_dat <- admin_3_population_dat %>% 
  dplyr::group_by(admin_level_3_mobile) %>%
  mutate(population_2020_mobile = sum(population_2020)) %>%
  distinct(admin_level_3_mobile, admin_level_2, admin_level_1, population_2020_mobile, 
           .keep_all = FALSE)

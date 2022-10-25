################################################################################
# File Name: 01d_gravity_model_prep                                            #
#                                                                              #
# Purpose:   Prepare data for gravity model.                                   #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create mobility matrices                                       #
#            3. Create population vectors                                      #
#            4. Create distance matrices                                       #
#            5. Save data                                                      #
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
library(geosphere)
library(readxl)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution')

###############################
# 2. CREATE MOBILITY MATRICES #
###############################

# Load mobility data
load('./tmp/mobility_dat.RData')

# Administrative Unit 3
adm_3_day_avg <- mobility_dat %>%                                
  group_by(adm_3_origin, adm_3_destination) %>%
  mutate(adm_3_daily_avg = ceiling(mean(trips, na.rm = TRUE))) %>%
  mutate(adm_3_daily_var = var(trips, na.rm = TRUE)) %>%
  distinct(adm_3_origin, adm_3_destination, adm_3_daily_avg, adm_3_daily_var, .keep_all = FALSE)

# Create Matrix
adm_3_day_avg <- adm_3_day_avg[,c('adm_3_origin', 'adm_3_destination', 'adm_3_daily_avg')]
# Reshape to wide
adm_3_day_avg_mat <- reshape::cast(adm_3_day_avg, adm_3_origin ~ adm_3_destination)            
# Label rows with adm 3 names
rownames(adm_3_day_avg_mat) <- adm_3_day_avg_mat$adm_3_origin                         
# Get rid of the first column
adm_3_day_avg_mat <- adm_3_day_avg_mat[, -1]
# Convert to matrix
adm_3_day_avg_mat <- as.data.frame(adm_3_day_avg_mat)
adm_3_day_avg_mat <- as.matrix(adm_3_day_avg_mat)
names(dimnames(adm_3_day_avg_mat)) <- c('origin', 'destination')
# Replace NAs with 0
adm_3_day_avg_mat[is.na(adm_3_day_avg_mat)] <- 0

# Administrative Unit 2
adm_2_day_avg <- mobility_dat %>%                                
  group_by(adm_2_origin, adm_2_destination, date) %>%
  mutate(trips_sum = sum(trips)) %>%
  distinct(adm_2_origin, adm_2_destination, date, trips_sum, .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(adm_2_origin, adm_2_destination) %>%
  mutate(adm_2_daily_avg = ceiling(mean(trips_sum, na.rm = TRUE))) %>%
  mutate(adm_2_daily_var = var(trips_sum, na.rm = TRUE)) %>%
  distinct(adm_2_origin, adm_2_destination, adm_2_daily_avg, adm_2_daily_var, .keep_all = FALSE)

# Creat Matrix
adm_2_day_avg <- adm_2_day_avg[,c('adm_2_origin', 'adm_2_destination', 'adm_2_daily_avg')]
# Reshape to wide
adm_2_day_avg_mat <- reshape::cast(adm_2_day_avg, adm_2_origin ~ adm_2_destination)            
# Label rows with adm 3 names
rownames(adm_2_day_avg_mat) <- adm_2_day_avg_mat$adm_2_origin                         
# Get rid of the first column
adm_2_day_avg_mat <- adm_2_day_avg_mat[, -1]
# Convert to matrix
adm_2_day_avg_mat <- as.data.frame(adm_2_day_avg_mat)
adm_2_day_avg_mat <- as.matrix(adm_2_day_avg_mat)
names(dimnames(adm_2_day_avg_mat)) <- c('origin', 'destination')
# Replace NAs with 0
adm_2_day_avg_mat[is.na(adm_2_day_avg_mat)] <- 0

# Administrative Unit 1
adm_1_day_avg <- mobility_dat %>%                                
  group_by(adm_1_origin, adm_1_destination, date) %>%
  mutate(trips_sum = sum(trips)) %>%
  distinct(adm_1_origin, adm_1_destination, date, trips_sum, .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(adm_1_origin, adm_1_destination) %>%
  mutate(adm_1_daily_avg = ceiling(mean(trips_sum, na.rm = TRUE))) %>%
  mutate(adm_1_daily_var = var(trips_sum, na.rm = TRUE)) %>%
  distinct(adm_1_origin, adm_1_destination, adm_1_daily_avg, adm_1_daily_var, .keep_all = FALSE)

# Create Matrix
adm_1_day_avg <- adm_1_day_avg[,c('adm_1_origin', 'adm_1_destination', 'adm_1_daily_avg')]
# Reshape to wide
adm_1_day_avg_mat <- reshape::cast(adm_1_day_avg, adm_1_origin ~ adm_1_destination)            
# Label rows with adm 3 names
rownames(adm_1_day_avg_mat) <- adm_1_day_avg_mat$adm_1_origin                         
# Get rid of the first column
adm_1_day_avg_mat <- adm_1_day_avg_mat[, -1]
# Convert to matrix
adm_1_day_avg_mat <- as.data.frame(adm_1_day_avg_mat)
adm_1_day_avg_mat <- as.matrix(adm_1_day_avg_mat)
names(dimnames(adm_1_day_avg_mat)) <- c('origin', 'destination')
# Replace NAs with 0
adm_1_day_avg_mat[is.na(adm_1_day_avg_mat)] <- 0

################################
# 3. CREATE POPULATION VECTORS #
################################

# Load population data for all administrative units
load('./tmp/adm_3_population_dat.RData')
load('./tmp/adm_2_population_dat.RData')
load('./tmp/adm_1_population_dat.RData')

# Administrative Unit 3
adm_3_population_dat <- adm_3_population_dat %>%
  arrange(adm_3_mobility)
adm_3_pop_vec <- adm_3_population_dat$population_2020_adm_3

# Administrative Unit 2
adm_2_population_dat <- adm_2_population_dat %>%
  arrange(adm_2)
adm_2_pop_vec <- adm_2_population_dat$population_2020_adm_2

# Administrative Unit 1
adm_1_population_dat <- adm_1_population_dat %>%
  arrange(adm_1)
adm_1_pop_vec <- adm_1_population_dat$population_2020_adm_1

###############################
# 4. CREATE DISTANCE MATRICES #
###############################

# Administrative Unit 3
# Do not need to load centroid data because it is in the mobility data

# Get unique data for admin 3 origins
adm_3_origin <- mobility_dat %>% 
  group_by(adm_3_origin, origin_lat, origin_long) %>%
  distinct(adm_3_origin, origin_lat, origin_long, .keep_all = FALSE)
# Get unique data for admin 3 destinations
adm_3_destination <- mobility_dat %>% 
  group_by(adm_3_destination, destination_lat, destination_long) %>%
  distinct(adm_3_destination, destination_lat, destination_long, .keep_all = FALSE)

# Create all combinations or origin and destination
adm_3 <- as.data.frame(NULL)
for (i in adm_3_origin$adm_3_origin) {
  print(i)
  adm_3_origin$adm_3_destination <- i
  adm_3 <- rbind(adm_3, left_join(adm_3_origin, adm_3_destination, 
                                  by = c('adm_3_destination' = 'adm_3_destination')))
}

# Calculate distance (in km)
adm_3$distance <- distHaversine(as.matrix(adm_3[, c('origin_long', 'origin_lat')]), 
                                as.matrix(adm_3[, c('destination_long', 'destination_lat')])) * 0.001

# Make distance matrix
adm_3_dist <- adm_3[, c('adm_3_origin', 'adm_3_destination', 'distance')]
adm_3_dist_mat <- reshape::cast(adm_3_dist, adm_3_origin ~ adm_3_destination)            
# Label rows with district numbers
rownames(adm_3_dist_mat) <- adm_3_dist_mat$adm_3_origin                         
# Get rid of the first column
adm_3_dist_mat <- adm_3_dist_mat[ ,-1]
adm_3_dist_mat <- as.data.frame(adm_3_dist_mat)
adm_3_dist_mat <- as.matrix(adm_3_dist_mat)
names(dimnames(adm_3_dist_mat)) <- c('origin', 'destination')

# Administrative Unit 2
# Load centroids of each unit (calculated in ArcGIS)
adm_2_centroid <- read_excel('./tmp/adm_2_centroid.xlsx')

# Get unique data for admin 2 origins
adm_2_origin <- adm_2_centroid %>%
  dplyr::select(c('ADM2_EN', 'x_point', 'y_point')) %>%
  dplyr::rename('adm_2_origin' = 'ADM2_EN',
         'origin_lat' = 'y_point',
         'origin_long' = 'x_point')
# Get unique data for admin 2 destinations
adm_2_destination <- adm_2_centroid %>%
  dplyr::select(c('ADM2_EN', 'x_point', 'y_point')) %>%
  dplyr::rename('adm_2_destination' = 'ADM2_EN',
         'destination_lat' = 'y_point',
         'destination_long' = 'x_point')

# Create all combinations or origin and destination
adm_2 <- as.data.frame(NULL)
for (i in adm_2_origin$adm_2_origin) {
  print(i)
  adm_2_origin$adm_2_destination <- i
  adm_2 <- rbind(adm_2, left_join(adm_2_origin, adm_2_destination, 
                                  by = c('adm_2_destination' = 'adm_2_destination')))
}

# Calculate distance (in km)
adm_2$distance <- distHaversine(as.matrix(adm_2[, c('origin_long', 'origin_lat')]), 
                                as.matrix(adm_2[, c('destination_long', 'destination_lat')])) * 0.001

# Make distance matrix
adm_2_dist <- adm_2[, c('adm_2_origin', 'adm_2_destination', 'distance')]
adm_2_dist_mat <- reshape::cast(adm_2_dist, adm_2_origin ~ adm_2_destination)            
# Label rows with district numbers
rownames(adm_2_dist_mat) <- adm_2_dist_mat$adm_2_origin                         
# Get rid of the first column
adm_2_dist_mat <- adm_2_dist_mat[ ,-1]
adm_2_dist_mat <- as.data.frame(adm_2_dist_mat)
adm_2_dist_mat <- as.matrix(adm_2_dist_mat)
names(dimnames(adm_2_dist_mat)) <- c('origin', 'destination')

# Administrative Unit 1
# Load centroids of each unit (calculated in ArcGIS)
adm_1_centroid <- read_excel('./tmp/adm_1_centroid.xlsx')

# Get unique data for admin 1 origins
adm_1_origin <- adm_1_centroid %>%
  dplyr::select(c('ADM1_EN', 'x_point', 'y_point')) %>%
  dplyr::rename('adm_1_origin' = 'ADM1_EN',
                'origin_lat' = 'y_point',
                'origin_long' = 'x_point')
# Get unique data for admin 1 destinations
adm_1_destination <- adm_1_centroid %>%
  dplyr::select(c('ADM1_EN', 'x_point', 'y_point')) %>%
  dplyr::rename('adm_1_destination' = 'ADM1_EN',
                'destination_lat' = 'y_point',
                'destination_long' = 'x_point')

# Create all combinations or origin and destination
adm_1 <- as.data.frame(NULL)
for (i in adm_1_origin$adm_1_origin) {
  print(i)
  adm_1_origin$adm_1_destination <- i
  adm_1 <- rbind(adm_1, left_join(adm_1_origin, adm_1_destination, 
                                  by = c('adm_1_destination' = 'adm_1_destination')))
}

# Calculate distance (in km)
adm_1$distance <- distHaversine(as.matrix(adm_1[, c('origin_long', 'origin_lat')]), 
                                as.matrix(adm_1[, c('destination_long', 'destination_lat')])) * 0.001

# Make distance matrix
adm_1_dist <- adm_1[, c('adm_1_origin', 'adm_1_destination', 'distance')]
adm_1_dist_mat <- reshape::cast(adm_1_dist, adm_1_origin ~ adm_1_destination)            
# Label rows with district numbers
rownames(adm_1_dist_mat) <- adm_1_dist_mat$adm_1_origin                         
# Get rid of the first column
adm_1_dist_mat <- adm_1_dist_mat[ ,-1]
adm_1_dist_mat <- as.data.frame(adm_1_dist_mat)
adm_1_dist_mat <- as.matrix(adm_1_dist_mat)
names(dimnames(adm_1_dist_mat)) <- c('origin', 'destination')

################
# 5. SAVE DATA #
################

# Administrative Unit 3
save(list = c('adm_3_day_avg_mat', 'adm_3_dist_mat', 'adm_3_pop_vec'), 
              file = './tmp/adm_3_gravity_dat.RData')

# Administrative Unit 2
save(list = c('adm_2_day_avg_mat', 'adm_2_dist_mat', 'adm_2_pop_vec'), 
              file = './tmp/adm_2_gravity_dat.RData')

# Administrative Unit 1
save(list = c('adm_1_day_avg_mat', 'adm_1_dist_mat', 'adm_1_pop_vec'), 
              file = './tmp/adm_1_gravity_dat.RData')

################################################################################
################################################################################

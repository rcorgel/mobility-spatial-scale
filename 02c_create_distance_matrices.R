################################################################################
# File Name: 02c_create_distance_matrices                                      #
# Purpose:   Create distance matrices.                                         #                                                  
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Load coordinate data and create distance matrices                 #
#         3. Save data                                                         #
#                                                                              #
# Project: Sri Lanka Spatial Aggregation                                       #
# Author: Ronan Corgel                                                         #
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
library(assertr)
library(readxl)
library(geosphere)

# Set the seed
set.seed(12345)

# Set directory 
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project')

########################################################
# 2. LOAD COORDINATE DATA AND CREATE DISTANCE MATRICES #
########################################################

#########################
# ADMINISTRATIVE UNIT 3 #
#########################

# Do not need to load centroid data because it is in the mobility data
load('./tmp/phone_mobility_dat.RData')

# Get unique data for admin 3 origins
adm_3_origin <- phone_mobility_dat %>% 
  group_by(adm_3_origin, origin_lat, origin_long) %>%
  distinct(adm_3_origin, origin_lat, origin_long, .keep_all = FALSE)
# Get unique data for admin 3 destinations
adm_3_destination <- phone_mobility_dat %>% 
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

#########################
# ADMINISTRATIVE UNIT 2 #
#########################

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

#########################
# ADMINISTRATIVE UNIT 1 #
#########################

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
# 3. SAVE DATA #
################

save(list = c('adm_1_dist_mat', 'adm_2_dist_mat', 'adm_3_dist_mat', 
              'adm_1_dist', 'adm_2_dist', 'adm_3_dist'), 
     file = './tmp/adm_dist_mat.RData')

################################################################################
################################################################################

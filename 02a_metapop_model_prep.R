################################################################################
# File Name: 02a_metapop_model_prep                                            #
#                                                                              #
# Purpose:   Prepare data for the metapopulation model.                        #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create mobility matrices                                       #
#            3. Create population vectors                                      #
#            4. Create admin name vectors                                      #
#            5. Create admin w walk                                            #    
#            6. Save data                                                      #
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
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

###############################
# 2. CREATE MOBILITY MATRICES #
###############################

# Load mobility data
load('./tmp/mobility_dat.RData')

# Administrative Unit 3
adm_3_day_avg <- mobility_dat %>%                                
  group_by(adm_3_origin, adm_3_destination) %>%
  mutate(adm_3_daily_avg = mean(trips_adj, na.rm = TRUE)) %>%
  mutate(adm_3_daily_sd = sd(trips_adj, na.rm = TRUE)) %>%
  distinct(adm_3_origin, adm_3_destination, adm_3_daily_avg, adm_3_daily_sd, 
           .keep_all = FALSE) 

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
  mutate(trips_sum = sum(trips_adj)) %>%
  distinct(adm_2_origin, adm_2_destination, date, trips_sum, .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(adm_2_origin, adm_2_destination) %>%
  mutate(adm_2_daily_avg = mean(trips_sum, na.rm = TRUE)) %>%
  mutate(adm_2_daily_sd = sd(trips_sum, na.rm = TRUE)) %>%
  distinct(adm_2_origin, adm_2_destination, adm_2_daily_avg, adm_2_daily_sd, .keep_all = FALSE)

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
  mutate(trips_sum = sum(trips_adj)) %>%
  distinct(adm_1_origin, adm_1_destination, date, trips_sum, .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(adm_1_origin, adm_1_destination) %>%
  mutate(adm_1_daily_avg = ceiling(mean(trips_sum, na.rm = TRUE))) %>%
  mutate(adm_1_daily_sd = sd(trips_sum, na.rm = TRUE)) %>%
  distinct(adm_1_origin, adm_1_destination, adm_1_daily_avg, adm_1_daily_sd, .keep_all = FALSE)

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

################################
# 4. CREATE ADMIN NAME VECTORS #
################################

# Administrative Unit 3
adm_3_name_vec <- colnames(adm_3_day_avg_mat)

# Administrative Unit 2
adm_2_name_vec <- colnames(adm_2_day_avg_mat)

# Administrative Unit 1
adm_1_name_vec <- colnames(adm_1_day_avg_mat)

##########################
# 5. CREATE ADMIN X WALK #
##########################

# Administrative Unit 3
adm_3_x_walk <- mobility_dat %>% 
  group_by(adm_3_origin) %>%
  dplyr::distinct(adm_3_origin, .keep_all = TRUE) %>%
  dplyr::select(c('adm_3_origin', 'adm_2_origin', 'adm_1_origin')) %>%
  dplyr::rename('adm_3' = 'adm_3_origin',
                'adm_2' = 'adm_2_origin',
                'adm_1' = 'adm_1_origin')

# Administrative Unit 2
adm_2_x_walk <- mobility_dat %>% 
  group_by(adm_2_origin) %>%
  dplyr::distinct(adm_2_origin, .keep_all = TRUE) %>%
  dplyr::select(c('adm_2_origin', 'adm_1_origin')) %>%
  dplyr::rename('adm_2' = 'adm_2_origin',
                'adm_1' = 'adm_1_origin')

# Administrative Unit 1
# Do not need to do this for admin 1

################
# 6. SAVE DATA #
################

# Administrative Unit 3
save(list = c('adm_3_day_avg_mat', 'adm_3_name_vec', 'adm_3_pop_vec', 'adm_3_x_walk'), 
              file = './tmp/adm_3_metapop_dat.RData')

# Administrative Unit 2
save(list = c('adm_2_day_avg_mat', 'adm_2_name_vec', 'adm_2_pop_vec', 'adm_2_x_walk'), 
              file = './tmp/adm_2_metapop_dat.RData')

# Administrative Unit 1
save(list = c('adm_1_day_avg_mat', 'adm_1_name_vec', 'adm_1_pop_vec'), 
              file = './tmp/adm_1_metapop_dat.RData')

################################################################################
################################################################################

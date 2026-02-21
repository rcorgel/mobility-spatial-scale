################################################################################
# File Name: 02b_rescale_phone_mobility_data                                   #
# Purpose:   Rescale phone mobility data such that external travel at finer    #
#            scales is the same as external travel at aggregated scales.       #
#            This is in order to isolate the homogeneous mixing assumption.    #
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Load phone mobility data                                          #
#         3. Rescale phone mobility data                                       #
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

# Set the seed
set.seed(12345)

# Set directory 
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project')

###############################
# 2. LOAD PHONE MOBILITY DATA #
###############################

# Load formatted mobility data at all admin levels
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')
load('./tmp/fmt_adm_2_phone_mobility_dat.RData')
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')

# Create and load cross walks to match different admin levels
# Load admin crosswalk
admin_xwalk <- readRDS('./tmp/admin_xwalk.rds')

# Administrative Unit 3
adm_3_x_walk <- admin_xwalk |> 
  group_by(adm_3) |>
  dplyr::distinct(adm_3, .keep_all = TRUE) |>
  dplyr::select(c('adm_3', 'adm_2', 'adm_1')) 

# Administrative Unit 2
adm_2_x_walk <- admin_xwalk |> 
  group_by(adm_2) |>
  dplyr::distinct(adm_2, .keep_all = TRUE) |>
  dplyr::select(c('adm_2', 'adm_1')) 

############################
# 3. RESCALE MOBILITY DATA #
############################

############################
# ADMIN 3 MATCHING ADMIN 2 #
############################

# Replace NA values with 0
adm_3_phone_mobility_long$value <- ifelse(is.na(adm_3_phone_mobility_long$value), 
                                          0, adm_3_phone_mobility_long$value)

# Merge admin 2 to admin 3 
# Merge on admin 2 origins
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_3_x_walk,
                                       by = c('origin' = 'adm_3'))
# Merge on admin 2 destinations
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_3_x_walk,
                                       by = c('destination' = 'adm_3'))

# # Create variable to evaluate internal and external travel (Admin 3)
# adm_3_phone_mobility_long$type <- ifelse(adm_3_phone_mobility_long$adm_2.x == 
#                                            adm_3_phone_mobility_long$adm_2.y, 
#                                          'internal', 'external')
# Calculate internal and external travel for admin 3 at admin 2
adm_3_phone_mobility_long <- adm_3_phone_mobility_long |> group_by(origin, adm_2.y) |>
  mutate(type_sum = sum(value))

# # Create variable to evaluate internal and external travel (Admin 2)
# adm_2_phone_mobility_long$type <- ifelse(adm_2_phone_mobility_long$origin == 
#                                            adm_2_phone_mobility_long$destination, 
#                                          'internal', 'external')
# Calculate internal and external travel for admin 2
# adm_2_phone_mobility_long_merge <- adm_2_phone_mobility_long |> group_by(origin, type) |>
#   mutate(type_sum_adm_2 = sum(value)) |> distinct(origin, type, type_sum_adm_2, .keep_all = FALSE)

# Join admin 2 travel to admin 3 travel
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_2_phone_mobility_long,
                                       by = c('adm_2.x' = 'origin', 'adm_2.y' = 'destination'))

# Rescale admin 3 travel to match internal and external travel at admin 2
adm_3_phone_mobility_long$value_rescale <- adm_3_phone_mobility_long$value.x * 
  (adm_3_phone_mobility_long$value.y/adm_3_phone_mobility_long$type_sum)

# Check to make sure rescale went well
adm_3_phone_mobility_long <- adm_3_phone_mobility_long |> group_by(origin) |>
  mutate(check = sum(value_rescale)) |> ungroup() 

# Select variables
adm_3_phone_mobility_mat_rescale_adm_2_long <- adm_3_phone_mobility_long[, c('origin', 'destination', 'value_rescale')]

# Transform into a matrix
adm_3_phone_mobility_mat_rescale_adm_2 <- reshape::cast(
  adm_3_phone_mobility_long[, c('origin', 'destination', 'value_rescale')], 
  origin ~ destination)   
rownames(adm_3_phone_mobility_mat_rescale_adm_2) <- adm_3_phone_mobility_mat_rescale_adm_2$origin   
adm_3_phone_mobility_mat_rescale_adm_2 <- adm_3_phone_mobility_mat_rescale_adm_2[, -1]

############################
# ADMIN 3 MATCHING ADMIN 1 #
############################

# Reload data (overwrite data)
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')

# Replace NA values with 0
adm_3_phone_mobility_long$value <- ifelse(is.na(adm_3_phone_mobility_long$value), 
                                          0, adm_3_phone_mobility_long$value)

# Merge admin 1 to admin 3 
# Merge on admin 1 origins
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_3_x_walk,
                                       by = c('origin' = 'adm_3'))
# Merge on admin 1 destinations
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_3_x_walk,
                                       by = c('destination' = 'adm_3'))

# Create variable to evaluate internal and external travel (Admin 3)
# adm_3_phone_mobility_long$type <- ifelse(adm_3_phone_mobility_long$adm_1.x == 
#                                            adm_3_phone_mobility_long$adm_1.y, 
#                                          'internal', 'external')
# Calculate internal and external travel for admin 3 at admin 1
adm_3_phone_mobility_long <- adm_3_phone_mobility_long |> group_by(origin, adm_1.y) |>
  mutate(type_sum = sum(value))
# 
# # Create variable to evaluate internal and external travel (Admin 1)
# adm_1_phone_mobility_long$type <- ifelse(adm_1_phone_mobility_long$origin == 
#                                            adm_1_phone_mobility_long$destination, 
#                                          'internal', 'external')
# # Calculate internal and external travel for admin 2
# adm_1_phone_mobility_long_merge <- adm_1_phone_mobility_long |> group_by(origin, type) |>
#   mutate(type_sum_adm_1 = sum(value)) |> distinct(origin, type, type_sum_adm_1, .keep_all = FALSE)

# Join admin 2 travel to admin 3 travel
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_1_phone_mobility_long,
                                       by = c('adm_1.x' = 'origin', 'adm_1.y' = 'destination'))

# Rescale admin 3 travel to match internal and external travel at admin 2
adm_3_phone_mobility_long$value_rescale <- adm_3_phone_mobility_long$value.x * 
  (adm_3_phone_mobility_long$value.y/adm_3_phone_mobility_long$type_sum)

# Check to make sure rescale went well
adm_3_phone_mobility_long <- adm_3_phone_mobility_long |> group_by(origin) |>
  mutate(check = sum(value_rescale)) 

# Select variables
adm_3_phone_mobility_mat_rescale_adm_1_long <- adm_3_phone_mobility_long[, c('origin', 'destination', 'value_rescale')]

# Transform into a matrix
adm_3_phone_mobility_mat_rescale_adm_1 <- reshape::cast(
  adm_3_phone_mobility_long[, c('origin', 'destination', 'value_rescale')], 
  origin ~ destination)   
rownames(adm_3_phone_mobility_mat_rescale_adm_1) <- adm_3_phone_mobility_mat_rescale_adm_1$origin   
adm_3_phone_mobility_mat_rescale_adm_1 <- adm_3_phone_mobility_mat_rescale_adm_1[, -1]

############################
# ADMIN 2 MATCHING ADMIN 1 #
############################

# Reload data (overwrite data)
load('./tmp/fmt_adm_2_phone_mobility_dat.RData')
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')

# Merge admin 1 to admin 2 
# Merge on admin 1 origins
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, adm_2_x_walk,
                                        by = c('origin' = 'adm_2'))
# Merge on admin 1 destinations
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, adm_2_x_walk,
                                       by = c('destination' = 'adm_2'))

# # Create variable to evaluate internal and external travel (Admin 2)
# adm_2_phone_mobility_long$type <- ifelse(adm_2_phone_mobility_long$adm_1.x == 
#                                            adm_2_phone_mobility_long$adm_1.y, 
#                                          'internal', 'external')
# Calculate internal and external travel for admin 2 at admin 1
adm_2_phone_mobility_long <- adm_2_phone_mobility_long |> group_by(origin, adm_1.y) |>
  mutate(type_sum = sum(value))

# # Create variable to evaluate internal and external travel (Admin 1)
# adm_1_phone_mobility_long$type <- ifelse(adm_1_phone_mobility_long$origin == 
#                                            adm_1_phone_mobility_long$destination, 
#                                          'internal', 'external')
# # Calculate internal and external travel for admin 1
# adm_1_phone_mobility_long_merge <- adm_1_phone_mobility_long |> group_by(origin, type) |>
#   mutate(type_sum_adm_1 = sum(value)) |> distinct(origin, type, type_sum_adm_1, .keep_all = FALSE)

# Join admin 1 travel to admin 2 travel
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, adm_1_phone_mobility_long,
                                       by = c('adm_1.x' = 'origin', 'adm_1.y' = 'destination'))

# Rescale admin 2 travel to match internal and external travel at admin 1
adm_2_phone_mobility_long$value_rescale <- adm_2_phone_mobility_long$value.x * 
  (adm_2_phone_mobility_long$value.y/adm_2_phone_mobility_long$type_sum)

# Check to make sure rescale went well
adm_2_phone_mobility_long <- adm_2_phone_mobility_long |> group_by(origin) |>
  mutate(check = sum(value_rescale))

# Select variables
adm_2_phone_mobility_mat_rescale_long <- adm_2_phone_mobility_long[, c('origin', 'destination', 'value_rescale')]

# Transform into a matrix
adm_2_phone_mobility_mat_rescale <- reshape::cast(
  adm_2_phone_mobility_long[, c('origin', 'destination', 'value_rescale')], 
  origin ~ destination)   
rownames(adm_2_phone_mobility_mat_rescale) <- adm_2_phone_mobility_mat_rescale$origin   
adm_2_phone_mobility_mat_rescale <- adm_2_phone_mobility_mat_rescale[, -1]

#############
# SAVE DATA #
#############

save(list = c('adm_3_phone_mobility_mat_rescale_adm_2', 
              'adm_3_phone_mobility_mat_rescale_adm_1', 
              'adm_2_phone_mobility_mat_rescale',
              'adm_3_phone_mobility_mat_rescale_adm_2_long', 
              'adm_3_phone_mobility_mat_rescale_adm_1_long', 
              'adm_2_phone_mobility_mat_rescale_long'), 
     file = './tmp/rescale_phone_mobility_dat_2.RData')

################################################################################
################################################################################

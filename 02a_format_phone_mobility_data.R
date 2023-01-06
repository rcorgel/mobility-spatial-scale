################################################################################
# File Name: 02a_format_phone_mobility_data                                    #
# Purpose:   Format phone mobility data.                                       #                                                  
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Format phone mobility data                                        #
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
library(assertr)

# Set the seed
set.seed(12345)

# Set directory 
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

# Load format function
source('./spatial-resolution/02_format_functions.R')

#################################
# 2. FORMAT PHONE MOBILITY DATA #
#################################

# Load mobile phone data
load('./tmp/adm_phone_mobility_dat.RData')

##########################
# Administrative Level 3 #
##########################

# Rename/select name data
adm_3_phone_mobility_dat_name <- adm_3_phone_mobility_dat %>%
  dplyr::select(c('adm_3_origin', 'adm_3_destination', 'trips_avg')) %>%
  dplyr::rename('adm_origin' = 'adm_3_origin',
                'adm_destination' = 'adm_3_destination')
# Create name matrix and long data
adm_3_phone_mobility_mat <- format_mobility_data(data = adm_3_phone_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = TRUE)
adm_3_phone_mobility_long <- format_mobility_data(data = adm_3_phone_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = TRUE)

# Rename/select code data
adm_3_phone_mobility_dat_code <- adm_3_phone_mobility_dat %>%
  dplyr::select(c('adm_3_origin_code', 'adm_3_destination_code', 'trips_avg')) %>%
  dplyr::rename('adm_origin_code' = 'adm_3_origin_code',
                'adm_destination_code' = 'adm_3_destination_code')
# Create code matrix and long data
adm_3_phone_mobility_mat_code <- format_mobility_data(data = adm_3_phone_mobility_dat_code, 
                                                 method = 'code', output = 'matrix', na_replace = TRUE)
adm_3_phone_mobility_long_code <- format_mobility_data(data = adm_3_phone_mobility_dat_code, 
                                                  method = 'code', output = 'long', na_replace = TRUE)

# Save data
save(list = c('adm_3_phone_mobility_mat', 
              'adm_3_phone_mobility_long', 
              'adm_3_phone_mobility_mat_code', 
              'adm_3_phone_mobility_long_code'), 
     file = './tmp/fmt_adm_3_phone_mobility_dat.RData')

##########################
# Administrative Level 2 #
##########################

# Rename/select name data
adm_2_phone_mobility_dat_name <- adm_2_phone_mobility_dat %>%
  dplyr::select(c('adm_2_origin', 'adm_2_destination', 'trips_avg')) %>%
  dplyr::rename('adm_origin' = 'adm_2_origin',
                'adm_destination' = 'adm_2_destination')
# Create name matrix and long data
adm_2_phone_mobility_mat <- format_mobility_data(data = adm_2_phone_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = TRUE)
adm_2_phone_mobility_long <- format_mobility_data(data = adm_2_phone_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = TRUE)

# Rename/select code data
adm_2_phone_mobility_dat_code <- adm_2_phone_mobility_dat %>%
  dplyr::select(c('adm_2_origin_code', 'adm_2_destination_code', 'trips_avg')) %>%
  dplyr::rename('adm_origin_code' = 'adm_2_origin_code',
                'adm_destination_code' = 'adm_2_destination_code')
# Create code matrix and long data
adm_2_phone_mobility_mat_code <- format_mobility_data(data = adm_2_phone_mobility_dat_code, 
                                                      method = 'code', output = 'matrix', na_replace = TRUE)
adm_2_phone_mobility_long_code <- format_mobility_data(data = adm_2_phone_mobility_dat_code, 
                                                       method = 'code', output = 'long', na_replace = TRUE)

# Save data
save(list = c('adm_2_phone_mobility_mat', 
              'adm_2_phone_mobility_long', 
              'adm_2_phone_mobility_mat_code', 
              'adm_2_phone_mobility_long_code'), 
     file = './tmp/fmt_adm_2_phone_mobility_dat.RData')

##########################
# Administrative Level 1 #
##########################

# Rename/select name data
adm_1_phone_mobility_dat_name <- adm_1_phone_mobility_dat %>%
  dplyr::select(c('adm_1_origin', 'adm_1_destination', 'trips_avg')) %>%
  dplyr::rename('adm_origin' = 'adm_1_origin',
                'adm_destination' = 'adm_1_destination')
# Create name matrix and long data
adm_1_phone_mobility_mat <- format_mobility_data(data = adm_1_phone_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = TRUE)
adm_1_phone_mobility_long <- format_mobility_data(data = adm_1_phone_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = TRUE)

# Rename/select code data
adm_1_phone_mobility_dat_code <- adm_1_phone_mobility_dat %>%
  dplyr::select(c('adm_1_origin_code', 'adm_1_destination_code', 'trips_avg')) %>%
  dplyr::rename('adm_origin_code' = 'adm_1_origin_code',
                'adm_destination_code' = 'adm_1_destination_code')
# Create code matrix and long data
adm_1_phone_mobility_mat_code <- format_mobility_data(data = adm_1_phone_mobility_dat_code, 
                                                      method = 'code', output = 'matrix', na_replace = TRUE)
adm_1_phone_mobility_long_code <- format_mobility_data(data = adm_1_phone_mobility_dat_code, 
                                                       method = 'code', output = 'long', na_replace = TRUE)

# Save data
save(list = c('adm_1_phone_mobility_mat', 
              'adm_1_phone_mobility_long', 
              'adm_1_phone_mobility_mat_code', 
              'adm_1_phone_mobility_long_code'), 
     file = './tmp/fmt_adm_1_phone_mobility_dat.RData')

################################################################################
################################################################################






####################################
# 3. FORMAT FACEBOOK MOBILITY DATA #
####################################

# Load FB data
load('./tmp/adm_fb_mobility_dat.RData')

##################################
# 4. FORMAT SURVEY MOBILITY DATA #
##################################

# Load survey data
load('./tmp/adm_survey_mobility_dat.RData')

adm_3_phone_mobility_dat_rename <- adm_3_phone_mobility_dat %>%
  dplyr::select(c('adm_3_origin_code', 'adm_3_destination_code', 'trips_avg')) %>%
  dplyr::rename('adm_origin' = 'adm_3_origin',
                'adm_destination_code' = 'adm_3_destination_code')

adm_3_fb_mobility_dat_rename <- adm_3_fb_mobility_dat %>%
  dplyr::select(c('adm_3_origin', 'adm_3_destination', 'trips_avg')) %>%
  dplyr::rename('adm_origin' = 'adm_3_origin',
                'adm_destination' = 'adm_3_destination')

test <- format_mobility_data(data = adm_3_fb_mobility_dat_rename, 
                             method = 'name', output = 'long', na_replace = FALSE)
data = adm_3_phone_mobility_dat_rename
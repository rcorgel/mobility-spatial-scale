################################################################################
# File Name: 02b_format_fb_mobility_data                                       #
# Purpose:   Format Facebook mobility data.                                    #                                                  
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Format Facebook mobility data                                     #
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

####################################
# 2. FORMAT FACEBOOK MOBILITY DATA #
####################################

# Load fb mobility data
load('./tmp/adm_fb_mobility_dat.RData')

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
# Administrative Level 2
adm_2_fb_mobility_dat <- left_join(adm_2_fb_mobility_dat, unique(admin_xwalk[c('adm_2_code', 'adm_2')]),
                                   by = c('adm_2_origin' = 'adm_2'))
adm_2_fb_mobility_dat <- left_join(adm_2_fb_mobility_dat, unique(admin_xwalk[c('adm_2_code', 'adm_2')]),
                                   by = c('adm_2_destination' = 'adm_2'))
adm_2_fb_mobility_dat <- adm_2_fb_mobility_dat %>% 
  dplyr::rename('adm_2_origin_code' = 'adm_2_code.x',
                'adm_2_destination_code' = 'adm_2_code.y')
# Administrative Level 1
adm_1_fb_mobility_dat <- left_join(adm_1_fb_mobility_dat, unique(admin_xwalk[c('adm_1_code', 'adm_1')]),
                                   by = c('adm_1_origin' = 'adm_1'))
adm_1_fb_mobility_dat <- left_join(adm_1_fb_mobility_dat, unique(admin_xwalk[c('adm_1_code', 'adm_1')]),
                                   by = c('adm_1_destination' = 'adm_1'))
adm_1_fb_mobility_dat <- adm_1_fb_mobility_dat %>% 
  dplyr::rename('adm_1_origin_code' = 'adm_1_code.x',
                'adm_1_destination_code' = 'adm_1_code.y')

##########################
# Administrative Level 3 #
##########################

# Create all origin destination combinations (names and codes)
load('./tmp/adm_phone_mobility_dat.RData')
adm_origin <- unique(adm_3_phone_mobility_dat$adm_3_origin)
adm_origin <- as.data.frame(adm_origin)
adm_3_routes <- as.data.frame(NULL)
for (i in adm_origin$adm_origin) {
  adm_combos <- adm_origin
  adm_combos$adm_destination <- i
  adm_3_routes <- rbind(adm_3_routes, adm_combos)
}
adm_origin_code <- unique(adm_3_phone_mobility_dat$adm_3_origin_code)
adm_origin_code <- as.data.frame(adm_origin_code)
adm_3_routes_code <- as.data.frame(NULL)
for (i in adm_origin_code$adm_origin_code) {
  adm_combos <- adm_origin_code
  adm_combos$adm_destination_code <- i
  adm_3_routes_code <- rbind(adm_3_routes_code, adm_combos)
}

# Rename/select name data
adm_3_fb_mobility_dat_name <- adm_3_fb_mobility_dat %>%
  dplyr::select(c('adm_3_origin', 'adm_3_destination', 'trips_avg')) %>%
  dplyr::rename('adm_origin' = 'adm_3_origin',
                'adm_destination' = 'adm_3_destination')
# Merge on all origin destination combinations
adm_3_fb_mobility_dat_name <- full_join(adm_3_fb_mobility_dat_name, adm_3_routes,
                                        by = c('adm_origin', 'adm_destination'))
# Create name matrix and long data
adm_3_fb_mobility_mat <- format_mobility_data(data = adm_3_fb_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = FALSE)
adm_3_fb_mobility_long <- format_mobility_data(data = adm_3_fb_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = FALSE)

# Rename/select code data
adm_3_fb_mobility_dat_code <- adm_3_fb_mobility_dat %>% ungroup()
  dplyr::select(c('adm_3_origin_code', 'adm_3_destination_code', 'trips_avg')) %>%
  dplyr::rename('adm_origin_code' = 'adm_3_origin_code',
                'adm_destination_code' = 'adm_3_destination_code')
# Merge on all origin destination combinations
adm_3_fb_mobility_dat_code <- full_join(adm_3_fb_mobility_dat_code, adm_3_routes_code,
                                        by = c('adm_origin_code', 'adm_destination_code'))
# Create code matrix and long data
adm_3_fb_mobility_mat_code <- format_mobility_data(data = adm_3_fb_mobility_dat_code, 
                                                      method = 'code', output = 'matrix', na_replace = TRUE)
adm_3_fb_mobility_long_code <- format_mobility_data(data = adm_3_fb_mobility_dat_code, 
                                                       method = 'code', output = 'long', na_replace = TRUE)

# Save data
save(list = c('adm_3_fb_mobility_mat', 
              'adm_3_fb_mobility_long', 
              'adm_3_fb_mobility_mat_code', 
              'adm_3_fb_mobility_long_code'), 
     file = './tmp/fmt_adm_3_fb_mobility_dat.RData')

##########################
# Administrative Level 2 #
##########################

# Rename/select name data
adm_2_fb_mobility_dat_name <- adm_2_fb_mobility_dat %>%
  dplyr::select(c('adm_2_origin', 'adm_2_destination', 'trips_avg')) %>%
  dplyr::rename('adm_origin' = 'adm_2_origin',
                'adm_destination' = 'adm_2_destination')
# Create name matrix and long data
adm_2_fb_mobility_mat <- format_mobility_data(data = adm_2_fb_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = TRUE)
adm_2_fb_mobility_long <- format_mobility_data(data = adm_2_fb_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = TRUE)

# Rename/select code data
adm_2_fb_mobility_dat_code <- adm_2_fb_mobility_dat %>%
  dplyr::select(c('adm_2_origin_code', 'adm_2_destination_code', 'trips_avg')) %>%
  dplyr::rename('adm_origin_code' = 'adm_2_origin_code',
                'adm_destination_code' = 'adm_2_destination_code')
# Create code matrix and long data
adm_2_fb_mobility_mat_code <- format_mobility_data(data = adm_2_fb_mobility_dat_code, 
                                                      method = 'code', output = 'matrix', na_replace = TRUE)
adm_2_fb_mobility_long_code <- format_mobility_data(data = adm_2_fb_mobility_dat_code, 
                                                       method = 'code', output = 'long', na_replace = TRUE)

# Save data
save(list = c('adm_2_fb_mobility_mat', 
              'adm_2_fb_mobility_long', 
              'adm_2_fb_mobility_mat_code', 
              'adm_2_fb_mobility_long_code'), 
     file = './tmp/fmt_adm_2_fb_mobility_dat.RData')

##########################
# Administrative Level 1 #
##########################

# Rename/select name data
adm_1_fb_mobility_dat_name <- adm_1_fb_mobility_dat %>%
  dplyr::select(c('adm_1_origin', 'adm_1_destination', 'trips_avg')) %>%
  dplyr::rename('adm_origin' = 'adm_1_origin',
                'adm_destination' = 'adm_1_destination')
# Create name matrix and long data
adm_1_fb_mobility_mat <- format_mobility_data(data = adm_1_fb_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = TRUE)
adm_1_fb_mobility_long <- format_mobility_data(data = adm_1_fb_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = TRUE)

# Rename/select code data
adm_1_fb_mobility_dat_code <- adm_1_fb_mobility_dat %>%
  dplyr::select(c('adm_1_origin_code', 'adm_1_destination_code', 'trips_avg')) %>%
  dplyr::rename('adm_origin_code' = 'adm_1_origin_code',
                'adm_destination_code' = 'adm_1_destination_code')
# Create code matrix and long data
adm_1_fb_mobility_mat_code <- format_mobility_data(data = adm_1_fb_mobility_dat_code, 
                                                      method = 'code', output = 'matrix', na_replace = TRUE)
adm_1_fb_mobility_long_code <- format_mobility_data(data = adm_1_fb_mobility_dat_code, 
                                                       method = 'code', output = 'long', na_replace = TRUE)

# Save data
save(list = c('adm_1_fb_mobility_mat', 
              'adm_1_fb_mobility_long', 
              'adm_1_fb_mobility_mat_code', 
              'adm_1_fb_mobility_long_code'), 
     file = './tmp/fmt_adm_1_fb_mobility_dat.RData')

################################################################################
################################################################################


################################################################################
# File Name: 02a_format_phone_mobility_data                                    #
# Purpose:   Format phone mobility data.                                       #                                                  
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Format phone mobility data                                        #
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

# Load format functions
source('./mobility-spatial-scale/02_format_functions.R')

#################################
# 2. FORMAT PHONE MOBILITY DATA #
#################################

# Load mobile phone data
adm_3_phone_mobility_dat <- readRDS('./out/adm_3_phone_mobility_dat.rds')
adm_2_phone_mobility_dat <- readRDS('./out/adm_2_phone_mobility_dat.rds')
adm_1_phone_mobility_dat <- readRDS('./out/adm_1_phone_mobility_dat.rds')

##########################
# Administrative Level 3 #
##########################

# Rename/select name data
adm_3_phone_mobility_dat_name <- adm_3_phone_mobility_dat |>
  dplyr::select(c('adm_3_origin', 'adm_3_destination', 'trips_avg')) |>
  dplyr::rename('adm_origin' = 'adm_3_origin',
                'adm_destination' = 'adm_3_destination')
# Create name matrix and long data
adm_3_phone_mobility_mat <- format_mobility_data(data = adm_3_phone_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = FALSE)
adm_3_phone_mobility_long <- format_mobility_data(data = adm_3_phone_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = FALSE)

# Rename/select code data
adm_3_phone_mobility_dat_code <- adm_3_phone_mobility_dat |>
  dplyr::select(c('adm_3_origin_code', 'adm_3_destination_code', 'trips_avg')) |>
  dplyr::rename('adm_origin_code' = 'adm_3_origin_code',
                'adm_destination_code' = 'adm_3_destination_code')
# Create code matrix and long data
adm_3_phone_mobility_mat_code <- format_mobility_data(data = adm_3_phone_mobility_dat_code, 
                                                 method = 'code', output = 'matrix', na_replace = FALSE)
adm_3_phone_mobility_long_code <- format_mobility_data(data = adm_3_phone_mobility_dat_code, 
                                                  method = 'code', output = 'long', na_replace = FALSE)

# Save data
saveRDS(adm_3_phone_mobility_mat, './out/adm_3_phone_mobility_mat.rds')
saveRDS(adm_3_phone_mobility_long, './out/adm_3_phone_mobility_long.rds')
saveRDS(adm_3_phone_mobility_mat_code, './out/adm_3_phone_mobility_mat_code.rds')
saveRDS(adm_3_phone_mobility_long_code, './out/adm_3_phone_mobility_long_code.rds')

##########################
# Administrative Level 2 #
##########################

# Rename/select name data
adm_2_phone_mobility_dat_name <- adm_2_phone_mobility_dat |>
  dplyr::select(c('adm_2_origin', 'adm_2_destination', 'trips_avg')) |>
  dplyr::rename('adm_origin' = 'adm_2_origin',
                'adm_destination' = 'adm_2_destination')
# Create name matrix and long data
adm_2_phone_mobility_mat <- format_mobility_data(data = adm_2_phone_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = FALSE)
adm_2_phone_mobility_long <- format_mobility_data(data = adm_2_phone_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = FALSE)

# Rename/select code data
adm_2_phone_mobility_dat_code <- adm_2_phone_mobility_dat |>
  dplyr::select(c('adm_2_origin_code', 'adm_2_destination_code', 'trips_avg')) |>
  dplyr::rename('adm_origin_code' = 'adm_2_origin_code',
                'adm_destination_code' = 'adm_2_destination_code')
# Create code matrix and long data
adm_2_phone_mobility_mat_code <- format_mobility_data(data = adm_2_phone_mobility_dat_code, 
                                                      method = 'code', output = 'matrix', na_replace = FALSE)
adm_2_phone_mobility_long_code <- format_mobility_data(data = adm_2_phone_mobility_dat_code, 
                                                       method = 'code', output = 'long', na_replace = FALSE)

# Save data
saveRDS(adm_2_phone_mobility_mat, './out/adm_2_phone_mobility_mat.rds')
saveRDS(adm_2_phone_mobility_long, './out/adm_2_phone_mobility_long.rds')
saveRDS(adm_2_phone_mobility_mat_code, './out/adm_2_phone_mobility_mat_code.rds')
saveRDS(adm_2_phone_mobility_long_code, './out/adm_2_phone_mobility_long_code.rds')

##########################
# Administrative Level 1 #
##########################

# Rename/select name data
adm_1_phone_mobility_dat_name <- adm_1_phone_mobility_dat |>
  dplyr::select(c('adm_1_origin', 'adm_1_destination', 'trips_avg')) |>
  dplyr::rename('adm_origin' = 'adm_1_origin',
                'adm_destination' = 'adm_1_destination')
# Create name matrix and long data
adm_1_phone_mobility_mat <- format_mobility_data(data = adm_1_phone_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = FALSE)
adm_1_phone_mobility_long <- format_mobility_data(data = adm_1_phone_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = FALSE)

# Rename/select code data
adm_1_phone_mobility_dat_code <- adm_1_phone_mobility_dat |>
  dplyr::select(c('adm_1_origin_code', 'adm_1_destination_code', 'trips_avg')) |>
  dplyr::rename('adm_origin_code' = 'adm_1_origin_code',
                'adm_destination_code' = 'adm_1_destination_code')
# Create code matrix and long data
adm_1_phone_mobility_mat_code <- format_mobility_data(data = adm_1_phone_mobility_dat_code, 
                                                      method = 'code', output = 'matrix', na_replace = FALSE)
adm_1_phone_mobility_long_code <- format_mobility_data(data = adm_1_phone_mobility_dat_code, 
                                                       method = 'code', output = 'long', na_replace = FALSE)

# Save data
saveRDS(adm_1_phone_mobility_mat, './out/adm_1_phone_mobility_mat.rds')
saveRDS(adm_1_phone_mobility_long, './out/adm_1_phone_mobility_long.rds')
saveRDS(adm_1_phone_mobility_mat_code, './out/adm_1_phone_mobility_mat_code.rds')
saveRDS(adm_1_phone_mobility_long_code, './out/adm_1_phone_mobility_long_code.rds')

################################################################################
################################################################################

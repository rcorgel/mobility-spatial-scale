################################################################################
# File Name: 03a_format_simulated_mobility_data                                #
# Purpose:   Format simulated mobility data.                                   #                                                  
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Format simulated mobility data                                    #
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

#####################################
# 2. FORMAT SIMULATED MOBILITY DATA #
#####################################

# Load simulated trip count data
phone_mobility_adm_3_pred <- readRDS('./mobility-spatial-scale/simulated data/adm_3_predictions_count.rds')
phone_mobility_adm_3_pred_prop <- readRDS('./mobility-spatial-scale/simulated data/adm_3_predictions_proportion.rds')
phone_mobility_adm_2_pred <- readRDS('./mobility-spatial-scale/simulated data/adm_2_predictions_count.rds')
phone_mobility_adm_2_pred_prop <- readRDS('./mobility-spatial-scale/simulated data/adm_2_predictions_proportion.rds')
phone_mobility_adm_1_pred <- readRDS('./mobility-spatial-scale/simulated data/adm_1_predictions_count.rds')
phone_mobility_adm_1_pred_prop <- readRDS('./mobility-spatial-scale/simulated data/adm_1_predictions_proportion.rds')

# Reshape the simulated matrices from wide to long
# Administrative Level 1
adm_1_long_pred <- reshape2::melt(phone_mobility_adm_1_pred, 
                                  id.vars = 'origin_area', 
                                  variable.name = 'destination_area')
adm_1_sim_mobility_dat <- adm_1_long_pred |>
  dplyr::rename('adm_1_origin' = 'Var1',
                'adm_1_destination' = 'Var2')

# Administrative Level 2
adm_2_long_pred <- reshape2::melt(phone_mobility_adm_2_pred, 
                                  id.vars = 'origin_area', 
                                  variable.name = 'destination_area')
adm_2_sim_mobility_dat <- adm_2_long_pred |>
  dplyr::rename('adm_2_origin' = 'Var1',
                'adm_2_destination' = 'Var2')

# Administrative Level 3
adm_3_long_pred <- reshape2::melt(phone_mobility_adm_3_pred, 
                                  id.vars = 'origin_area', 
                                  variable.name = 'destination_area')
adm_3_sim_mobility_dat <- adm_3_long_pred |>
  dplyr::rename('adm_3_origin' = 'Var1',
                'adm_3_destination' = 'Var2')

# Save long count data
saveRDS(adm_3_sim_mobility_dat, './out/adm_3_sim_mobility_dat.rds')
saveRDS(adm_2_sim_mobility_dat, './out/adm_2_sim_mobility_dat.rds')
saveRDS(adm_1_sim_mobility_dat, './out/adm_1_sim_mobility_dat.rds')

##########################
# Administrative Level 3 #
##########################

# Load admin crosswalk with codes
admin_xwalk <- read.csv('./tmp/admin_xwalk.csv')

# Rename/select name data
adm_3_sim_mobility_dat_name <- adm_3_sim_mobility_dat |>
  dplyr::select(c('adm_3_origin', 'adm_3_destination', 'value')) |>
  dplyr::rename('adm_origin' = 'adm_3_origin',
                'adm_destination' = 'adm_3_destination')
# Create name matrix and long data
adm_3_sim_mobility_mat <- format_mobility_data(data = adm_3_sim_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = FALSE)
adm_3_sim_mobility_long <- format_mobility_data(data = adm_3_sim_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = FALSE)

# Join on codes
adm_3_sim_mobility_dat <- left_join(adm_3_sim_mobility_dat, admin_xwalk[, c(1, 4)],
                                          by = c('adm_3_origin' = 'adm_3'))
adm_3_sim_mobility_dat <- left_join(adm_3_sim_mobility_dat, admin_xwalk[, c(1, 4)],
                                          by = c('adm_3_destination' = 'adm_3'))
# Rename codes
adm_3_sim_mobility_dat <- adm_3_sim_mobility_dat |>
  dplyr::rename('adm_3_origin_code' = 'adm_3_code.x',
                'adm_3_destination_code' = 'adm_3_code.y')

# Rename/select code data
adm_3_sim_mobility_dat_code <- adm_3_sim_mobility_dat |>
  dplyr::select(c('adm_3_origin_code', 'adm_3_destination_code', 'value')) |>
  dplyr::rename('adm_origin_code' = 'adm_3_origin_code',
                'adm_destination_code' = 'adm_3_destination_code')

# Create code matrix and long data
adm_3_sim_mobility_mat_code <- format_mobility_data(data = adm_3_sim_mobility_dat_code, 
                                                      method = 'code', output = 'matrix', na_replace = FALSE)
adm_3_sim_mobility_long_code <- format_mobility_data(data = adm_3_sim_mobility_dat_code, 
                                                       method = 'code', output = 'long', na_replace = FALSE)

# Save data
saveRDS(adm_3_sim_mobility_mat, './out/adm_3_sim_mobility_mat.rds')
saveRDS(adm_3_sim_mobility_long, './out/adm_3_sim_mobility_long.rds')
saveRDS(adm_3_sim_mobility_mat_code, './out/adm_3_sim_mobility_mat_code.rds')
saveRDS(adm_3_sim_mobility_long_code, './out/adm_3_sim_mobility_long_code.rds')

##########################
# Administrative Level 2 #
##########################

# Subset x walk to admin 2
admin_xwalk_2 <- admin_xwalk |> group_by(adm_2) |>
  distinct(adm_2, adm_2_code)

# Rename/select name data
adm_2_sim_mobility_dat_name <- adm_2_sim_mobility_dat |>
  dplyr::select(c('adm_2_origin', 'adm_2_destination', 'value')) |>
  dplyr::rename('adm_origin' = 'adm_2_origin',
                'adm_destination' = 'adm_2_destination')
# Create name matrix and long data
adm_2_sim_mobility_mat <- format_mobility_data(data = adm_2_sim_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = FALSE)
adm_2_sim_mobility_long <- format_mobility_data(data = adm_2_sim_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = FALSE)

# Join on codes
adm_2_sim_mobility_dat <- left_join(adm_2_sim_mobility_dat, admin_xwalk_2,
                                          by = c('adm_2_origin' = 'adm_2'))
adm_2_sim_mobility_dat <- left_join(adm_2_sim_mobility_dat, admin_xwalk_2,
                                          by = c('adm_2_destination' = 'adm_2'))
# Rename codes
adm_2_sim_mobility_dat <- adm_2_sim_mobility_dat |>
  dplyr::rename('adm_2_origin_code' = 'adm_2_code.x',
                'adm_2_destination_code' = 'adm_2_code.y')

# Rename/select code data
adm_2_sim_mobility_dat_code <- adm_2_sim_mobility_dat |>
  dplyr::select(c('adm_2_origin_code', 'adm_2_destination_code', 'value')) |>
  dplyr::rename('adm_origin_code' = 'adm_2_origin_code',
                'adm_destination_code' = 'adm_2_destination_code')

# Create code matrix and long data
adm_2_sim_mobility_mat_code <- format_mobility_data(data = adm_2_sim_mobility_dat_code, 
                                                      method = 'code', output = 'matrix', na_replace = FALSE)
adm_2_sim_mobility_long_code <- format_mobility_data(data = adm_2_sim_mobility_dat_code, 
                                                       method = 'code', output = 'long', na_replace = FALSE)

# Save data
saveRDS(adm_2_sim_mobility_mat, './out/adm_2_sim_mobility_mat.rds')
saveRDS(adm_2_sim_mobility_long, './out/adm_2_sim_mobility_long.rds')
saveRDS(adm_2_sim_mobility_mat_code, './out/adm_2_sim_mobility_mat_code.rds')
saveRDS(adm_2_sim_mobility_long_code, './out/adm_2_sim_mobility_long_code.rds')

##########################
# Administrative Level 1 #
##########################

# Subset x walk to admin 1
admin_xwalk_1 <- admin_xwalk |> group_by(adm_1) |>
  distinct(adm_1, adm_1_code)

# Rename/select name data
adm_1_sim_mobility_dat_name <- adm_1_sim_mobility_dat |>
  dplyr::select(c('adm_1_origin', 'adm_1_destination', 'value')) |>
  dplyr::rename('adm_origin' = 'adm_1_origin',
                'adm_destination' = 'adm_1_destination')
# Create name matrix and long data
adm_1_sim_mobility_mat <- format_mobility_data(data = adm_1_sim_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = FALSE)
adm_1_sim_mobility_long <- format_mobility_data(data = adm_1_sim_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = FALSE)

# Join on codes
adm_1_sim_mobility_dat <- left_join(adm_1_sim_mobility_dat, admin_xwalk_1,
                                          by = c('adm_1_origin' = 'adm_1'))
adm_1_sim_mobility_dat <- left_join(adm_1_sim_mobility_dat, admin_xwalk_1,
                                          by = c('adm_1_destination' = 'adm_1'))
# Rename codes
adm_1_sim_mobility_dat <- adm_1_sim_mobility_dat |>
  dplyr::rename('adm_1_origin_code' = 'adm_1_code.x',
                'adm_1_destination_code' = 'adm_1_code.y')

# Rename/select code data
adm_1_sim_mobility_dat_code <- adm_1_sim_mobility_dat |>
  dplyr::select(c('adm_1_origin_code', 'adm_1_destination_code', 'value')) |>
  dplyr::rename('adm_origin_code' = 'adm_1_origin_code',
                'adm_destination_code' = 'adm_1_destination_code')

# Create code matrix and long data
adm_1_sim_mobility_mat_code <- format_mobility_data(data = adm_1_sim_mobility_dat_code, 
                                                      method = 'code', output = 'matrix', na_replace = FALSE)
adm_1_sim_mobility_long_code <- format_mobility_data(data = adm_1_sim_mobility_dat_code, 
                                                       method = 'code', output = 'long', na_replace = FALSE)

# Save data
saveRDS(adm_1_sim_mobility_mat, './out/adm_1_sim_mobility_mat.rds')
saveRDS(adm_1_sim_mobility_long, './out/adm_1_sim_mobility_long.rds')
saveRDS(adm_1_sim_mobility_mat_code, './out/adm_1_sim_mobility_mat_code.rds')
saveRDS(adm_1_sim_mobility_long_code, './out/adm_1_sim_mobility_long_code.rds')

################################################################################
################################################################################

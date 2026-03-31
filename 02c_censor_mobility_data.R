################################################################################
# File Name: 02c_censor_mobile_phone_data                                      #
#                                                                              #
# Purpose:   Censor mobile phone data for data sharing.                        #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Censor data and save                                           #
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
library(mobility)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

###########################
# 2. CENSOR DATA AND SAVE #
###########################

# Load phone mobility formatted data 
adm_3_phone_mobility_dat <- readRDS('./out/adm_3_phone_mobility_dat.rds')
adm_2_phone_mobility_dat <- readRDS('./out/adm_2_phone_mobility_dat.rds')
adm_1_phone_mobility_dat <- readRDS('./out/adm_1_phone_mobility_dat.rds')

# Censor values that are < 10
adm_3_phone_mobility_dat$trips_avg_cen <- ifelse(adm_3_phone_mobility_dat$trips_avg < 10, NA, 
                                                 adm_3_phone_mobility_dat$trips_avg)
adm_2_phone_mobility_dat$trips_avg_cen <- ifelse(adm_2_phone_mobility_dat$trips_avg < 10, NA, 
                                                 adm_2_phone_mobility_dat$trips_avg)
adm_1_phone_mobility_dat$trips_avg_cen <- ifelse(adm_1_phone_mobility_dat$trips_avg < 10, NA, 
                                                 adm_1_phone_mobility_dat$trips_avg)

# Round remaining trips to the nearest 10
adm_3_phone_mobility_dat$trips_avg_cen <- round(adm_3_phone_mobility_dat$trips_avg_cen, digits = -1)
adm_2_phone_mobility_dat$trips_avg_cen <- round(adm_2_phone_mobility_dat$trips_avg_cen, digits = -1)
adm_1_phone_mobility_dat$trips_avg_cen <- round(adm_1_phone_mobility_dat$trips_avg_cen, digits = -1)

# Keep relevant variables
adm_3_phone_mobility_cen <- adm_3_phone_mobility_dat |>
  dplyr::select(c(adm_3_origin, adm_3_destination, trips_avg_cen)) |>
  dplyr::rename('trips' = 'trips_avg_cen')
adm_2_phone_mobility_cen <- adm_2_phone_mobility_dat |>
  dplyr::select(c(adm_2_origin, adm_2_destination, trips_avg_cen)) |>
  dplyr::rename('trips' = 'trips_avg_cen')
adm_1_phone_mobility_cen <- adm_1_phone_mobility_dat |>
  dplyr::select(c(adm_1_origin, adm_1_destination, trips_avg_cen)) |>
  dplyr::rename('trips' = 'trips_avg_cen')

# Convert to matrix
adm_3_mat <- reshape::cast(adm_3_phone_mobility_cen, adm_3_origin ~ adm_3_destination)    
# Label rows with adm names
rownames(adm_3_mat) <- adm_3_mat$adm_3_origin   
# Get rid of the first column
adm_3_mat <- adm_3_mat[, -1]
# Convert to matrix
adm_3_mat <- as.data.frame(adm_3_mat)
adm_3_mat <- as.matrix(adm_3_mat)
names(dimnames(adm_3_mat)) <- c('origin', 'destination')

adm_2_mat <- reshape::cast(adm_2_phone_mobility_cen, adm_2_origin ~ adm_2_destination)    
# Label rows with adm names
rownames(adm_2_mat) <- adm_2_mat$adm_2_origin   
# Get rid of the first column
adm_2_mat <- adm_2_mat[, -1]
# Convert to matrix
adm_2_mat <- as.data.frame(adm_2_mat)
adm_2_mat <- as.matrix(adm_2_mat)
names(dimnames(adm_2_mat)) <- c('origin', 'destination')

adm_1_mat <- reshape::cast(adm_1_phone_mobility_cen, adm_1_origin ~ adm_1_destination)    
# Label rows with adm names
rownames(adm_1_mat) <- adm_1_mat$adm_1_origin   
# Get rid of the first column
adm_1_mat <- adm_1_mat[, -1]
# Convert to matrix
adm_1_mat <- as.data.frame(adm_1_mat)
adm_1_mat <- as.matrix(adm_1_mat)
names(dimnames(adm_1_mat)) <- c('origin', 'destination')

# Save
saveRDS(adm_3_mat, './mobility-spatial-scale/observed data/adm_3_observed_count.rds')
saveRDS(adm_2_mat, './mobility-spatial-scale/observed data/adm_2_observed_count.rds')
saveRDS(adm_1_mat, './mobility-spatial-scale/observed data/adm_1_observed_count.rds')

################################################################################
################################################################################

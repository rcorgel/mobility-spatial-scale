################################################################################
# File Name: 03_simulate_mobile_phone_data                                     #
#                                                                              #
# Purpose:   Simulate mobile phone data OD matrices using mobility models      #
#            from observed data.                                               #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Simulate mobile phone data                                     #
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

# Load format functions
source('./mobility-spatial-scale/02_format_functions.R')

#################################
# 2. SIMULATE MOBILE PHONE DATA #
#################################

# Distance data
load('./tmp/adm_dist_mat.RData')

# Population data
adm_1_population_dat <- readRDS('./out/adm_1_population_dat.rds')
adm_2_population_dat <- readRDS('./out/adm_2_population_dat.rds')
adm_3_population_dat <- readRDS('./out/adm_3_population_dat.rds')

# Load mobile phone data to create all origin/destination combinations
phone_mobility_dat <- readRDS('./tmp/phone_mobility_dat.rds')

# Load phone mobility formatted data 
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
# Merge on all origin destination combinations
adm_3_combos_name <- all_admin_combos(data = phone_mobility_dat, 
                                      level = '3', method = 'name')
adm_3_phone_mobility_dat_name <- full_join(adm_3_phone_mobility_dat_name, adm_3_combos_name,
                                        by = c('adm_origin' = 'origin', 
                                               'adm_destination' = 'destination'))

# Replace NA values with 0
adm_3_phone_mobility_dat_name$trips_avg <- ifelse(is.na(adm_3_phone_mobility_dat_name$trips_avg), 
                                                  0, adm_3_phone_mobility_dat_name$trips_avg)

# Create mobility matrix
M_3 <- ceiling(get_mob_matrix(orig = adm_3_phone_mobility_dat_name$adm_origin,
                              dest = adm_3_phone_mobility_dat_name$adm_destination,
                              value = adm_3_phone_mobility_dat_name$trips_avg))

# Create mobility objects for adm 3
adm_3_pop <- adm_3_population_dat[order(adm_3_population_dat$adm_3_mobility), c(1, 4)]
N_3 <- ceiling(adm_3_pop$population_2020_adm_3)
names(N_3) <- adm_3_pop$adm_3_mobility
adm_3 <- list(M_3, adm_3_dist_mat, N_3)
names(adm_3) <- c('M', 'D', 'N')

##########################
# Administrative Level 2 #
##########################

# Rename/select name data
adm_2_phone_mobility_dat_name <- adm_2_phone_mobility_dat |>
  dplyr::select(c('adm_2_origin', 'adm_2_destination', 'trips_avg')) |>
  dplyr::rename('adm_origin' = 'adm_2_origin',
                'adm_destination' = 'adm_2_destination')

# Create mobility matrix
M_2 <- ceiling(get_mob_matrix(orig = adm_2_phone_mobility_dat_name$adm_origin,
                              dest = adm_2_phone_mobility_dat_name$adm_destination,
                              value = adm_2_phone_mobility_dat_name$trips_avg))

# Create mobility objects for adm 2
adm_2_pop <- adm_2_population_dat[order(adm_2_population_dat$adm_2), c(1, 3)]
N_2 <- ceiling(adm_2_pop$population_2020_adm_2)
names(N_2) <- adm_2_pop$adm_2
adm_2 <- list(M_2, adm_2_dist_mat, N_2)
names(adm_2) <- c('M', 'D', 'N')

##########################
# Administrative Level 1 #
##########################

# Rename/select name data
adm_1_phone_mobility_dat_name <- adm_1_phone_mobility_dat |>
  dplyr::select(c('adm_1_origin', 'adm_1_destination', 'trips_avg')) |>
  dplyr::rename('adm_origin' = 'adm_1_origin',
                'adm_destination' = 'adm_1_destination')

# Create mobility matrix
M_1 <- ceiling(get_mob_matrix(orig = adm_1_phone_mobility_dat_name$adm_origin,
                              dest = adm_1_phone_mobility_dat_name$adm_destination,
                              value = adm_1_phone_mobility_dat_name$trips_avg))

# Create mobility objects for adm 1
adm_1_pop <- adm_1_population_dat[order(adm_1_population_dat$adm_1), c(1, 2)]
N_1 <- ceiling(adm_1_pop$population_2020_adm_1)
names(N_1) <- adm_1_pop$adm_1
adm_1 <- list(M_1, adm_1_dist_mat, N_1)
names(adm_1) <- c('M', 'D', 'N')

############################################
# FIT MOBILITY MODELS TO MOBILE PHONE DATA #
############################################

# Administrative Level 3
model_3 <- mobility(data=adm_3,
                    model='gravity',
                    type='exp',
                    n_chain=2,
                    n_burn=1000,
                    n_samp=1000,
                    n_thin=2,
                    DIC=TRUE)

# Administrative Level 2
model_2 <- mobility(data=adm_2,
                    model='gravity',
                    type='exp',
                    n_chain=2,
                    n_burn=1000,
                    n_samp=1000,
                    n_thin=2,
                    DIC=TRUE)

# Administrative Level 1
model_1 <- mobility(data=adm_1,
                    model='gravity',
                    type='exp',
                    n_chain=2,
                    n_burn=1000,
                    n_samp=1000,
                    n_thin=2,
                    DIC=TRUE)

# Predict for Admin 1, Admin 2, Admin 3 and average across iterations
phone_mobility_adm_1_pred <- predict(model_1, nsim = 100)
phone_mobility_adm_1_pred[phone_mobility_adm_1_pred < 1] <- 0 # censor trips < 1
phone_mobility_adm_1_pred <- apply(phone_mobility_adm_1_pred, 1:2, mean)
phone_mobility_adm_2_pred <- predict(model_2, nsim = 100)
phone_mobility_adm_2_pred[phone_mobility_adm_2_pred < 1] <- 0 # censor trips < 1
phone_mobility_adm_2_pred <- apply(phone_mobility_adm_2_pred, 1:2, mean)
phone_mobility_adm_3_pred <- predict(model_3, nsim = 100)
phone_mobility_adm_3_pred[phone_mobility_adm_3_pred < 1] <- 0 # censor trips < 1
phone_mobility_adm_3_pred <- apply(phone_mobility_adm_3_pred, 1:2, mean)

# Model checks
check(model_1)
check(model_2)
check(model_3)

# Convert to trip proportion
adm_1_phone_pred_mobility_mat <- phone_mobility_adm_1_pred / rowSums(phone_mobility_adm_1_pred)
adm_2_phone_pred_mobility_mat <- phone_mobility_adm_2_pred / rowSums(phone_mobility_adm_2_pred)
adm_3_phone_pred_mobility_mat <- phone_mobility_adm_3_pred / rowSums(phone_mobility_adm_3_pred)

# Save predicted count, proportion data, and model fits
saveRDS(phone_mobility_adm_3_pred, './mobility-spatial-scale/simulated data/adm_3_predictions_count.rds')
saveRDS(adm_3_phone_pred_mobility_mat, './mobility-spatial-scale/simulated data/adm_3_predictions_proportion.rds')
saveRDS(model_3, './mobility-spatial-scale/simulated data/adm_3_model_fit.rds')
saveRDS(phone_mobility_adm_2_pred, './mobility-spatial-scale/simulated data/adm_2_predictions_count.rds')
saveRDS(adm_2_phone_pred_mobility_mat, './mobility-spatial-scale/simulated data/adm_2_predictions_proportion.rds')
saveRDS(model_2, './mobility-spatial-scale/simulated data/adm_2_model_fit.rds')
saveRDS(phone_mobility_adm_1_pred, './mobility-spatial-scale/simulated data/adm_1_predictions_count.rds')
saveRDS(adm_1_phone_pred_mobility_mat, './mobility-spatial-scale/simulated data/adm_1_predictions_proportion.rds')
saveRDS(model_1, './mobility-spatial-scale/simulated data/adm_1_model_fit.rds')

################################################################################
################################################################################

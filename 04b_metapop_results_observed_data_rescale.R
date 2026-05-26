################################################################################
# File Name: 04b_metapop_results_observed_data_rescale                         #
#                                                                              #
# Purpose:   Simulate disease dynamics with the obs. rescaled mobility data.   #
#            Two introduction scenarios (Colombo & Delft) and three            #
#            transmissibility scenarios (R_0 = 1.1, 1.5, 3) are modeled.       #
#            Finally, 25 random introduction scenarios are modeled.            #
#                                                                              #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Simulate epidemics                                             #
#                                                                              #
# Project:   Mobility Spatial Scale                                            #
# Author:    Ronan Corgel                                                      #
################################################################################

####################
# 1. SET-UP SCRIPT #
####################

# Start with a clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(parallel)

# Set the RNG kind for parallel reproducibility
RNGkind("L'Ecuyer-CMRG")

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load model 
source('./mobility-spatial-scale/04_metapop_model.R')

#########################
# 2. SIMULATE EPIDEMICS #
#########################

# Load metapopulation data
# Admin 3
adm_3_phone_mobility_mat_rescale_adm_2 <- readRDS('./out/adm_3_phone_mobility_mat_rescale_adm_2.rds')
adm_3_phone_mobility_mat_rescale_adm_1 <- readRDS('./out/adm_3_phone_mobility_mat_rescale_adm_1.rds')
adm_3_name_vec <- readRDS('./out/adm_3_name_vec.rds')
adm_3_pop_vec <- readRDS('./out/adm_3_pop_vec.rds')
adm_3_x_walk <- readRDS('./out/adm_3_x_walk.rds')

# Admin 2
adm_2_phone_mobility_mat_rescale <- readRDS('./out/adm_2_phone_mobility_mat_rescale.rds')
adm_2_name_vec <- readRDS('./out/adm_2_name_vec.rds')
adm_2_pop_vec <- readRDS('./out/adm_2_pop_vec.rds')
adm_2_x_walk <- readRDS('./out/adm_2_x_walk.rds')

#############
# R_0 = 1.5 #
#############

# Adm 3 at 2 Colombo introduction, R_0 = 1.5
adm_3_col <- mclapply(1:100, run_seir_model, beta = 0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 41,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_2),
                      max_time = 365, time_step = 1)
adm_3_sen_col <- do.call(rbind, adm_3_col)
remove(adm_3_col)

# Save
saveRDS(adm_3_sen_col, file = './out/adm_3_at_2_sen_col_1.5.rds')
remove(adm_3_sen_col)

# Adm 3 at 2 Delft introduction, R_0 = 1.5
adm_3_del <- mclapply(1:100, run_seir_model, beta = 0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 267,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_2),
                      max_time = 365, time_step = 1)
adm_3_sen_del <- do.call(rbind, adm_3_del)
remove(adm_3_del)

# Save
saveRDS(adm_3_sen_del, file = './out/adm_3_at_2_sen_del_1.5.rds')
remove(adm_3_sen_del)

# Adm 3 at 1 Colombo introduction, R_0 = 1.5
adm_3_col <- mclapply(1:100, run_seir_model, beta = 0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 41,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_1),
                      max_time = 365, time_step = 1)
adm_3_sen_col <- do.call(rbind, adm_3_col)
remove(adm_3_col)

# Save
saveRDS(adm_3_sen_col, file = './out/adm_3_at_1_sen_col_1.5.rds')
remove(adm_3_sen_col)

# Adm 3 at 1 Delft introduction, R_0 = 1.5
adm_3_del <- mclapply(1:100, run_seir_model, beta = 0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 267,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_1),
                      max_time = 365, time_step = 1)
adm_3_sen_del <- do.call(rbind, adm_3_del)
remove(adm_3_del)

# Save
saveRDS(adm_3_sen_del, file = './out/adm_3_at_1_sen_del_1.5.rds')
remove(adm_3_sen_del)

# Adm 2 at 1 Colombo introduction, R_0 = 1.5
adm_2_col <- mclapply(1:100, run_seir_model, beta = 0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_2_name_vec, adm_level = '2',
                      pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = 5,
                      adm_x_walk = adm_2_x_walk, travel_mat = as.matrix(adm_2_phone_mobility_mat_rescale),
                      max_time = 365, time_step = 1)
adm_2_sen_col <- do.call(rbind, adm_2_col)
remove(adm_2_col)

# Save
saveRDS(adm_2_sen_col, file = './out/adm_2_sen_col_1.5.rds')
remove(adm_2_sen_col)

# Adm 2 at 1 Delft introduction, R_0 = 1.5
adm_2_del <- mclapply(1:100, run_seir_model, beta = 0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_2_name_vec, adm_level = '2',
                      pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = 18,
                      adm_x_walk = adm_2_x_walk, travel_mat = as.matrix(adm_2_phone_mobility_mat_rescale),
                      max_time = 365, time_step = 1)
adm_2_sen_del <- do.call(rbind, adm_2_del)
remove(adm_2_del)

# Save
saveRDS(adm_2_sen_del, file = './out/adm_2_sen_del_1.5.rds')
remove(adm_2_sen_del)


#############
# R_0 = 1.1 #
#############

# Adm 3 at 2 Colombo introduction, R_0 = 1.1
adm_3_col <- mclapply(1:100, run_seir_model, beta = 0.22, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 41,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_2),
                      max_time = 365, time_step = 1)
adm_3_sen_col <- do.call(rbind, adm_3_col)
remove(adm_3_col)

# Save
saveRDS(adm_3_sen_col, file = './out/adm_3_at_2_sen_col_1.1.rds')
remove(adm_3_sen_col)

# Adm 3 at 2 Delft introduction, R_0 = 1.1
adm_3_del <- mclapply(1:100, run_seir_model, beta = 0.22, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 267,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_2),
                      max_time = 365, time_step = 1)
adm_3_sen_del <- do.call(rbind, adm_3_del)
remove(adm_3_del)

# Save
saveRDS(adm_3_sen_del, file = './out/adm_3_at_2_sen_del_1.1.rds')
remove(adm_3_sen_del)

# Adm 3 at 1 Colombo introduction, R_0 = 1.1
adm_3_col <- mclapply(1:100, run_seir_model, beta = 0.22, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 41,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_1),
                      max_time = 365, time_step = 1)
adm_3_sen_col <- do.call(rbind, adm_3_col)
remove(adm_3_col)

# Save
saveRDS(adm_3_sen_col, file = './out/adm_3_at_1_sen_col_1.1.rds')
remove(adm_3_sen_col)

# Adm 3 at 1 Delft introduction, R_0 = 1.1
adm_3_del <- mclapply(1:100, run_seir_model, beta = 0.22, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 267,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_1),
                      max_time = 365, time_step = 1)
adm_3_sen_del <- do.call(rbind, adm_3_del)
remove(adm_3_del)

# Save
saveRDS(adm_3_sen_del, file = './out/adm_3_at_1_sen_del_1.1.rds')
remove(adm_3_sen_del)

# Adm 2 at 1 Colombo introduction, R_0 = 1.1
adm_2_col <- mclapply(1:100, run_seir_model, beta = 0.22, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_2_name_vec, adm_level = '2',
                      pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = 5,
                      adm_x_walk = adm_2_x_walk, travel_mat = as.matrix(adm_2_phone_mobility_mat_rescale),
                      max_time = 365, time_step = 1)
adm_2_sen_col <- do.call(rbind, adm_2_col)
remove(adm_2_col)

# Save
saveRDS(adm_2_sen_col, file = './out/adm_2_sen_col_1.1.rds')
remove(adm_2_sen_col)

# Adm 2 at 1 Delft introduction, R_0 = 1.1
adm_2_del <- mclapply(1:100, run_seir_model, beta = 0.22, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_2_name_vec, adm_level = '2',
                      pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = 18,
                      adm_x_walk = adm_2_x_walk, travel_mat = as.matrix(adm_2_phone_mobility_mat_rescale),
                      max_time = 365, time_step = 1)
adm_2_sen_del <- do.call(rbind, adm_2_del)
remove(adm_2_del)

# Save
saveRDS(adm_2_sen_del, file = './out/adm_2_sen_del_1.1.rds')
remove(adm_2_sen_del)

#############
# R_0 = 3.0 #
#############

# Adm 3 at 2 Colombo introduction, R_0 = 3.0
adm_3_col <- mclapply(1:100, run_seir_model, beta = 0.6, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 41,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_2),
                      max_time = 365, time_step = 1)
adm_3_sen_col <- do.call(rbind, adm_3_col)
remove(adm_3_col)

# Save
saveRDS(adm_3_sen_col, file = './out/adm_3_at_2_sen_col_3.0.rds')
remove(adm_3_sen_col)

# Adm 3 at 2 Delft introduction, R_0 = 3.0
adm_3_del <- mclapply(1:100, run_seir_model, beta = 0.6, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 267,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_2),
                      max_time = 365, time_step = 1)
adm_3_sen_del <- do.call(rbind, adm_3_del)
remove(adm_3_del)

# Save
saveRDS(adm_3_sen_del, file = './out/adm_3_at_2_sen_del_3.0.rds')
remove(adm_3_sen_del)

# Adm 3 at 1 Colombo introduction, R_0 = 3.0
adm_3_col <- mclapply(1:100, run_seir_model, beta = 0.6, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 41,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_1),
                      max_time = 365, time_step = 1)
adm_3_sen_col <- do.call(rbind, adm_3_col)
remove(adm_3_col)

# Save
saveRDS(adm_3_sen_col, file = './out/adm_3_at_1_sen_col_3.0.rds')
remove(adm_3_sen_col)

# Adm 3 at 1 Delft introduction, R_0 = 3.0
adm_3_del <- mclapply(1:100, run_seir_model, beta = 0.6, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 267,
                      adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_1),
                      max_time = 365, time_step = 1)
adm_3_sen_del <- do.call(rbind, adm_3_del)
remove(adm_3_del)

# Save
saveRDS(adm_3_sen_del, file = './out/adm_3_at_1_sen_del_3.0.rds')
remove(adm_3_sen_del)

# Adm 2 at 1 Colombo introduction, R_0 = 3.0
adm_2_col <- mclapply(1:100, run_seir_model, beta = 0.6, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_2_name_vec, adm_level = '2',
                      pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = 5,
                      adm_x_walk = adm_2_x_walk, travel_mat = as.matrix(adm_2_phone_mobility_mat_rescale),
                      max_time = 365, time_step = 1)
adm_2_sen_col <- do.call(rbind, adm_2_col)
remove(adm_2_col)

# Save
saveRDS(adm_2_sen_col, file = './out/adm_2_sen_col_3.0.rds')
remove(adm_2_sen_col)

# Adm 2 at 1 Delft introduction, R_0 = 3.0
adm_2_del <- mclapply(1:100, run_seir_model, beta = 0.6, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_2_name_vec, adm_level = '2',
                      pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = 18,
                      adm_x_walk = adm_2_x_walk, travel_mat = as.matrix(adm_2_phone_mobility_mat_rescale),
                      max_time = 365, time_step = 1)
adm_2_sen_del <- do.call(rbind, adm_2_del)
remove(adm_2_del)

# Save
saveRDS(adm_2_sen_del, file = './out/adm_2_sen_del_3.0.rds')
remove(adm_2_sen_del)

##################################
# 25 RANDOM SCENARIOS @ R0 = 1.5 #
##################################

# Adm 3
# Randomly sample 25 introduction locations
intro_nums <- readRDS(file = './out/intro_nums_adm_3.rds')

# Create an empty list to fill
adm_3_random <- NULL

# Loop through each location
count <- 1
for (i in intro_nums) {
  print(i)
  adm_3 <- mclapply(1:100, run_seir_model, beta = 0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                    adm_name_vec = adm_3_name_vec, adm_level = '3',
                    pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = i,
                    adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_1),
                    max_time = 365, time_step = 1)
  adm_3_obs <- do.call(rbind, adm_3)
  adm_3_obs$intro_num <- i
  adm_3_random[[count]] <- adm_3_obs
  remove(adm_3, adm_3_obs)
  count <- count + 1
}

# Save
saveRDS(adm_3_random, file = './out/adm_3_random_rescale.rds')
remove(adm_3_random)

################################################################################
################################################################################

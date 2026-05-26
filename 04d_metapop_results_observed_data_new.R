################################################################################
# File Name: 04d_metapop_results_observed_data_new                             #
#                                                                              #
# Purpose:   Simulate disease dynamics with the observed and rescaled mobility # 
#            data but under new spatial aggregations. Choose 50 random         #
#            introduction scenarios and use R_0 = 1.5.                         #
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
set.seed(123456)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load model 
source('./mobility-spatial-scale/04_metapop_model.R')

#########################
# 2. SIMULATE EPIDEMICS #
#########################

# Load metapopulation data (NEW)
# Admin 3
adm_3_phone_mobility_mat <- readRDS('./out/adm_3_phone_mobility_mat.rds')
adm_3_name_vec <- readRDS('./out/adm_3_name_vec.rds')
adm_3_pop_vec <- readRDS('./out/adm_3_pop_vec.rds')
adm_3_x_walk <- readRDS('./out/adm_3_x_walk_new.rds')
# Admin 2
adm_2_phone_mobility_mat <- readRDS('./out/adm_2_phone_mobility_mat_new.rds')
adm_2_name_vec <- readRDS('./out/adm_2_name_vec_new.rds')
adm_2_pop_vec <- readRDS('./out/adm_2_pop_vec_new.rds')
adm_2_x_walk <- readRDS('./out/adm_2_x_walk_new.rds')
# Admin 1
adm_1_phone_mobility_mat <- readRDS('./out/adm_1_phone_mobility_mat_new.rds')
adm_1_name_vec <- readRDS('./out/adm_1_name_vec_new.rds')
adm_1_pop_vec <- readRDS('./out/adm_1_pop_vec_new.rds')

###################################################
# 25 RANDOM SCENARIOS @ R0 = 1.5 W/ OBSERVED DATA #
###################################################

# Adm 3
# Use Adm 3 simulations from 04a since they are the same

# Adm 1
# Just do all 9
intro_nums <- seq(1, 9, 1)

# Create an empty list to fill
adm_1_random <- NULL

# Loop through each location
count <- 1
for (i in intro_nums) {
  print(i)
  adm_1 <- mclapply(1:100, run_seir_model, beta = 0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                    adm_name_vec = adm_1_name_vec, adm_level = '1',
                    pop_vec = adm_1_pop_vec, intro_adm = 'All', intro_num = i,
                    adm_x_walk = adm_2_x_walk, travel_mat = adm_1_phone_mobility_mat,
                    max_time = 365, time_step = 1)
  adm_1_obs <- do.call(rbind, adm_1)
  adm_1_obs <- do.call(rbind, adm_1)
  adm_1_random[[count]] <- adm_1_obs
  remove(adm_1, adm_1_obs)
  count <- count + 1
}

# Save
saveRDS(adm_1_random, file = './out/adm_1_random_new.rds')
remove(adm_1_random)

###################################################
# 25 RANDOM SCENARIOS @ R0 = 1.5 W/ RESCALED DATA #
###################################################

# Load rescaled data (NEW)
adm_3_phone_mobility_mat_rescale_adm_1 <- readRDS('./out/adm_3_phone_mobility_mat_rescale_adm_1_new.rds')

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
saveRDS(adm_3_random, file = './out/adm_3_random_rescale_new.rds')
remove(adm_3_random)

################################################################################
################################################################################

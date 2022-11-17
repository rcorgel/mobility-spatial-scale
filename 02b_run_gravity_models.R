################################################################################
# File Name: 02b_run_gravity_models                                            #
#                                                                              #
# Purpose:   Run the gravity model for different aggregation levels.           #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load gravity model data
#            3. Create simulated trips
#            4. 
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
library(rstan)
options(mc.cores=2)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution')

##############################
# 2. LOAD GRAVITY MODEL DATA #
##############################

# Administrative Unit 3
load('./tmp/adm_3_gravity_dat.RData')

# Administrative Unit 2
load('./tmp/adm_2_gravity_dat.RData')

# Administrative Unit 1
load('./tmp/adm_1_gravity_dat.RData')

#############################
# 3. CREATE SIMULATED TRIPS #
#############################

# Set empty matrix and n
sim_trips <- adm_2_day_avg_mat
sim_trips[!is.na(sim_trips)] <- NA
n <- length(adm_2_pop_vec)

for (i in 1:n) {
  for (j in 1:n) {
    if (!(i == j)) {
      sim_trips[i, j] = 4877.56 * adm_2_pop_vec[j] ^ 0.61  * adm_2_dist_mat[i, j] ^ -2.75
    }
    if ((i == j)) {
      sim_trips[i, j] = 0;
    }
  }
}

# Set the model from stan
# Model with a power decay function for distance
model_pwr = stan_model('./code/gravity_model_constrained_pwr.stan')
# Model with a exponential decay function for distance
model_exp = stan_model('./code/gravity_model_constrained_exp.stan')

fit = sampling(model_pwr,
               list(n = length(adm_3_pop_vec),
                    pop = adm_3_pop_vec,
                    distance_matrix = adm_3_dist_mat,
                    trip_counts = adm_3_day_avg_mat),
               iter = 2000,
               chains = 2)

print(fit)

fit_2 = sampling(model_pwr,
               list(n = length(adm_2_pop_vec),
                    pop = adm_2_pop_vec,
                    distance_matrix = adm_2_dist_mat,
                    trip_counts = adm_2_day_avg_mat),
               iter = 2000,
               chains = 2)

print(fit_2)

fit_1 = sampling(model_pwr,
                 list(n = length(adm_1_pop_vec),
                      pop = adm_1_pop_vec,
                      distance_matrix = adm_1_dist_mat,
                      trip_counts = adm_1_day_avg_mat),
                 iter = 2000,
                 chains = 2)

print(fit_1)

fit_sim = sampling(model_pwr,
               list(n = length(adm_3_pop_vec),
                    pop = adm_3_pop_vec,
                    distance_matrix = adm_3_dist_mat,
                    trip_counts = sim_trips),
               iter = 2000,
               chains = 2)

print(fit_sim)

fit_sim_2 = sampling(model_pwr,
                 list(n = length(adm_2_pop_vec),
                      pop = adm_2_pop_vec,
                      distance_matrix = adm_2_dist_mat,
                      trip_counts = sim_trips),
                 iter = 2000,
                 chains = 2)

print(fit_sim_2)

fit_sim_1 = sampling(model_pwr,
                 list(n = length(adm_1_pop_vec),
                      pop = adm_1_pop_vec,
                      distance_matrix = adm_1_dist_mat,
                      trip_counts = adm_1_day_avg_mat),
                 iter = 2000,
                 chains = 2)

print(fit_1)

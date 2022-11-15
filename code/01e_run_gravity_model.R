################################################################################
# File Name: 01e_run_gravity_model                                             #
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


# Set the model from stan
model = stan_model('./code/gravity_model_constrained.stan')

fit = sampling(model,
               list(n = length(adm_3_pop_vec),
                    pop = adm_3_pop_vec,
                    distance_matrix = adm_3_dist_mat,
                    trip_counts = adm_3_day_avg_mat),
               iter = 2000,
               chains = 4)

#true values are gamma=2, alpha=0.1, beta=0.1
print(fit)

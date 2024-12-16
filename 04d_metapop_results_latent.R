################################################################################
# File Name: 04d_metapop_model_results_latent                                  #
#                                                                              #
# Purpose:   Examine differences in metapopulation model results between       #
#            different mobility spatial scale across infectious latent         #
#            period values.                                                    #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Run metapopulation model at administrative level 3             #
#            3. Run metapopulation model at administrative level 2             #
#            4. Run metapopulation model at administrative level 1             #
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

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load metapopulation model functions
source('./mobility-spatial-scale/04_metapop_model.R')

#########################################################
# 2. RUN METAPOPULATION MODEL AT ADMINISTRATIVE LEVEL 3 #
#########################################################

# First, load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Load simulated mobility data
load('./mobility-spatial-scale/simulated data/mobile_phone_sim_prop_dat.RData')

# Create empty objects to fill
adm_3_at_2_mp_col <- NULL
adm_3_at_1_mp_col <- NULL
adm_3_at_2_mp_mad <- NULL
adm_3_at_1_mp_mad <- NULL
adm_3_at_2_sim_col <- NULL
adm_3_at_1_sim_col <- NULL
adm_3_at_2_sim_mad <- NULL
adm_3_at_1_sim_mad <- NULL

# Create object for mobility data
mobility_dat_adm_3 <- list(adm_3_phone_mobility_mat, adm_3_phone_pred_mobility_mat)

# Run 50 each iterations for two introduction locations and multiple R_0 values
for (i in c('Colombo', 'Madhu')) {
  for (k in seq(1, 2, 1)) {
    for (j in seq(1, 20, 1)) {
      print(j)
      adm_3 <- run_seir_model_multi(n = 50, density_dep = FALSE, method = 'append',
                                   R_0 = 1.5, gamma = 1/7, sigma = 1/j, prop_s = 0.90, 
                                   adm_name_vec = adm_3_name_vec, adm_level = '3', 
                                   pop_vec = adm_3_pop_vec, intro_adm = i, intro_num = 1,
                                   adm_x_walk = adm_3_x_walk, travel_mat = mobility_dat_adm_3[[k]], 
                                   max_time = 365, time_step = 1, mobility = TRUE)
      
      # Aggregate to Admin 2
      adm_3_at_2 <- adm_3 %>% group_by(time, adm_2, run_num) %>%
        # Calculate incidence at Admin 2
        mutate(incid_I_adm_2 = sum(incid_I)) %>%
        distinct(time, run_num, adm_2, incid_I_adm_2) %>% 
        ungroup() %>%
        group_by(time, adm_2) %>%
        mutate(avg_incid_I_adm_2 = mean(incid_I_adm_2)) %>%
        distinct(time, adm_2, avg_incid_I_adm_2) %>% 
        ungroup() %>%
        # Calculate total epidemic size
        mutate(magnitude = sum(avg_incid_I_adm_2)) %>%
        group_by(adm_2) %>%
        # Calculate cumulative cases by Admin 2
        # Indicate when cumulative cases > 1 for each unit
        mutate(cum_I = cumsum(avg_incid_I_adm_2),
               intro = ifelse(cum_I > 1, 1, 0)) %>%
        # Indicate the first instance when cumulative > 1
        mutate(intro_first = intro == 1 & !duplicated(intro == 1)) %>%
        # Filter to first instance for all Admin 2
        filter(intro_first == TRUE) %>%
        ungroup() %>%
        arrange(time) %>%
        slice_tail(n = 24) %>%
        mutate(intro_time = mean(time),
               intro_loc = j) %>%
        slice(1) %>% select(intro_loc, magnitude, intro_time)
      
      # Aggregate to Admin 1
      adm_3_at_1 <- adm_3 %>% group_by(time, adm_1, run_num) %>%
        # Calculate incidence at Admin 1
        mutate(incid_I_adm_1 = sum(incid_I)) %>%
        distinct(time, run_num, adm_1, incid_I_adm_1) %>% 
        ungroup() %>%
        group_by(time, adm_1) %>%
        mutate(avg_incid_I_adm_1 = mean(incid_I_adm_1)) %>%
        distinct(time, adm_1, avg_incid_I_adm_1) %>% 
        ungroup() %>%
        # Calculate total epidemic size
        mutate(magnitude = sum(avg_incid_I_adm_1)) %>%
        group_by(adm_1) %>%
        # Calculate cumulative cases by Admin 1
        # Indicate when cumulative cases > 1 for each unit
        mutate(cum_I = cumsum(avg_incid_I_adm_1),
               intro = ifelse(cum_I > 1, 1, 0)) %>%
        # Indicate the first instance when cumulative > 1
        mutate(intro_first = intro == 1 & !duplicated(intro == 1)) %>%
        # Filter to first instance for all Admin 1
        filter(intro_first == TRUE) %>%
        ungroup() %>%
        arrange(time) %>%
        slice_tail(n = 8) %>%
        mutate(intro_time = mean(time),
               intro_loc = j) %>%
        slice(1) %>% select(intro_loc, magnitude, intro_time)
      
      if (i == 'Colombo') {
        if (k == 1) {
          adm_3_at_2_mp_col <- rbind(adm_3_at_2_mp_col, adm_3_at_2)
          adm_3_at_1_mp_col <- rbind(adm_3_at_1_mp_col, adm_3_at_1)
        }
        if (k == 2) {
          adm_3_at_2_sim_col <- rbind(adm_3_at_2_sim_col, adm_3_at_2)
          adm_3_at_1_sim_col <- rbind(adm_3_at_1_sim_col, adm_3_at_1)
        }
      }
      if (i == 'Madhu') {
        if (k == 1) {
          adm_3_at_2_mp_mad <- rbind(adm_3_at_2_mp_mad, adm_3_at_2)
          adm_3_at_1_mp_mad <- rbind(adm_3_at_1_mp_mad, adm_3_at_1)
        }
        if (k == 2) {
          adm_3_at_2_sim_mad <- rbind(adm_3_at_2_sim_mad, adm_3_at_2)
          adm_3_at_1_sim_mad <- rbind(adm_3_at_1_sim_mad, adm_3_at_1)
        }
      }
    }
  }
}

#########################################################
# 3. RUN METAPOPULATION MODEL AT ADMINISTRATIVE LEVEL 2 #
#########################################################

# Create empty objects to fill
adm_2_at_2_mp_col <- NULL
adm_2_at_1_mp_col <- NULL
adm_2_at_2_mp_mad <- NULL
adm_2_at_1_mp_mad <- NULL
adm_2_at_2_sim_col <- NULL
adm_2_at_1_sim_col <- NULL
adm_2_at_2_sim_mad <- NULL
adm_2_at_1_sim_mad <- NULL

# Create object for mobility data
mobility_dat_adm_2 <- list(adm_2_phone_mobility_mat, adm_2_phone_pred_mobility_mat)

# Run 50 each iterations for two introduction locations and multiple R_0 values
for (i in c('Colombo', 'Madhu')) {
  for (k in seq(1, 2, 1)) {
    for (j in seq(1, 20, 1)) {
      adm_2 <- run_seir_model_multi(n = 50, density_dep = FALSE, method = 'append',
                                   R_0 = 1.5, gamma = 1/7, sigma = 1/j, prop_s = 0.90, 
                                   adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                   pop_vec = adm_2_pop_vec, intro_adm = i, intro_num = 1,
                                   adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_2[[k]], 
                                   max_time = 365, time_step = 1, mobility = TRUE)
      
      # Admin 2
      adm_2_at_2 <- adm_2 %>%
        group_by(time, adm_2) %>%
        mutate(avg_incid_I_adm_2 = mean(incid_I)) %>%
        distinct(time, adm_2, avg_incid_I_adm_2) %>% 
        ungroup() %>%
        # Calculate total epidemic size
        mutate(magnitude = sum(avg_incid_I_adm_2)) %>%
        group_by(adm_2) %>%
        # Calculate cumulative cases by Admin 2
        # Indicate when cumulative cases > 1 for each unit
        mutate(cum_I = cumsum(avg_incid_I_adm_2),
               intro = ifelse(cum_I > 1, 1, 0)) %>%
        # Indicate the first instance when cumulative > 1
        mutate(intro_first = intro == 1 & !duplicated(intro == 1)) %>%
        # Filter to first instance for all Admin 2
        filter(intro_first == TRUE) %>%
        ungroup() %>%
        arrange(time) %>%
        slice_tail(n = 24) %>%
        mutate(intro_time = mean(time),
               intro_loc = j) %>%
        slice(1) %>% select(intro_loc, magnitude, intro_time)
      
      # Aggregate to Admin 1
      adm_2_at_1 <- adm_2 %>% group_by(time, adm_1, run_num) %>%
        # Calculate incidence at Admin 1
        mutate(incid_I_adm_1 = sum(incid_I)) %>%
        distinct(time, run_num, adm_1, incid_I_adm_1) %>% 
        ungroup() %>%
        group_by(time, adm_1) %>%
        mutate(avg_incid_I_adm_1 = mean(incid_I_adm_1)) %>%
        distinct(time, adm_1, avg_incid_I_adm_1) %>% 
        ungroup() %>%
        # Calculate total epidemic size
        mutate(magnitude = sum(avg_incid_I_adm_1)) %>%
        group_by(adm_1) %>%
        # Calculate cumulative cases by Admin 1
        # Indicate when cumulative cases > 1 for each unit
        mutate(cum_I = cumsum(avg_incid_I_adm_1),
               intro = ifelse(cum_I > 1, 1, 0)) %>%
        # Indicate the first instance when cumulative > 1
        mutate(intro_first = intro == 1 & !duplicated(intro == 1)) %>%
        # Filter to first instance for all Admin 1
        filter(intro_first == TRUE) %>%
        ungroup() %>%
        arrange(time) %>%
        slice_tail(n = 8) %>%
        mutate(intro_time = mean(time),
               intro_loc = j) %>%
        slice(1) %>% select(intro_loc, magnitude, intro_time)
      
      if (i == 'Colombo') {
        if (k == 1) {
          adm_2_at_2_mp_col <- rbind(adm_2_at_2_mp_col, adm_2_at_2)
          adm_2_at_1_mp_col <- rbind(adm_2_at_1_mp_col, adm_2_at_1)
        }
        if (k == 2) {
          adm_2_at_2_sim_col <- rbind(adm_2_at_2_sim_col, adm_2_at_2)
          adm_2_at_1_sim_col <- rbind(adm_2_at_1_sim_col, adm_2_at_1)
        }
      }
      if (i == 'Madhu') {
        if (k == 1) {
          adm_2_at_2_mp_mad <- rbind(adm_2_at_2_mp_mad, adm_2_at_2)
          adm_2_at_1_mp_mad <- rbind(adm_2_at_1_mp_mad, adm_2_at_1)
        }
        if (k == 2) {
          adm_2_at_2_sim_mad <- rbind(adm_2_at_2_sim_mad, adm_2_at_2)
          adm_2_at_1_sim_mad <- rbind(adm_2_at_1_sim_mad, adm_2_at_1)
        }
      }
    }
  }
}

#########################################################
# 4. RUN METAPOPULATION MODEL AT ADMINISTRATIVE LEVEL 1 #
#########################################################

# Create empty objects to fill
adm_1_at_1_mp_col <- NULL
adm_1_at_1_mp_mad <- NULL
adm_1_at_1_sim_col <- NULL
adm_1_at_1_sim_mad <- NULL

# Create object for mobility data
mobility_dat_adm_1 <- list(adm_1_phone_mobility_mat, adm_1_phone_pred_mobility_mat)

# Run 50 each iterations for two introduction locations and multiple R_0 values
for (i in c('Colombo', 'Madhu')) {
  for (k in seq(1, 2, 1)) {
    for (j in seq(1, 20, 1)) {
      adm_1 <- run_seir_model_multi(n = 50, density_dep = FALSE, method = 'append',
                                   R_0 = 1.5, gamma = 1/7, sigma = 1/j, prop_s = 0.90, 
                                   adm_name_vec = adm_1_name_vec, adm_level = '1', 
                                   pop_vec = adm_1_pop_vec, intro_adm = i, intro_num = 1,
                                   adm_x_walk = adm_1_x_walk, travel_mat = mobility_dat_adm_1[[k]], 
                                   max_time = 365, time_step = 1, mobility = TRUE)
      
      # Admin 1
      adm_1_at_1 <- adm_1 %>%
        group_by(time, adm_1) %>%
        mutate(avg_incid_I_adm_1 = mean(incid_I)) %>%
        distinct(time, adm_1, avg_incid_I_adm_1) %>% 
        ungroup() %>%
        # Calculate total epidemic size
        mutate(magnitude = sum(avg_incid_I_adm_1)) %>%
        group_by(adm_1) %>%
        # Calculate cumulative cases by Admin 1
        # Indicate when cumulative cases > 1 for each unit
        mutate(cum_I = cumsum(avg_incid_I_adm_1),
               intro = ifelse(cum_I > 1, 1, 0)) %>%
        # Indicate the first instance when cumulative > 1
        mutate(intro_first = intro == 1 & !duplicated(intro == 1)) %>%
        # Filter to first instance for all Admin 1
        filter(intro_first == TRUE) %>%
        ungroup() %>%
        arrange(time) %>%
        slice_tail(n = 8) %>%
        mutate(intro_time = mean(time),
               intro_loc = j) %>%
        slice(1) %>% select(intro_loc, magnitude, intro_time)
      
      if (i == 'Colombo') {
        if (k == 1) {
          adm_1_at_1_mp_col <- rbind(adm_1_at_1_mp_col, adm_1_at_1)
        }
        if (k == 2) {
          adm_1_at_1_sim_col <- rbind(adm_1_at_1_sim_col, adm_1_at_1)
        }
      }
      if (i == 'Madhu') {
        if (k == 1) {
          adm_1_at_1_mp_mad <- rbind(adm_1_at_1_mp_mad, adm_1_at_1)
        }
        if (k == 2) {
          adm_1_at_1_sim_mad <- rbind(adm_1_at_1_sim_mad, adm_1_at_1)
        }
      }
    }
  }
}

# Save all results
save(list = c('adm_1_at_1_mp_col',
              'adm_2_at_1_mp_col', 
              'adm_3_at_1_mp_col', 
              'adm_3_at_2_mp_col', 
              'adm_2_at_2_mp_col',
              'adm_1_at_1_mp_mad',
              'adm_2_at_1_mp_mad', 
              'adm_3_at_1_mp_mad', 
              'adm_3_at_2_mp_mad', 
              'adm_2_at_2_mp_mad',
              'adm_1_at_1_sim_col',
              'adm_2_at_1_sim_col', 
              'adm_3_at_1_sim_col', 
              'adm_3_at_2_sim_col', 
              'adm_2_at_2_sim_col',
              'adm_1_at_1_sim_mad',
              'adm_2_at_1_sim_mad', 
              'adm_3_at_1_sim_mad', 
              'adm_3_at_2_sim_mad', 
              'adm_2_at_2_sim_mad'), 
     file = './tmp/latent_model_results.RData')

################################################################################
################################################################################

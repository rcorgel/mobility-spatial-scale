################################################################################
# File Name: 04e_metapop_results_intro_loc_sens                                #
#                                                                              #
# Purpose:   Examine differences in metapopulation model results between       #
#            different mobility spatial scale across introduction locations    #
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

# First, load metapopulation model data at each scale
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Load rescaled mobility data
load('./tmp/rescale_phone_mobility_dat.RData')

# Create function to run multiple simulations at admin level 3 and aggregate to
# levels 1 and 2, then run in parallel
run_seir_model_multi_adm_3 <- function(intro_num, data) {
  # Show progress
  print(intro_num)
  
  # Run model
  adm_3 <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                R_0 = 1.5, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                adm_name_vec = adm_3_name_vec, adm_level = '3', 
                                pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = intro_num,
                                adm_x_walk = adm_3_x_walk, travel_mat = data, 
                                max_time = 365, time_step = 1, mobility = TRUE)
  
  # Aggregate to Admin 2
  adm_3_at_2 <- adm_3 |> group_by(time, adm_2, run_num) |>
    # Calculate incidence at Admin 2
    mutate(incid_I_adm_2 = sum(incid_I)) |>
    distinct(time, run_num, adm_2, incid_I_adm_2) |> 
    ungroup() |>
    # Calculate burn out and drop runs that did not take off
    group_by(run_num) |>
    mutate(run_total = sum(incid_I_adm_2)) |>
    ungroup() |>
    mutate(burn_out_perc = sum(run_total > 100) / n()) |>
    filter(run_total > 100) |>
    # Calculate average take-off simulation across runs
    group_by(time, adm_2) |>
    mutate(avg_incid_I_adm_2 = mean(incid_I_adm_2)) |>
    distinct(time, adm_2, avg_incid_I_adm_2, burn_out_perc) |> 
    ungroup() |>
    # Calculate total epidemic size
    mutate(magnitude = sum(avg_incid_I_adm_2)) |>
    group_by(adm_2) |>
    # Calculate cumulative cases by Admin 2
    # Indicate when cumulative cases > 1 for each unit
    mutate(cum_I = cumsum(avg_incid_I_adm_2),
           intro = ifelse(cum_I > 1, 1, 0)) |>
    # Indicate the first instance when cumulative > 1
    mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
    # Filter to first instance for all Admin 2
    filter(intro_first == TRUE) |>
    ungroup() |>
    arrange(time) |>
    slice_tail(n = 24) |>
    mutate(intro_time = mean(time),
           intro_loc = intro_num) |>
    slice(1) |> dplyr::select(intro_loc, magnitude, intro_time, burn_out_perc) |>
    mutate(level = '2')
  
  # Aggregate to Admin 1
  adm_3_at_1 <- adm_3 |> group_by(time, adm_1, run_num) |>
    # Calculate incidence at Admin 1
    mutate(incid_I_adm_1 = sum(incid_I)) |>
    distinct(time, run_num, adm_1, incid_I_adm_1) |> 
    ungroup() |>
    # Calculate burn out and drop runs that did not take off
    group_by(run_num) |>
    mutate(run_total = sum(incid_I_adm_1)) |>
    ungroup() |>
    mutate(burn_out_perc = sum(run_total > 100) / n()) |>
    filter(run_total > 100) |>
    # Calculate average take-off simulation across runs
    group_by(time, adm_1) |>
    mutate(avg_incid_I_adm_1 = mean(incid_I_adm_1)) |>
    distinct(time, adm_1, avg_incid_I_adm_1, burn_out_perc) |> 
    ungroup() |>
    # Calculate total epidemic size
    mutate(magnitude = sum(avg_incid_I_adm_1)) |>
    group_by(adm_1) |>
    # Calculate cumulative cases by Admin 1
    # Indicate when cumulative cases > 1 for each unit
    mutate(cum_I = cumsum(avg_incid_I_adm_1),
           intro = ifelse(cum_I > 1, 1, 0)) |>
    # Indicate the first instance when cumulative > 1
    mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
    # Filter to first instance for all Admin 1
    filter(intro_first == TRUE) |>
    ungroup() |>
    arrange(time) |>
    slice_tail(n = 8) |>
    mutate(intro_time = mean(time),
           intro_loc = intro_num) |>
    slice(1) |> dplyr::select(intro_loc, magnitude, intro_time, burn_out_perc) |>
    mutate(level = '1')
  
  # Return summarized data
  return(list(adm_3_at_2, adm_3_at_1))
}

# Observed data: loop through all introduction locations and save to object
adm_3_mp <- mclapply(1:330, run_seir_model_multi_adm_3, data = adm_3_phone_mobility_mat_rescale_adm_1)
adm_3_mp_df <- bind_rows(adm_3_mp)
adm_3_at_1_mp_sens_1 <- adm_3_mp_df |> filter(level == '1')
adm_3_at_2_mp_sens_1 <- adm_3_mp_df |> filter(level == '2')

# Simulated data: loop through all introduction locations and save to object
adm_3_mp <- mclapply(1:330, run_seir_model_multi_adm_3, data = adm_3_phone_mobility_mat_rescale_adm_2)
adm_3_mp_df <- bind_rows(adm_3_mp)
adm_3_at_1_mp_sens_2 <- adm_3_mp_df |> filter(level == '1')
adm_3_at_2_mp_sens_2 <- adm_3_mp_df |> filter(level == '2')

#########################################################
# 3. RUN METAPOPULATION MODEL AT ADMINISTRATIVE LEVEL 2 #
#########################################################

# Create empty objects to fill
adm_2_at_2_mp_sens_1 <- NULL
adm_2_at_1_mp_sens_1 <- NULL
adm_2_at_2_mp_sens_2 <- NULL
adm_2_at_1_mp_sens_2 <- NULL

# Create object for mobility data
mobility_dat_adm_2 <- list(adm_2_phone_mobility_mat_rescale, adm_2_phone_mobility_mat)

# Run 50 iterations for each introduction location
for (i in seq(1, 2, 1)) {
  for (j in seq(1, 25, 1)) {
    # Run model
    adm_2 <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                  R_0 = 1.5, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                  adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                  pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = j,
                                  adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_2[[i]], 
                                  max_time = 365, time_step = 1, mobility = TRUE)
    
    # Admin 2
    adm_2_at_2 <- adm_2 |>
      # Calculate burn out and drop runs that did not take off
      group_by(run_num) |>
      mutate(run_total = sum(incid_I)) |>
      ungroup() |>
      mutate(burn_out_perc = sum(run_total > 100) / n()) |>
      filter(run_total > 100) |>
      # Calculate average take-off simulation across runs
      group_by(time, adm_2) |>
      mutate(avg_incid_I_adm_2 = mean(incid_I)) |>
      distinct(time, adm_2, avg_incid_I_adm_2, burn_out_perc) |> 
      ungroup() |>
      # Calculate total epidemic size
      mutate(magnitude = sum(avg_incid_I_adm_2)) |>
      group_by(adm_2) |>
      # Calculate cumulative cases by Admin 2
      # Indicate when cumulative cases > 1 for each unit
      mutate(cum_I = cumsum(avg_incid_I_adm_2),
             intro = ifelse(cum_I > 1, 1, 0)) |>
      # Indicate the first instance when cumulative > 1
      mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
      # Filter to first instance for all Admin 2
      filter(intro_first == TRUE) |>
      ungroup() |>
      arrange(time) |>
      slice_tail(n = 24) |>
      mutate(intro_time = mean(time),
             intro_loc = j) |>
      slice(1) |> dplyr::select(intro_loc, magnitude, intro_time, burn_out_perc)
    
    # Aggregate to Admin 1
    adm_2_at_1 <- adm_2 |> group_by(time, adm_1, run_num) |>
      # Calculate incidence at Admin 1
      mutate(incid_I_adm_1 = sum(incid_I)) |>
      distinct(time, run_num, adm_1, incid_I_adm_1) |> 
      ungroup() |>
      # Calculate burn out and drop runs that did not take off
      group_by(run_num) |>
      mutate(run_total = sum(incid_I_adm_1)) |>
      ungroup() |>
      mutate(burn_out_perc = sum(run_total > 100) / n()) |>
      filter(run_total > 100) |>
      # Calculate average take-off simulation across runs
      group_by(time, adm_1) |>
      mutate(avg_incid_I_adm_1 = mean(incid_I_adm_1)) |>
      distinct(time, adm_1, avg_incid_I_adm_1, burn_out_perc) |> 
      ungroup() |>
      # Calculate total epidemic size
      mutate(magnitude = sum(avg_incid_I_adm_1)) |>
      group_by(adm_1) |>
      # Calculate cumulative cases by Admin 1
      # Indicate when cumulative cases > 1 for each unit
      mutate(cum_I = cumsum(avg_incid_I_adm_1),
             intro = ifelse(cum_I > 1, 1, 0)) |>
      # Indicate the first instance when cumulative > 1
      mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
      # Filter to first instance for all Admin 1
      filter(intro_first == TRUE) |>
      ungroup() |>
      arrange(time) |>
      slice_tail(n = 8) |>
      mutate(intro_time = mean(time),
             intro_loc = j) |>
      slice(1) |> dplyr::select(intro_loc, magnitude, intro_time, burn_out_perc)
    
    # Fill objects with results
    if (i == 1) {
      adm_2_at_2_mp_sens_1 <- rbind(adm_2_at_2_mp_sens_1, adm_2_at_2)
      adm_2_at_1_mp_sens_1 <- rbind(adm_2_at_1_mp_sens_1, adm_2_at_1)
    }
    if (i == 2) {
      adm_2_at_2_mp_sens_2 <- rbind(adm_2_at_2_mp_sens_2, adm_2_at_2)
      adm_2_at_1_mp_sens_2 <- rbind(adm_2_at_1_mp_sens_2, adm_2_at_1)
    }
  } 
}

# Save results
save(list = c('adm_3_at_1_mp_sens_1',
              'adm_3_at_2_mp_sens_2', 
              'adm_2_at_1_mp_sens_1', 
              'adm_2_at_2_mp_sens_2'), 
     file = './tmp/introduction_location_model_results_obs_sens.RData')

################################################################################
################################################################################

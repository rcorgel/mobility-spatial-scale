################################################################################
# File Name: 04a_metapop_results_intro_loc                                     #
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
library(parallel)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load metapopulation model functions
source('./mobility-spatial-scale/04_metapop_model_2.R')

#########################################################
# 2. RUN METAPOPULATION MODEL AT ADMINISTRATIVE LEVEL 3 #
#########################################################

# First, load metapopulation model data at each scale
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Ampara Example
adm_1 <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                              R_0 = 1.1, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                              adm_name_vec = adm_1_name_vec, adm_level = '1', 
                              pop_vec = adm_1_pop_vec, intro_adm = 'All', intro_num = 9,
                              adm_x_walk = adm_2_x_walk, travel_mat = adm_1_phone_mobility_mat, 
                              max_time = 365, time_step = 1, mobility = TRUE, mixing = FALSE,
                              mixing_mat = 0)

adm_1_at_1 <- adm_1 |>
  # Calculate burn out and drop runs that did not take off
  group_by(run_num) |>
  mutate(run_total = sum(incid_I)) |>
  ungroup() |>
  mutate(burn_out_perc = sum(run_total > 100) / n()) |>
  filter(run_total > 100) |>
  # Calculate total epidemic size
  group_by(run_num) |>
  mutate(magnitude = sum(incid_I)) |>
  ungroup() |>
  group_by(run_num, adm_1) |>
  # Calculate cumulative cases by Admin 1
  # Indicate when cumulative cases > 1 for each unit
  mutate(cum_I = cumsum(incid_I),
         intro = ifelse(cum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all Admin 1
  filter(intro_first == TRUE) |>
  ungroup() |>
  group_by(run_num) |>
  arrange(time) |>
  slice(1:5) |>
  mutate(intro_time = mean(time),
         intro_loc = 2) |>
  distinct(intro_loc, run_num, magnitude, intro_time, burn_out_perc) |>
  mutate(level = '1')

adm_2 <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                              R_0 = 1.1, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                              adm_name_vec = adm_2_name_vec, adm_level = '2', 
                              pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = 10,
                              adm_x_walk = adm_2_x_walk, travel_mat = adm_2_phone_mobility_mat, 
                              max_time = 365, time_step = 1, mobility = TRUE, mixing = FALSE,
                              mixing_mat = 0)

adm_2_at_2 <- adm_2 |>
  # Calculate burn out and drop runs that did not take off
  group_by(run_num) |>
  mutate(run_total = sum(incid_I)) |>
  ungroup() |>
  mutate(burn_out_perc = sum(run_total > 100) / n()) |>
  filter(run_total > 100) |>
  # Calculate total epidemic size
  group_by(run_num) |>
  mutate(magnitude = sum(incid_I)) |>
  ungroup() |>
  group_by(run_num, adm_2) |>
  # Calculate cumulative cases by Admin 2
  # Indicate when cumulative cases > 1 for each unit
  mutate(cum_I = cumsum(incid_I),
         intro = ifelse(cum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all Admin 2
  filter(intro_first == TRUE) |>
  ungroup() |>
  group_by(run_num) |>
  arrange(time) |>
  slice(1:13) |>
  mutate(intro_time = mean(time),
         intro_loc = 1) |>
  distinct(intro_loc, run_num, magnitude, intro_time, burn_out_perc) |>
  mutate(level = '2')
  

adm_3 <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                              R_0 = 1.1, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                              adm_name_vec = adm_3_name_vec, adm_level = '3', 
                              pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 55,
                              adm_x_walk = adm_3_x_walk, travel_mat = adm_3_phone_mobility_mat, 
                              max_time = 365, time_step = 1, mobility = TRUE, mixing = FALSE,
                              mixing_mat = 0)


# Aggregate to Admin 2
adm_3_at_2 <- adm_3 |> group_by(time, adm_2, run_num) |>
  # Calculate incidence at Admin 2
  mutate(incid_I = sum(incid_I)) |>
  distinct(time, run_num, adm_2, incid_I) |> 
  ungroup() |>
  # Calculate burn out and drop runs that did not take off
  group_by(run_num) |>
  mutate(run_total = sum(incid_I)) |>
  ungroup() |>
  mutate(burn_out_perc = sum(run_total > 100) / n()) |>
  filter(run_total > 100) |>
  # Calculate total epidemic size
  group_by(run_num) |>
  mutate(magnitude = sum(incid_I)) |>
  ungroup() |>
  group_by(run_num, adm_2) |>
  # Calculate cumulative cases by Admin 2
  # Indicate when cumulative cases > 1 for each unit
  mutate(cum_I = cumsum(incid_I),
         intro = ifelse(cum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all Admin 2
  filter(intro_first == TRUE) |>
  ungroup() |>
  group_by(run_num) |>
  arrange(time) |>
  slice(1:13) |>
  mutate(intro_time = mean(time),
         intro_loc = 1) |>
  distinct(intro_loc, run_num, magnitude, intro_time, burn_out_perc) |>
  mutate(level = '2 from 3')

# Aggregate to Admin 1
adm_3_at_1 <- adm_3 |> group_by(time, adm_1, run_num) |>
  # Calculate incidence at Admin 1
  mutate(incid_I = sum(incid_I)) |>
  distinct(time, run_num, adm_1, incid_I) |> 
  ungroup() |>
  # Calculate burn out and drop runs that did not take off
  group_by(run_num) |>
  mutate(run_total = sum(incid_I)) |>
  ungroup() |>
  mutate(burn_out_perc = sum(run_total > 100) / n()) |>
  filter(run_total > 100) |>
  # Calculate total epidemic size
  group_by(run_num) |>
  mutate(magnitude = sum(incid_I)) |>
  ungroup() |>
  group_by(run_num, adm_1) |>
  # Calculate cumulative cases by Admin 1
  # Indicate when cumulative cases > 1 for each unit
  mutate(cum_I = cumsum(incid_I),
         intro = ifelse(cum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all Admin 1
  filter(intro_first == TRUE) |>
  ungroup() |>
  group_by(run_num) |>
  arrange(time) |>
  slice(1:5) |>
  mutate(intro_time = mean(time),
         intro_loc = 2) |>
  distinct(intro_loc, run_num, magnitude, intro_time, burn_out_perc) |>
  mutate(level = '1 from 3')



test <- rbind(adm_1_at_1, adm_2_at_2, adm_3_at_2, adm_3_at_1)


ggplot(test) + geom_boxplot(aes(y = intro_time, x = as.factor(level)))






# Run 50 iterations for each introduction location
  for (j in seq(1, 25, 1)) {
    # Run model
    adm_2 <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                  R_0 = 1.05, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                  adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                  pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = j,
                                  adm_x_walk = adm_2_x_walk, travel_mat = adm_2_phone_mobility_mat, 
                                  max_time = 365, time_step = 1, mobility = TRUE, mixing = FALSE,
                                  mixing_mat = 0)
    
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
      adm_2_at_2_mp <- rbind(adm_2_at_2_mp, adm_2_at_2)
      adm_2_at_1_mp <- rbind(adm_2_at_1_mp, adm_2_at_1)

  } 


load('./tmp/fmt_adm_2_phone_mobility_dat.RData')

adm_2_phone_mobility_long
adm_2_int <- adm_2_phone_mobility_long |> dplyr::filter(origin == destination) |>
  arrange(origin) |>
  mutate(merge = row_number()) |>
  rename(internal = value)

adm_2_at_2_mp$pop <- adm_2_pop_vec
adm_2_at_2_mp$name <- adm_2_name_vec

test <-  adm_2_phone_mobility_mat %*% (1 - adm_2_int$internal)


adm_2_at_2_mp$int_2 <- as.data.frame(test)$V1
adm_2_at_2_mp$int_1 <- adm_2_int$internal

eff <- c(adm_2_pop_vec %*% adm_2_phone_mobility_mat)

adm_2_at_2_mp$eff <- eff

ggplot(adm_2_at_2_mp) + geom_point(aes(x = int_2, y = intro_time, color = log(pop))) + geom_smooth(aes(x = int_2, y = intro_time), method = 'lm')




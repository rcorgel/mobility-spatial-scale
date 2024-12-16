################################################################################
# File Name: 03d_metapop_results_covid                                         #
#                                                                              #
# Purpose:   Examine differences in metapopulation model results between       #
#            different mobility spatial scale comparing to true COVID-19       #
#            data.                                                             #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Run metapopulation model at administrative level 2             #
#            3. Run metapopulation model at administrative level 1             #
#            4. Create and save model timeseries average results               #
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
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

# Load metapopulation model functions
source('./mobility-spatial-scale/04_metapop_model.R')

#########################################################
# 2. RUN METAPOPULATION MODEL AT ADMINISTRATIVE LEVEL 2 #
#########################################################

# Load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Load simulated mobility data
load('./mobility-spatial-scale/simulated data/mobile_phone_sim_prop_dat.RData')

# Administrative Level 2
# Run 1000 iterations
# Assuming mobility
adm_2_mobile <- run_sir_model_multi(n = 1000, density_dep = FALSE, method = 'append',
                                    R_0 = 1.50, gamma = 1/10, sigma = 1/3, prop_s = 0.95, 
                                    adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                    pop_vec = adm_2_pop_vec, intro_adm = 'COVID',
                                    adm_x_walk = adm_2_x_walk, travel_mat = adm_2_phone_mobility_mat, 
                                    max_time = 200, time_step = 1, mobility = TRUE)

# Assuming simulated mobility
adm_2_mobile_sim <- run_sir_model_multi(n = 1000, density_dep = FALSE, method = 'append',
                                    R_0 = 1.50, gamma = 1/10, sigma = 1/3, prop_s = 0.95, 
                                    adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                    pop_vec = adm_2_pop_vec, intro_adm = 'COVID',
                                    adm_x_walk = adm_2_x_walk, travel_mat = adm_2_phone_pred_mobility_mat, 
                                    max_time = 200, time_step = 1, mobility = TRUE)

# Assuming no mobility
adm_2_none <- run_sir_model_multi(n = 1000, density_dep = FALSE, method = 'append',
                                  R_0 = 1.50, gamma = 1/10, sigma = 1/3, prop_s = 0.95, 
                                  adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                  pop_vec = adm_2_pop_vec, intro_adm = 'COVID',
                                  adm_x_walk = adm_2_x_walk, travel_mat = adm_2_phone_mobility_mat, 
                                  max_time = 200, time_step = 1, mobility = FALSE)

# Create empty objects to fill
adm_2_avg_2_run_list <- NULL
adm_2_avg_2_avg_list <- NULL
adm_2_avg_1_run_list <- NULL
adm_2_avg_1_avg_list <- NULL
adm_2_results <- list(adm_2_mobile, adm_2_mobile_sim, adm_2_none)
# Set counter
count <- 0

# Loop through mobility/no mobility results
for (i in c(1, 2, 3)) {
  # Increase counter
  count <- count + 1
  
  # Admin 2
  # Create introduction times for all iterations
  adm_2_avg_2_run <- adm_2_results[[i]] %>% 
    group_by(adm_2, run_num) %>%
    mutate(intro_first_5 = incid_I > 5 & !duplicated(incid_I > 5)) %>%
    filter(intro_first_5 == TRUE) %>%
    ungroup() 
  # Fill object
  adm_2_avg_2_run_list[[i]] <- adm_2_avg_2_run
  
  # Create average introduction times across all iterations
  # With mean and 95% interval
  adm_2_avg_2_avg <- adm_2_results[[i]] %>% 
    group_by(time, adm_2) %>%
    mutate(incid_I_avg = mean(incid_I),
           incid_I_up = quantile(incid_I, probs = 0.975), 
           incid_I_low = quantile(incid_I, probs = 0.025)) %>%
    distinct(time, adm_2, adm_1, adm_level, incid_I_avg, incid_I_up, incid_I_low,
             .keep_all = FALSE) %>%
    ungroup() %>%
    group_by(adm_2) %>%
    mutate(intro_first_5 = incid_I_avg > 5 & !duplicated(incid_I_avg > 5),
           intro_first_5_up = incid_I_up > 5 & !duplicated(incid_I_up > 5),
           intro_first_5_low = incid_I_low > 5 & !duplicated(incid_I_low > 5))
  
  # Filter and merge mean and 95% thresholds together
  adm_2_avg_2_avg_mean <- adm_2_avg_2_avg %>% filter(intro_first_5 == TRUE)
  adm_2_avg_2_avg_up <- adm_2_avg_2_avg %>% filter(intro_first_5_up == TRUE)
  adm_2_avg_2_avg_low <- adm_2_avg_2_avg %>% filter(intro_first_5_low == TRUE)
  adm_2_avg_2_avg_mean <- left_join(adm_2_avg_2_avg_mean, adm_2_avg_2_avg_up[, c(1, 2)],
                                    by = c('adm_2' = 'adm_2'))
  adm_2_avg_2_avg_mean <- left_join(adm_2_avg_2_avg_mean, adm_2_avg_2_avg_low[, c(1, 2)],
                                    by = c('adm_2' = 'adm_2'))
  # Rename variables
  adm_2_avg_2_avg_mean <- adm_2_avg_2_avg_mean %>% rename('time_mean' = 'time.x',
                                                          'time_low' = 'time.y',
                                                          'time_high' = 'time')
  # Fill object
  adm_2_avg_2_avg_list[[i]] <- adm_2_avg_2_avg_mean
  
  # Aggregate to Admin 1
  # Create introduction times for all iterations
  adm_2_avg_1_run <- adm_2_results[[i]] %>% 
    group_by(time, adm_1, run_num) %>%
    mutate(incid_I_sum = sum(incid_I),
           time = time - 1) %>%
    distinct(time, run_num, adm_1, adm_level, incid_I_sum, .keep_all = FALSE) %>%
    ungroup() %>%
    group_by(adm_1, run_num) %>%
    mutate(intro_first_25 = incid_I_sum > 25 & !duplicated(incid_I_sum > 25)) %>%
    filter(intro_first_25 == TRUE) %>%
    ungroup() 
  # Fill object
  adm_2_avg_1_run_list[[i]] <- adm_2_avg_1_run
  
  # Create average introduction times across all iterations
  # With mean and 95% interval
  adm_2_avg_1_avg <- adm_2_results[[i]] %>% 
    group_by(time, adm_1, run_num) %>%
    mutate(incid_I_sum = sum(incid_I),
           time = time - 1) %>%
    distinct(time, run_num, adm_1, adm_level, incid_I_sum, .keep_all = FALSE) %>%
    ungroup() %>%
    group_by(time, adm_1) %>%
    mutate(incid_I_avg = mean(incid_I_sum),
           incid_I_up = quantile(incid_I_sum, probs = 0.975), 
           incid_I_low = quantile(incid_I_sum, probs = 0.025)) %>%
    distinct(time, adm_1, adm_level, incid_I_avg, incid_I_up, incid_I_low,
             .keep_all = FALSE) %>%
    ungroup() %>%
    group_by(adm_1) %>%
    mutate(intro_first_25 = incid_I_avg > 25 & !duplicated(incid_I_avg > 25),
           intro_first_25_up = incid_I_up > 25 & !duplicated(incid_I_up > 25),
           intro_first_25_low = incid_I_low > 25 & !duplicated(incid_I_low > 25))
  
  # Filter and merge mean and 95% thresholds together
  adm_2_avg_1_avg_mean <- adm_2_avg_1_avg %>% filter(intro_first_25 == TRUE)
  adm_2_avg_1_avg_up <- adm_2_avg_1_avg %>% filter(intro_first_25_up == TRUE)
  adm_2_avg_1_avg_low <- adm_2_avg_1_avg %>% filter(intro_first_25_low == TRUE)
  adm_2_avg_1_avg_mean <- left_join(adm_2_avg_1_avg_mean, adm_2_avg_1_avg_up[, c(1, 2)],
                                    by = c('adm_1' = 'adm_1'))
  adm_2_avg_1_avg_mean <- left_join(adm_2_avg_1_avg_mean, adm_2_avg_1_avg_low[, c(1, 2)],
                                    by = c('adm_1' = 'adm_1'))
  # Rename variables
  adm_2_avg_1_avg_mean <- adm_2_avg_1_avg_mean %>% rename('time_mean' = 'time.x',
                                                          'time_low' = 'time.y',
                                                          'time_high' = 'time')
  # Fill object
  adm_2_avg_1_avg_list[[i]] <- adm_2_avg_1_avg_mean
}

#########################################################
# 3. RUN METAPOPULATION MODEL AT ADMINISTRATIVE LEVEL 1 #
#########################################################

# Administrative Level 1
# Run 1000 iterations
# Assuming mobility
adm_1_mobile <- run_sir_model_multi(n = 1000, density_dep = FALSE, method = 'append',
                                    R_0 = 1.50, gamma = 1/10, sigma = 1/3, prop_s = 0.95, 
                                    adm_name_vec = adm_1_name_vec, adm_level = '1', 
                                    pop_vec = adm_1_pop_vec, intro_adm = 'COVID',
                                    adm_x_walk = adm_1_x_walk, travel_mat = adm_1_phone_mobility_mat, 
                                    max_time = 200, time_step = 1, mobility = TRUE)

# Assuming simulated mobility
adm_1_mobile_sim <- run_sir_model_multi(n = 1000, density_dep = FALSE, method = 'append',
                                    R_0 = 1.50, gamma = 1/10, sigma = 1/3, prop_s = 0.95, 
                                    adm_name_vec = adm_1_name_vec, adm_level = '1', 
                                    pop_vec = adm_1_pop_vec, intro_adm = 'COVID',
                                    adm_x_walk = adm_1_x_walk, travel_mat = adm_1_phone_pred_mobility_mat, 
                                    max_time = 200, time_step = 1, mobility = TRUE)

# Assuming no mobility
adm_1_none <- run_sir_model_multi(n = 1000, density_dep = FALSE, method = 'append',
                                  R_0 = 1.50, gamma = 1/10, sigma = 1/3, prop_s = 0.95, 
                                  adm_name_vec = adm_1_name_vec, adm_level = '1', 
                                  pop_vec = adm_1_pop_vec, intro_adm = 'COVID',
                                  adm_x_walk = adm_1_x_walk, travel_mat = adm_1_phone_mobility_mat, 
                                  max_time = 200, time_step = 1, mobility = FALSE)

# Create empty objects to fill
adm_1_avg_1_run_list <- NULL
adm_1_avg_1_avg_list <- NULL
adm_1_results <- list(adm_1_mobile, adm_1_mobile_sim, adm_1_none)
# Set counter
count <- 0

# Loop through mobility/no mobility results
for (i in c(1, 2, 3)) {
  # Increase counter
  count <- count + 1
  
  # Admin 1
  # Create introduction times for all iterations
  adm_1_avg_1_run <- adm_1_results[[i]] %>% 
    group_by(adm_1, run_num) %>%
    mutate(intro_first_25 = incid_I > 25 & !duplicated(incid_I > 25)) %>%
    filter(intro_first_25 == TRUE) %>%
    ungroup() 
  # Fill object
  adm_1_avg_1_run_list[[i]] <- adm_1_avg_1_run
  
  # Create average introduction times across all iterations
  # With mean and 95% interval
  adm_1_avg_1_avg <- adm_1_results[[i]] %>% 
    group_by(time, adm_1) %>%
    mutate(incid_I_avg = mean(incid_I),
           incid_I_up = quantile(incid_I, probs = 0.975), 
           incid_I_low = quantile(incid_I, probs = 0.025)) %>%
    distinct(time, adm_1, adm_level, incid_I_avg, incid_I_up, incid_I_low,
             .keep_all = FALSE) %>%
    ungroup() %>%
    group_by(adm_1) %>%
    mutate(intro_first_25 = incid_I_avg > 25 & !duplicated(incid_I_avg > 25),
           intro_first_25_up = incid_I_up > 25 & !duplicated(incid_I_up > 25),
           intro_first_25_low = incid_I_low > 25 & !duplicated(incid_I_low > 25))
  
  # Filter and merge mean and 95% thresholds together
  adm_1_avg_1_avg_mean <- adm_1_avg_1_avg %>% filter(intro_first_25 == TRUE)
  adm_1_avg_1_avg_up <- adm_1_avg_1_avg %>% filter(intro_first_25_up == TRUE)
  adm_1_avg_1_avg_low <- adm_1_avg_1_avg %>% filter(intro_first_25_low == TRUE)
  adm_1_avg_1_avg_mean <- left_join(adm_1_avg_1_avg_mean, adm_1_avg_1_avg_up[, c(1, 2)],
                                    by = c('adm_1' = 'adm_1'))
  adm_1_avg_1_avg_mean <- left_join(adm_1_avg_1_avg_mean, adm_1_avg_1_avg_low[, c(1, 2)],
                                    by = c('adm_1' = 'adm_1'))
  # Rename variables
  adm_1_avg_1_avg_mean <- adm_1_avg_1_avg_mean %>% rename('time_mean' = 'time.x',
                                                          'time_low' = 'time.y',
                                                          'time_high' = 'time')
  # Fill object
  adm_1_avg_1_avg_list[[i]] <- adm_1_avg_1_avg_mean

}

# Save results
save(list = c('adm_2_avg_2_run_list', 'adm_2_avg_2_avg_list', 'adm_2_avg_1_run_list', 
              'adm_2_avg_1_avg_list', 'adm_1_avg_1_run_list', 'adm_1_avg_1_avg_list'), 
     file = './tmp/covid_model_results.RData')

#######################################################
# 4. CREATE AND SAVE MODEL TIMESERIES AVERAGE RESULTS #
#######################################################

# Create average results for each spatial level, aggregation, and mobility 
# assumption
adm_1_mobile_avg <- adm_1_mobile %>% 
  group_by(adm_1, time) %>%
  mutate(avg_incid_i = mean(incid_I)) %>%
  distinct(adm_1, time, avg_incid_i) %>%
  ungroup() %>%
  filter(time <= 50)

adm_1_mobile_sim_avg <- adm_1_mobile_sim %>% 
  group_by(adm_1, time) %>%
  mutate(avg_incid_i = mean(incid_I)) %>%
  distinct(adm_1, time, avg_incid_i) %>%
  ungroup() %>%
  filter(time <= 50)

adm_1_none_avg <- adm_1_none %>% 
  group_by(adm_1, time) %>%
  mutate(avg_incid_i = mean(incid_I)) %>%
  distinct(adm_1, time, avg_incid_i) %>%
  ungroup() %>%
  filter(time <= 50)
  
adm_2_mobile_avg <- adm_2_mobile %>% 
  group_by(adm_2, time) %>%
  mutate(avg_incid_i = mean(incid_I)) %>%
  distinct(adm_2, adm_1, time, avg_incid_i) %>%
  ungroup() %>%
  filter(time <= 50)

adm_2_mobile_sim_avg <- adm_2_mobile_sim %>% 
  group_by(adm_2, time) %>%
  mutate(avg_incid_i = mean(incid_I)) %>%
  distinct(adm_2, adm_1, time, avg_incid_i) %>%
  ungroup() %>%
  filter(time <= 50)

adm_2_none_avg <- adm_2_none %>% 
  group_by(adm_2, time) %>%
  mutate(avg_incid_i = mean(incid_I)) %>%
  distinct(adm_2, adm_1, time, avg_incid_i) %>%
  ungroup() %>%
  filter(time <= 50)

adm_2_1_mobile_avg <- adm_2_mobile_avg %>% 
  group_by(adm_1, time) %>%
  mutate(sum_incid_i = sum(avg_incid_i)) %>%
  distinct(adm_1, time, sum_incid_i) %>%
  ungroup() %>%
  filter(time <= 50)

adm_2_1_mobile_sim_avg <- adm_2_mobile_sim_avg %>% 
  group_by(adm_1, time) %>%
  mutate(sum_incid_i = sum(avg_incid_i)) %>%
  distinct(adm_1, time, sum_incid_i) %>%
  ungroup() %>%
  filter(time <= 50)

adm_2_1_none_avg <- adm_2_none_avg %>% 
  group_by(adm_1, time) %>%
  mutate(sum_incid_i = sum(avg_incid_i)) %>%
  distinct(adm_1, time, sum_incid_i) %>%
  ungroup() %>%
  filter(time <= 50)

# Save results
save(list = c('adm_1_mobile_avg', 'adm_1_mobile_sim_avg', 'adm_1_none_avg', 
              'adm_2_mobile_avg', 'adm_2_mobile_sim_avg', 'adm_2_none_avg', 
              'adm_2_1_mobile_avg', 'adm_2_1_mobile_sim_avg', 'adm_2_1_none_avg'), 
     file = './tmp/covid_model_timeseries_results.RData')

################################################################################
################################################################################

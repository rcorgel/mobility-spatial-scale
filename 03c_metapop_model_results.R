################################################################################
# File Name: 02c_metapop_model_results                                         #
#                                                                              #
# Purpose:   Create a metapopulation model for different spatial scales.       #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create metapopulation model                                    #
#            3. Create multi-run model function                                #
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


load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# density = TRUE; R_0 = 2; gamma = 1/7; prop_s = 0.80; adm_name_vec = adm_2_name_vec
# adm_level = '2'; pop_vec = adm_2_pop_vec; intro_adm = 'Colombo'; adm_x_walk = adm_2_x_walk
# travel_mat = adm_2_day_avg_mat; max_time = 365; time_step = 1

adm_3 <- run_sir_model_multi(n = 50, density_dep = FALSE, method = 'average',
                             R_0 = 2, gamma = 1/7, prop_s = 0.80, 
                             adm_name_vec = adm_3_name_vec, adm_level = '3', 
                             pop_vec = adm_3_pop_vec, intro_adm = 'Colombo',
                             adm_x_walk = adm_3_x_walk, travel_mat = adm_3_day_avg_mat, 
                             max_time = 365, time_step = 1)


adm_3_total <- adm_3 %>% 
  group_by(time) %>% 
  mutate(sum_incid_I = sum(incid_I_avg)) %>%
  distinct(time, sum_incid_I, .keep_all = FALSE) %>%
  mutate(level = 3)
  

adm_2 <- run_sir_model_multi(n = 50, density_dep = FALSE, method = 'average',
                             R_0 = 2, gamma = 1/7, prop_s = 0.80, 
                             adm_name_vec = adm_2_name_vec, adm_level = '2', 
                             pop_vec = adm_2_pop_vec, intro_adm = 'Colombo',
                             adm_x_walk = adm_2_x_walk, travel_mat = adm_2_day_avg_mat, 
                             max_time = 365, time_step = 1)

adm_2_total <- adm_2 %>% 
  group_by(time) %>% 
  mutate(sum_incid_I = sum(incid_I_avg)) %>%
  distinct(time, sum_incid_I, .keep_all = FALSE) %>%
  mutate(level = 2)

adm_1 <- run_sir_model_multi(n = 50, density_dep = FALSE, method = 'average',
                             R_0 = 2, gamma = 1/7, prop_s = 0.80, 
                             adm_name_vec = adm_1_name_vec, adm_level = '1', 
                             pop_vec = adm_1_pop_vec, intro_adm = 'Colombo',
                             adm_x_walk = adm_1_x_walk, travel_mat = adm_1_day_avg_mat, 
                             max_time = 365, time_step = 1)

adm_1_total <- adm_1 %>% 
  group_by(time) %>% 
  mutate(sum_incid_I = sum(incid_I_avg)) %>%
  distinct(time, sum_incid_I, .keep_all = FALSE) %>%
  mutate(level = 1)

total <- rbind(adm_3_total, adm_2_total, adm_1_total)

ggplot(total) +
  geom_line(aes(x = time, y = sum_incid_I, color = as.character(level)))


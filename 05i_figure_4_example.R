################################################################################
# File Name: 05i_figure_4_example                                              #
#                                                                              #
# Purpose:   Create figure 4 for the manuscript.                               #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure functions                                        #
#            3. Simulate example epidemics                                     #
#            4. Create subfigures                                              #
#            5. Create final figure                                            #
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
library(cowplot)
library(RColorBrewer)
library(sf)
library(scales)
library(reshape2)
library(ggpubr)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load model 
source('./mobility-spatial-scale/04_metapop_model.R')

##############################
# 2. CREATE FIGURE FUNCTIONS #
##############################

# Plot functions
# Make a simple admin level map
make_simple_map <- function(data, coord_data) {
  map <- ggplot(data = data) +
    geom_sf(aes(), color= 'black', fill = 'white', linewidth = 0.20) +
    geom_point(data = coord_data, aes(x = long, y = lat), colour="white", fill = 'black', size=2.15, alpha = 1, shape=21) +
    geom_text(data = coord_data, aes(x = long - 0.10 , y = lat + 0.04 , label = city), size = 3.5, fontface = 'bold') +
    theme_void() + ggtitle(' ') + theme(legend.position = 'none',
                                        plot.title = element_text(size = 30, hjust = 0.5),
                                        legend.text = element_text(size = 24),
                                        legend.title = element_text(size = 24)) +
    coord_sf()
  return(map)
}

############################
# 3. CREATE MAP SUBFIGURES #
############################

# Add coordinates of highlight cities
coordinate_cities <- data.frame(
  city = c("Colombo", "Madhu"),
  lat = c(6.927632561772342, 8.85653415340985),
  long = c(79.85843709788357, 80.20433649099449))   

# Load the shape files
# From: https://data.humdata.org/dataset/cod-ab-mdg
choropleth_1 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm1_slsd_20220816')
choropleth_2 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm2_slsd_20220816')
choropleth_3 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm3_slsd_20220816')

# Load mobility to shape cross walk
# The mobility data combines multiple admin 3 units, changing the total from 339 to 330
mobility_shape_xwalk <- readRDS('./tmp/mobility_shape_xwalk.rds')

# Merge on the cross walk
choropleth_3 <- left_join(choropleth_3, mobility_shape_xwalk, by = c('ADM3_EN' = 'adm_3_shape'))

# Join polygons to create 330 mobility admin 3 units
choropleth_3_mobility <- choropleth_3 %>% 
  group_by(adm_3_mobility) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# Main map
map <- ggplot() +
  geom_sf(data = choropleth_1, aes(fill = ADM1_EN, group = ADM1_EN), color= 'black', linewidth = 1.3, alpha = 0.85) +
  geom_sf(data = choropleth_2, aes(group = ADM2_EN, linewidth = as.factor(1.3)), color= 'black', fill = '#FFFFFF00') +
  geom_sf(data = choropleth_3, aes(group = ADM3_EN, linewidth = as.factor(0.30)), color= 'black', fill = '#FFFFFF00') +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 5, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.12 , y = lat + 0.11 , label = city), size = 9, fontface = 'bold', alpha = 0.85) +
  theme_void() + ggtitle('Provinces, Districts, and Divisions of Sri Lanka') + theme(legend.position = "inside", legend.position.inside = c(0.99, 0.78),
                                                                                          plot.title = element_text(size = 34, hjust = 0.5),                                                                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                                          legend.title = element_text(size = 26),
                                                                                          legend.text = element_text(size = 24),
                                                                                          legend.key.size = unit(1.1, 'cm')) + 
  labs(fill = 'Provinces') + scale_fill_manual(values = c("#E6F598", "#ABDDA4", "#66C2A5",
                                                          "#5E4FA2", "#3288BD",  "#D53E4F", "#F46D43", "#FDAE61", "#9E0142")) +
  scale_linewidth_manual(values = c(1.3, 0.30), labels = c('Districts', 'Divisions'), name = '')


ggsave('./figs/geographic_map.jpg', plot = map, height = 16, width = 13)

#########################################################
# 2. RUN METAPOPULATION MODEL AT ADMINISTRATIVE LEVEL 3 #
#########################################################

# First, load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Load simulated mobility data
load('./mobility-spatial-scale/simulated data/mobile_phone_sim_prop_dat.RData')

# Create object for mobility data
mobility_dat_adm_3 <- list(adm_3_phone_mobility_mat, adm_3_phone_pred_mobility_mat)


adm_3_obs_col <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                             R_0 = 1.8, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                             adm_name_vec = adm_3_name_vec, adm_level = '3', 
                             pop_vec = adm_3_pop_vec, intro_adm = 'Colombo', intro_num = 1,
                             adm_x_walk = adm_3_x_walk, travel_mat = mobility_dat_adm_3[[1]], 
                             max_time = 365, time_step = 1, mobility = TRUE)

adm_3_at_2_obs_col <- adm_3_obs_col %>% group_by(time, adm_2, run_num) %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Colombo') %>% dplyr::select(time, adm_2, magnitude, intro_time)

adm_3_at_1_obs_col <- adm_3_obs_col %>% group_by(time, adm_1, run_num) %>%
  # Calculate incidence at Admin 2
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
  mutate(intro_time = mean(time),
         intro_loc = 'Colombo') %>% dplyr::select(time, adm_1, magnitude, intro_time)

adm_3_obs_mad <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                             R_0 = 1.8, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                             adm_name_vec = adm_3_name_vec, adm_level = '3', 
                             pop_vec = adm_3_pop_vec, intro_adm = 'Madhu', intro_num = 1,
                             adm_x_walk = adm_3_x_walk, travel_mat = mobility_dat_adm_3[[1]], 
                             max_time = 365, time_step = 1, mobility = TRUE)

adm_3_at_2_obs_mad <- adm_3_obs_mad %>% group_by(time, adm_2, run_num) %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Madhu') %>% dplyr::select(time, adm_2, magnitude, intro_time)

adm_3_at_1_obs_mad <- adm_3_obs_mad %>% group_by(time, adm_1, run_num) %>%
  # Calculate incidence at Admin 2
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
  mutate(intro_time = mean(time),
         intro_loc = 'Madhu') %>% dplyr::select(time, adm_1, magnitude, intro_time)

adm_3_sim_col <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                     R_0 = 1.5, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                     adm_name_vec = adm_3_name_vec, adm_level = '3', 
                                     pop_vec = adm_3_pop_vec, intro_adm = 'Colombo', intro_num = 1,
                                     adm_x_walk = adm_3_x_walk, travel_mat = mobility_dat_adm_3[[2]], 
                                     max_time = 365, time_step = 1, mobility = TRUE)

adm_3_at_2_sim_col <- adm_3_sim_col %>% group_by(time, adm_2, run_num) %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Colombo') %>% dplyr::select(time, adm_2, magnitude, intro_time)

adm_3_at_1_sim_col <- adm_3_sim_col %>% group_by(time, adm_1, run_num) %>%
  # Calculate incidence at Admin 2
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
  mutate(intro_time = mean(time),
         intro_loc = 'Colombo') %>% dplyr::select(time, adm_1, magnitude, intro_time)

adm_3_sim_mad <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                     R_0 = 1.5, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                     adm_name_vec = adm_3_name_vec, adm_level = '3', 
                                     pop_vec = adm_3_pop_vec, intro_adm = 'Madhu', intro_num = 1,
                                     adm_x_walk = adm_3_x_walk, travel_mat = mobility_dat_adm_3[[2]], 
                                     max_time = 365, time_step = 1, mobility = TRUE)

adm_3_at_2_sim_mad <- adm_3_sim_mad %>% group_by(time, adm_2, run_num) %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Madhu') %>% dplyr::select(time, adm_2, magnitude, intro_time)

adm_3_at_1_sim_mad <- adm_3_sim_mad %>% group_by(time, adm_1, run_num) %>%
  # Calculate incidence at Admin 2
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
  mutate(intro_time = mean(time),
         intro_loc = 'Madhu') %>% dplyr::select(time, adm_1, magnitude, intro_time)

# ADMIN 2
mobility_dat_adm_2 <- list(adm_2_phone_mobility_mat, adm_2_phone_pred_mobility_mat)


adm_2_obs_col <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                     R_0 = 1.8, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                     adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                     pop_vec = adm_2_pop_vec, intro_adm = 'Colombo', intro_num = 1,
                                     adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_2[[1]], 
                                     max_time = 365, time_step = 1, mobility = TRUE)

adm_2_at_2_obs_col <- adm_2_obs_col %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Colombo') %>%
  dplyr::select(time, adm_2, magnitude, intro_time)

adm_2_at_1_obs_col <- adm_2_obs_col %>% group_by(time, adm_1, run_num) %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Colombo') %>% dplyr::select(time, adm_1, magnitude, intro_time)

adm_2_obs_mad <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                     R_0 = 1.8, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                     adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                     pop_vec = adm_2_pop_vec, intro_adm = 'Madhu', intro_num = 1,
                                     adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_2[[1]], 
                                     max_time = 365, time_step = 1, mobility = TRUE)

adm_2_at_2_obs_mad <- adm_2_obs_mad %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Madhu') %>%
  dplyr::select(time, adm_2, magnitude, intro_time)

adm_2_at_1_obs_mad <- adm_2_obs_mad %>% group_by(time, adm_1, run_num) %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Madhu') %>% dplyr::select(time, adm_1, magnitude, intro_time)

adm_2_sim_col <- run_seir_model_multi(n = 50, density_dep = FALSE, method = 'append',
                                     R_0 = 1.5, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                     adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                     pop_vec = adm_2_pop_vec, intro_adm = 'Colombo', intro_num = 1,
                                     adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_2[[2]], 
                                     max_time = 365, time_step = 1, mobility = TRUE)

adm_2_at_2_sim_col <- adm_2_sim_col %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Colombo') %>%
  dplyr::select(time, adm_2, magnitude, intro_time)

adm_2_at_1_sim_col <- adm_2_sim_col %>% group_by(time, adm_1, run_num) %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Colombo') %>% dplyr::select(time, adm_1, magnitude, intro_time)

adm_2_sim_mad <- run_seir_model_multi(n = 50, density_dep = FALSE, method = 'append',
                                     R_0 = 1.5, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                     adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                     pop_vec = adm_2_pop_vec, intro_adm = 'Madhu', intro_num = 1,
                                     adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_2[[2]], 
                                     max_time = 365, time_step = 1, mobility = TRUE)

adm_2_at_2_sim_mad <- adm_2_sim_mad %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Madhu') %>%
  dplyr::select(time, adm_2, magnitude, intro_time)

adm_2_at_1_sim_mad <- adm_2_sim_mad %>% group_by(time, adm_1, run_num) %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Madhu') %>% dplyr::select(time, adm_1, magnitude, intro_time)

# ADMIN 1
mobility_dat_adm_1 <- list(adm_1_phone_mobility_mat, adm_1_phone_pred_mobility_mat)


adm_1_obs_col <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                     R_0 = 1.8, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                     adm_name_vec = adm_1_name_vec, adm_level = '1', 
                                     pop_vec = adm_1_pop_vec, intro_adm = 'Colombo', intro_num = 1,
                                     adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_1[[1]], 
                                     max_time = 365, time_step = 1, mobility = TRUE)

adm_1_at_1_obs_col <- adm_1_obs_col %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Colombo') %>%
  dplyr::select(time, adm_1, magnitude, intro_time)

adm_1_obs_mad <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                     R_0 = 1.8, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                     adm_name_vec = adm_1_name_vec, adm_level = '1', 
                                     pop_vec = adm_1_pop_vec, intro_adm = 'Madhu', intro_num = 1,
                                     adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_1[[1]], 
                                     max_time = 365, time_step = 1, mobility = TRUE)

adm_1_at_1_obs_mad <- adm_1_obs_mad %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Madhu') %>%
  dplyr::select(time, adm_1, magnitude, intro_time)

adm_1_sim_col <- run_seir_model_multi(n = 50, density_dep = FALSE, method = 'append',
                                     R_0 = 1.5, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                     adm_name_vec = adm_1_name_vec, adm_level = '1', 
                                     pop_vec = adm_1_pop_vec, intro_adm = 'Colombo', intro_num = 1,
                                     adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_1[[2]], 
                                     max_time = 365, time_step = 1, mobility = TRUE)

adm_1_at_1_sim_col <- adm_1_sim_col %>%
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
  mutate(intro_time = mean(time),
         intro_loc = 'Colombo') %>%
  dplyr::select(time, adm_1, magnitude, intro_time)

adm_1_sim_mad <- run_seir_model_multi(n = 50, density_dep = FALSE, method = 'append',
                                     R_0 = 1.5, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                     adm_name_vec = adm_1_name_vec, adm_level = '1', 
                                     pop_vec = adm_1_pop_vec, intro_adm = 'Madhu', intro_num = 1,
                                     adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_1[[2]], 
                                     max_time = 365, time_step = 1, mobility = TRUE)

adm_1_at_1_sim_mad <- adm_1_sim_mad %>%
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
  # Filter to first instance for all Admin 2
  filter(intro_first == TRUE) %>%
  ungroup() %>%
  arrange(time) %>%
  mutate(intro_time = mean(time),
         intro_loc = 'Madhu') %>%
  dplyr::select(time, adm_1, magnitude, intro_time)










# 3_2

obs_col <- left_join(adm_3_at_2_obs_col, adm_2_at_2_obs_col, by = c('adm_2' = 'adm_2'))
obs_col$time_diff <- obs_col$time.y - obs_col$time.x
  
obs_mad <- left_join(adm_3_at_2_obs_mad, adm_2_at_2_obs_mad, by = c('adm_2' = 'adm_2'))
obs_mad$time_diff <- obs_mad$time.y - obs_mad$time.x

sim_col <- left_join(adm_3_at_2_sim_col, adm_2_at_2_sim_col, by = c('adm_2' = 'adm_2'))
sim_col$time_diff <- sim_col$time.y - sim_col$time.x

sim_mad <- left_join(adm_3_at_2_sim_mad, adm_2_at_2_sim_mad, by = c('adm_2' = 'adm_2'))
sim_mad$time_diff <- sim_mad$time.y - sim_mad$time.x

# 3_1

obs_col_1 <- left_join(adm_3_at_1_obs_col, adm_1_at_1_obs_col, by = c('adm_1' = 'adm_1'))
obs_col_1$time_diff <- obs_col_1$time.y - obs_col_1$time.x

obs_mad_1 <- left_join(adm_3_at_1_obs_mad, adm_1_at_1_obs_mad, by = c('adm_1' = 'adm_1'))
obs_mad_1$time_diff <- obs_mad_1$time.y - obs_mad_1$time.x

sim_col_1 <- left_join(adm_3_at_1_sim_col, adm_1_at_1_sim_col, by = c('adm_1' = 'adm_1'))
sim_col_1$time_diff <- sim_col_1$time.y - sim_col_1$time.x

sim_mad_1 <- left_join(adm_3_at_1_sim_mad, adm_1_at_1_sim_mad, by = c('adm_1' = 'adm_1'))
sim_mad_1$time_diff <- sim_mad_1$time.y - sim_mad_1$time.x

# 2_1
obs_col_2 <- left_join(adm_2_at_1_obs_col, adm_1_at_1_obs_col, by = c('adm_1' = 'adm_1'))
obs_col_2$time_diff <- obs_col_2$time.y - obs_col_2$time.x

obs_mad_2 <- left_join(adm_2_at_1_obs_mad, adm_1_at_1_obs_mad, by = c('adm_1' = 'adm_1'))
obs_mad_2$time_diff <- obs_mad_2$time.y - obs_mad_2$time.x

sim_col_2 <- left_join(adm_2_at_1_sim_col, adm_1_at_1_sim_col, by = c('adm_1' = 'adm_1'))
sim_col_2$time_diff <- sim_col_2$time.y - sim_col_2$time.x

sim_mad_2 <- left_join(adm_2_at_1_sim_mad, adm_1_at_1_sim_mad, by = c('adm_1' = 'adm_1'))
sim_mad_2$time_diff <- sim_mad_2$time.y - sim_mad_2$time.x






choropleth_2_obs_col <- left_join(choropleth_2, obs_col, by = c('ADM2_EN' = 'adm_2'))
choropleth_2_obs_mad <- left_join(choropleth_2, obs_mad, by = c('ADM2_EN' = 'adm_2'))
choropleth_2_sim_col <- left_join(choropleth_2, sim_col, by = c('ADM2_EN' = 'adm_2'))
choropleth_2_sim_mad <- left_join(choropleth_2, sim_mad, by = c('ADM2_EN' = 'adm_2'))

choropleth_2_obs_col$time_diff[1] <- NA
choropleth_2_sim_col$time_diff[1] <- NA
choropleth_2_obs_mad$time_diff[11] <- NA
choropleth_2_sim_mad$time_diff[11] <- NA





choropleth_1_obs_col_1 <- left_join(choropleth_1, obs_col_1, by = c('ADM1_EN' = 'adm_1'))
choropleth_1_obs_mad_1 <- left_join(choropleth_1, obs_mad_1, by = c('ADM1_EN' = 'adm_1'))
choropleth_1_sim_col_1 <- left_join(choropleth_1, sim_col_1, by = c('ADM1_EN' = 'adm_1'))
choropleth_1_sim_mad_1 <- left_join(choropleth_1, sim_mad_1, by = c('ADM1_EN' = 'adm_1'))

choropleth_1_obs_col_1$time_diff[9] <- NA
choropleth_1_sim_col_1$time_diff[9] <- NA
choropleth_1_obs_mad_1$time_diff[5] <- NA
choropleth_1_sim_mad_1$time_diff[5] <- NA


choropleth_1_obs_col_2 <- left_join(choropleth_1, obs_col_2, by = c('ADM1_EN' = 'adm_1'))
choropleth_1_obs_mad_2 <- left_join(choropleth_1, obs_mad_2, by = c('ADM1_EN' = 'adm_1'))
choropleth_1_sim_col_2 <- left_join(choropleth_1, sim_col_2, by = c('ADM1_EN' = 'adm_1'))
choropleth_1_sim_mad_2 <- left_join(choropleth_1, sim_mad_2, by = c('ADM1_EN' = 'adm_1'))

choropleth_1_obs_col_2$time_diff[9] <- NA
choropleth_1_sim_col_2$time_diff[9] <- NA
choropleth_1_obs_mad_2$time_diff[5] <- NA
choropleth_1_sim_mad_2$time_diff[5] <- NA




map_1 <- ggplot() +
  geom_sf(data = choropleth_2_obs_col, aes(fill = time_diff), color= 'black', linewidth = 0.50) +
  geom_sf(data = choropleth_1, aes(group = ADM1_EN), fill = '#FFFFFF00', color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('\n\nColombo Introduction') + theme(legend.position = 'none',
                                                                                          plot.title = element_text(size = 30, hjust = 0.5),
                                                                                          panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                                          legend.title = element_text(size = 24),
                                                                                          legend.text = element_text(size = 24),
                                                                                          legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#41AE76",
    mid = "white",
    high = "#807DBA",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-80, 40))

map_2 <- ggplot() +
  geom_sf(data = choropleth_2_sim_col, aes(fill = time_diff), color= 'black', linewidth = 0.50) +
  geom_sf(data = choropleth_1, aes(group = ADM1_EN), fill = '#FFFFFF00', color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('\n\nColombo Introduction') + theme(legend.position = 'none',
                                                                        plot.title = element_text(size = 30, hjust = 0.5),
                                                                        panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                        legend.title = element_text(size = 24),
                                                                        legend.text = element_text(size = 24),
                                                                        legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#41AE76",
    mid = "white",
    high = "#807DBA",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-80, 40))

map_3 <- ggplot() +
  geom_sf(data = choropleth_2_obs_mad, aes(fill = time_diff), color= 'black', linewidth = 0.50) +
  geom_sf(data = choropleth_1, aes(group = ADM1_EN), fill = '#FFFFFF00', color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('\n\nMadhu Introduction') + theme(legend.position = 'none',
                                                                        plot.title = element_text(size = 30, hjust = 0.5),
                                                                        panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                        legend.title = element_text(size = 24),
                                                                        legend.text = element_text(size = 24),
                                                                        legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#41AE76",
    mid = "white",
    high = "#807DBA",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-80, 40))

map_4 <- ggplot() +
  geom_sf(data = choropleth_2_sim_mad, aes(fill = time_diff), color= 'black', linewidth = 0.50) +
  geom_sf(data = choropleth_1, aes(group = ADM1_EN), fill = '#FFFFFF00', color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('\n\nMadhu Introduction') + theme(legend.position = 'none',
                                                                        plot.title = element_text(size = 30, hjust = 0.5),
                                                                        panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                        legend.title = element_text(size = 24),
                                                                        legend.text = element_text(size = 24),
                                                                        legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#41AE76",
    mid = "white",
    high = "#807DBA",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-80, 40))

legend <- ggplot() +
  geom_sf(data = choropleth_2_sim_mad, aes(fill = time_diff), color= 'black', linewidth = 0.50) +
  geom_sf(data = choropleth_1, aes(group = ADM1_EN), fill = '#FFFFFF00', color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('\n\nMadhu Introduction') + theme(legend.position = 'bottom',
                                                                       plot.title = element_text(size = 28, hjust = 0.5),
                                                                       panel.border = element_rect(fill=NA, linewidth = 0.6, color = 'white'),
                                                                       legend.title = element_text(size = 22),
                                                                       legend.text = element_text(size = 22),
                                                                       legend.key.size = unit(1, 'cm')) + 
  labs(fill = 'Spatial Invasion    \nDifference (days)') + scale_fill_gradient2(
    low = "#41AE76",
    mid = "white",
    high = "#807DBA",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-80, 40)) +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 2))

legend_get <- get_legend(legend)

figure_3 <- plot_grid(map_1, map_2, map_3, map_4,
                      nrow = 2, 
                      rel_heights = c(1, 1),
                      labels = c('(a)', '(b)', '(c)', '(d)'), hjust = 0,
                      label_size = 34)

figure_3.5 <- plot_grid(figure_3, legend_get,
                      nrow = 2, 
                      rel_heights = c(1, 0.12))


ggsave('./figs/figure_3.jpg', plot = figure_3.5, height = 28, width = 25)



map_1_3_1 <- ggplot() +
  geom_sf(data = choropleth_1_obs_col_1, aes(fill = time_diff), color= 'black', linewidth = 1.50) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('\n\nColombo Introduction') + theme(legend.position = 'none',
                                                                        plot.title = element_text(size = 30, hjust = 0.5),
                                                                        panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                        legend.title = element_text(size = 24),
                                                                        legend.text = element_text(size = 24),
                                                                        legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#807DBA",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-50, 80))

map_3_3_1 <- ggplot() +
  geom_sf(data = choropleth_1_obs_mad_1, aes(fill = time_diff), color= 'black', linewidth = 1.50) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('\n\nMadhu Introduction') + theme(legend.position = 'none',
                                                                      plot.title = element_text(size = 30, hjust = 0.5),
                                                                      panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                      legend.title = element_text(size = 24),
                                                                      legend.text = element_text(size = 24),
                                                                      legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#807DBA",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-80, 70))

legend_1 <- ggplot() +
  geom_sf(data = choropleth_1_sim_mad_1, aes(fill = time_diff), color= 'black', linewidth = 0.50) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('\n\nMadhu Introduction') + theme(legend.position = 'bottom',
                                                                       plot.title = element_text(size = 28, hjust = 0.5),
                                                                       panel.border = element_rect(fill=NA, linewidth = 0.6, color = 'white'),
                                                                       legend.title = element_text(size = 22),
                                                                       legend.text = element_text(size = 22),
                                                                       legend.key.size = unit(1, 'cm')) + 
  labs(fill = 'Spatial Invasion    \nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#807DBA",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-50, 80)) +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 2))

legend_get_1 <- get_legend(legend_1)



map_1_2_1 <- ggplot() +
  geom_sf(data = choropleth_1_obs_col_2, aes(fill = time_diff), color= 'black', linewidth = 1.50) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('\n\nColombo Introduction') + theme(legend.position = 'none',
                                                                        plot.title = element_text(size = 30, hjust = 0.5),
                                                                        panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                        legend.title = element_text(size = 24),
                                                                        legend.text = element_text(size = 24),
                                                                        legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#41AE76",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-10, 30))

map_3_2_1 <- ggplot() +
  geom_sf(data = choropleth_1_obs_mad_2, aes(fill = time_diff), color= 'black', linewidth = 1.50) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('\n\nMadhu Introduction') + theme(legend.position = 'none',
                                                                      plot.title = element_text(size = 30, hjust = 0.5),
                                                                      panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                      legend.title = element_text(size = 24),
                                                                      legend.text = element_text(size = 24),
                                                                      legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#41AE76",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-10, 30))

legend_2 <- ggplot() +
  geom_sf(data = choropleth_1_sim_mad_2, aes(fill = time_diff), color= 'black', linewidth = 0.50) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('\n\nMadhu Introduction') + theme(legend.position = 'bottom',
                                                                       plot.title = element_text(size = 28, hjust = 0.5),
                                                                       panel.border = element_rect(fill=NA, linewidth = 0.6, color = 'white'),
                                                                       legend.title = element_text(size = 22),
                                                                       legend.text = element_text(size = 22),
                                                                       legend.key.size = unit(1, 'cm')) + 
  labs(fill = 'Spatial Invasion    \nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = '#41AE76',
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-10, 30)) +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 2))

legend_get_2 <- get_legend(legend_2)

figure_1 <- cowplot::plot_grid(map_1, map_3, legend_get, nrow = 3, rel_heights = c(1, 1, 0.15))
figure_2 <- plot_grid(map_1_3_1, map_3_3_1, legend_get_1, nrow = 3, rel_heights = c(1, 1, 0.15))
figure_3 <- plot_grid(map_1_2_1, map_3_2_1, legend_get_2, nrow = 3, rel_heights = c(1, 1, 0.15))


figure_3_ <- plot_grid(figure_3, figure_2, figure_1,
                      nrow = 1, 
                      rel_heights = c(1, 1, 1),
                      labels = c('(a) Province - District', '(b) Province - Division', '(c) District - Division'), hjust = 0,
                      label_size = 34)


ggsave('./figs/figure_3_new.jpg', plot = figure_3_, height = 30, width = 25)



figure_3.5 <- plot_grid(figure_3, legend_get,
                        nrow = 2, 
                        rel_heights = c(1, 0.12))


take_off <- adm_3_obs_col |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> distinct(run_num, sum)

ggplot(take_off) + geom_density(aes(sum))

take_off <- adm_3_obs_mad |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> distinct(run_num, sum)

ggplot(take_off) + geom_density(aes(sum))


adm_3_obs_col_avg <- adm_3_obs_col %>%
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(time, adm_3) %>%
  mutate(avg_incid_I_adm_3 = mean(incid_I)) %>%
  distinct(time, adm_3, avg_incid_I_adm_3) %>% 
  ungroup() |>
  group_by(time) |> 
  mutate(avg_incid_I = sum(avg_incid_I_adm_3)) |>
  distinct(time, avg_incid_I) |> ungroup() |>
  mutate(cum_sum_I = cumsum(avg_incid_I)) |>
  mutate(Scale = 'Division')




adm_1_obs_col_avg <- adm_1_obs_col %>%
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(time, adm_1) %>%
  mutate(avg_incid_I_adm_1 = mean(incid_I)) %>%
  distinct(time, adm_1, avg_incid_I_adm_1) %>% 
  ungroup() |>
  group_by(time) |> 
  mutate(avg_incid_I = sum(avg_incid_I_adm_1)) %>%
  distinct(time, avg_incid_I) |> ungroup() |>
  mutate(cum_sum_I = cumsum(avg_incid_I)) |>
  mutate(Scale = 'Province')

line <- rbind(adm_3_obs_col_avg, adm_1_obs_col_avg)


ggplot(data = line) + geom_line(aes(x = time, y = cum_sum_I, color = Scale)) 


adm_3_obs_col_int <- adm_3_obs_col |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Calculate average across simulations
  group_by(time, adm_3) |>
  mutate(avg_incid_I_adm_3 = mean(incid_I)) |>
  distinct(time, adm_3, adm_1, avg_incid_I_adm_3) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(time, adm_1) |> 
  mutate(avg_incid_I_adm_1 = sum(avg_incid_I_adm_3)) |>
  distinct(time, adm_1, avg_incid_I_adm_1) |> 
  ungroup() |>
  group_by(adm_1) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(avg_incid_I_adm_1),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(time) |>
  mutate(intro_loc = 'Col',
         Scale = 'Division',
         Count = seq(1, 9, 1)) |>
  dplyr::select(time, adm_1, time, Count, Scale)
  

#ggplot(data = adm_3_obs_col_int) + geom_line(aes(x = time, y = Count)) 


adm_1_obs_col_int <- adm_1_obs_col |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Calculate average across simulations
  group_by(time, adm_1) |>
  mutate(avg_incid_I_adm_1 = mean(incid_I)) |>
  distinct(time, adm_1, avg_incid_I_adm_1) |>
  ungroup() |>
  group_by(adm_1) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(avg_incid_I_adm_1),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(time) |>
  mutate(intro_loc = 'Col',
         Scale = 'Province',
         Count = seq(1, 9, 1)) |>
  dplyr::select(time, adm_1, time, Count, Scale)

test_1 <- rbind(adm_1_obs_col_int, adm_3_obs_col_int)

#ggplot(data = test) + geom_line(aes(x = time, y = Count, color = Scale)) 







adm_3_obs_mad_int <- adm_3_obs_mad |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Calculate average across simulations
  group_by(time, adm_3) |>
  mutate(avg_incid_I_adm_3 = mean(incid_I)) |>
  distinct(time, adm_3, adm_1, avg_incid_I_adm_3) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(time, adm_1) |> 
  mutate(avg_incid_I_adm_1 = sum(avg_incid_I_adm_3)) |>
  distinct(time, adm_1, avg_incid_I_adm_1) |> 
  ungroup() |>
  group_by(adm_1) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(avg_incid_I_adm_1),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(time) |>
  mutate(intro_loc = 'Col',
         Scale = 'Division',
         Count = seq(1, 9, 1)) |>
  dplyr::select(time, adm_1, time, Count, Scale)


#ggplot(data = adm_3_obs_col_int) + geom_line(aes(x = time, y = Count)) 


adm_1_obs_mad_int <- adm_1_obs_mad |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Calculate average across simulations
  group_by(time, adm_1) |>
  mutate(avg_incid_I_adm_1 = mean(incid_I)) |>
  distinct(time, adm_1, avg_incid_I_adm_1) |>
  ungroup() |>
  group_by(adm_1) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(avg_incid_I_adm_1),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(time) |>
  mutate(intro_loc = 'Col',
         Scale = 'Province',
         Count = seq(1, 9, 1)) |>
  dplyr::select(time, adm_1, time, Count, Scale)

test <- rbind(adm_1_obs_mad_int, adm_3_obs_mad_int)

line_plot_mad <- ggplot(data = test) + geom_line(aes(x = time, y = Count, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Count') + xlab('Time (days)') + ggtitle('Units Infected') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_y_continuous(breaks=c(0, 3, 6, 9)) +
  scale_color_manual(values = c("#9e9ac8", "#4292C6"))


line_plot_col <- ggplot(data = test_1) + geom_line(aes(x = time, y = Count, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Count') + xlab('Time (days)') + ggtitle('Units Infected') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_y_continuous(breaks=c(0, 3, 6, 9)) +
  scale_color_manual(values = c("#9e9ac8", "#4292C6"))


dis_plot_col <- ggplot(data = line) + geom_line(aes(x = time, y = avg_incid_I, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Disease Curves') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_color_manual(values = c("#9e9ac8", "#4292C6"))


dis_plot_mad <- ggplot(data = line_2) + geom_line(aes(x = time, y = avg_incid_I, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Disease Curves') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_color_manual(values = c("#9e9ac8", "#4292C6"))


#load('./tmp/introduction_location_model_results.RData')




adm_3_obs_mad_avg <- adm_3_obs_mad %>%
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(time, adm_3) %>%
  mutate(avg_incid_I_adm_3 = mean(incid_I)) %>%
  distinct(time, adm_3, avg_incid_I_adm_3) %>% 
  ungroup() |>
  group_by(time) |> 
  mutate(avg_incid_I = sum(avg_incid_I_adm_3)) |>
  distinct(time, avg_incid_I) |> ungroup() |>
  mutate(cum_sum_I = cumsum(avg_incid_I)) |>
  mutate(Scale = 'Division')


adm_1_obs_mad_avg <- adm_1_obs_mad %>%
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(time, adm_1) %>%
  mutate(avg_incid_I_adm_1 = mean(incid_I)) %>%
  distinct(time, adm_1, avg_incid_I_adm_1) %>% 
  ungroup() |>
  group_by(time) |> 
  mutate(avg_incid_I = sum(avg_incid_I_adm_1)) %>%
  distinct(time, avg_incid_I) |> ungroup() |>
  mutate(cum_sum_I = cumsum(avg_incid_I)) |>
  mutate(Scale = 'Province')

line_2 <- rbind(adm_3_obs_mad_avg, adm_1_obs_mad_avg)


ggplot(data = line_2) + geom_line(aes(x = time, y = cum_sum_I, color = Scale))




adm_1_obs_mad_int <- adm_1_obs_mad_int |> rename('time_1' = 'time')

join_mad <- left_join(adm_3_obs_mad_int, adm_1_obs_mad_int, by = c('adm_1' = 'adm_1'))
join_mad$diff <- join_mad$time_1 - join_mad$time


adm_1_obs_col_int <- adm_1_obs_col_int |> rename('time_1' = 'time')

join_col <- left_join(adm_3_obs_col_int, adm_1_obs_col_int, by = c('adm_1' = 'adm_1'))
join_col$diff <- join_col$time_1 - join_col$time

choropleth_1_obs_mad_1 <- left_join(choropleth_1, join_mad, by = c('ADM1_EN' = 'adm_1'))

choropleth_1_obs_col_1 <- left_join(choropleth_1, join_col, by = c('ADM1_EN' = 'adm_1'))


map_1_3_col <- ggplot() +
  geom_sf(data = choropleth_1_obs_col_1, aes(fill = diff), color= 'black', linewidth = 1.50) +
  geom_point(data = coordinate_cities[1,], aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities[1,], aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold', alpha = 0.85) +
  theme_void() + ggtitle('Invasion Time Difference') + theme(legend.position = 'bottom',
                                                             plot.title = element_text(size = 34, hjust = 0.5),
                                                             panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                             legend.title = element_text(size = 30),
                                                             legend.text = element_text(size = 30),
                                                             legend.key.width = unit(2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#807DBA",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-25, 25))


map_1_3_mad <- ggplot() +
  geom_sf(data = choropleth_1_obs_mad_1, aes(fill = diff), color= 'black', linewidth = 1.50) +
  geom_point(data = coordinate_cities[2,], aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities[2,], aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold', alpha = 0.85) +
  theme_void() + ggtitle('Invasion Time Difference') + theme(legend.position = 'bottom',
                                                             plot.title = element_text(size = 34, hjust = 0.5),
                                                             panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                             legend.title = element_text(size = 30),
                                                             legend.text = element_text(size = 30),
                                                             legend.key.width = unit(2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#807DBA",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-25, 25))




mad.5 <- cowplot::plot_grid(line_plot_mad, ggplot() + theme_void(), dis_plot_mad, nrow = 3, rel_heights = c(1, 0.10, 1))
mad <- cowplot::plot_grid(map_1_3_mad, mad.5, nrow = 1, rel_widths = c(1, 0.8))
ggsave('./figs/pres_mad_plot_dis.jpg', plot = mad, height = 15, width = 16)


col.5 <- cowplot::plot_grid(line_plot_col, ggplot() + theme_void(), dis_plot_col, nrow = 3, rel_heights = c(1, 0.10, 1))
col <- cowplot::plot_grid(map_1_3_col, col.5, nrow = 1, rel_widths = c(1, 0.8))
ggsave('./figs/pres_col_plot_dis.jpg', plot = col, height = 15, width = 16)

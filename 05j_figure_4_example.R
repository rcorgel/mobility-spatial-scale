################################################################################
# File Name: 05i_figure_4_example                                              #
#                                                                              #
# Purpose:   Create figure 4 for the manuscript.                               #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Simulate example epidemics                                     #
#            3. Create subfigures                                              #
#            4. Create final figure                                            #
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
library(forcats)

# Set the seed
set.seed(123456)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load model 
source('./mobility-spatial-scale/04_metapop_model.R')

#load('./tmp/rescale_phone_mobility_dat.RData')

#################################
# 2. SIMULATE EXAMPLE EPIDEMICS #
#################################

# First, load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Load simulated mobility data
#load('./mobility-spatial-scale/simulated data/mobile_phone_sim_prop_dat.RData')

##########################
# Administrative Level 3 #
##########################

# Create object for mobility data
#mobility_dat_adm_3 <- list(as.matrix(adm_3_phone_mobility_mat_rescale_adm_1), adm_3_phone_pred_mobility_mat)

adm_3_col <- mclapply(1:150, run_seir_model, beta = 0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 41,
                      adm_x_walk = adm_3_x_walk, travel_mat = adm_3_phone_mobility_mat,
                      max_time = 365, time_step = 1)

adm_3_obs_col <- do.call(rbind, adm_3_col)

adm_3_at_1_obs_col_int <- adm_3_obs_col |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(run_num, time, adm_3) |> 
  mutate(sum_incid_I = sum(incid_I)) |>
  distinct(run_num, time, adm_3, sum_incid_I) |> 
  ungroup() |>
  group_by(run_num, adm_3) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(sum_incid_I),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  dplyr::filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(run_num, time) |>
  group_by(run_num) |>
  arrange(time) |>
  mutate(intro_loc = 'Col',
         Scale = 'Division',
         Count = row_number()) |>
  dplyr::select(run_num, time, adm_3, time, Count, Scale) 


(unique(adm_3_at_1_obs_col_int$run_num))

test <- adm_3_at_1_obs_col_int |> dplyr::filter(Count < 18) |>
  mutate(count = 1) |>
  group_by(adm_3) |>
  mutate(unit_count = sum(count),
         unit_prop = unit_count / 47) |>
  distinct(adm_3, unit_count, unit_prop) 

choropleth_3 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm3_slsd_20220816')

library(sf)
# Load population data
load('./tmp/adm_population_dat.RData')

# Load mobility to shape cross walk
# The mobility data combines multiple admin 3 units, changing the total from 339 to 330
mobility_shape_xwalk <- readRDS('./tmp/mobility_shape_xwalk.rds')

# Merge on the cross walk
choropleth_3 <- left_join(choropleth_3, mobility_shape_xwalk, by = c('ADM3_EN' = 'adm_3_shape'))

# Join polygons to create 330 mobility admin 3 units
choropleth_3_mobility <- choropleth_3 |> 
  group_by(adm_3_mobility) |>
  summarise(geometry = sf::st_union(geometry)) |>
  ungroup()

choropleth_3_mobility <- left_join(choropleth_3_mobility, test, by = c('adm_3_mobility' = 'adm_3'))


ggplot(data = choropleth_3_mobility) +
  geom_sf(aes(fill = unit_prop), color= 'black', linewidth = 0.20) +
  scale_fill_distiller(palette = 'Blues', direction = 1, name = 'invasion') +
  theme_void() + ggtitle(' ') + theme(legend.position = 'inside', legend.position.inside = c(0.85, 0.90),
                                      plot.title = element_text(size = 30, hjust = 0.5),
                                      legend.text = element_text(size = 22),
                                      legend.title = element_text(size = 24)) +
  coord_sf()





adm_3_mad <- mclapply(1:100, run_seir_model, beta =   0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_3_name_vec, adm_level = '3',
                      pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 48,
                      adm_x_walk = adm_3_x_walk, travel_mat = adm_3_phone_mobility_mat,
                      max_time = 365, time_step = 1)

adm_3_obs_col <- do.call(rbind, adm_3_col)

adm_3_obs_mad <- do.call(rbind, adm_3_mad)

##########################
# Administrative Level 2 #
##########################

adm_2_col <- mclapply(1:100, run_seir_model, beta =   0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_2_name_vec, adm_level = '2',
                      pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = 5,
                      adm_x_walk = adm_2_x_walk, travel_mat = adm_2_phone_mobility_mat,
                      max_time = 365, time_step = 1)

adm_2_mad <- mclapply(1:100, run_seir_model, beta =   0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_2_name_vec, adm_level = '2',
                      pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = 9,
                      adm_x_walk = adm_2_x_walk, travel_mat = adm_2_phone_mobility_mat,
                      max_time = 365, time_step = 1)

adm_2_obs_col <- do.call(rbind, adm_2_col)

adm_2_obs_mad <- do.call(rbind, adm_2_mad)

##########################
# Administrative Level 1 #
##########################

adm_1_col <- mclapply(1:100, run_seir_model, beta =   0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_1_name_vec, adm_level = '1',
                      pop_vec = adm_1_pop_vec, intro_adm = 'All', intro_num = 9,
                      adm_x_walk = adm_2_x_walk, travel_mat = adm_1_phone_mobility_mat,
                      max_time = 365, time_step = 1)

adm_1_mad <- mclapply(1:100, run_seir_model, beta =   0.3, gamma = 1/5, sigma = 1/2, prop_s = 0.90,
                      adm_name_vec = adm_1_name_vec, adm_level = '1',
                      pop_vec = adm_1_pop_vec, intro_adm = 'All', intro_num = 5,
                      adm_x_walk = adm_2_x_walk, travel_mat = adm_1_phone_mobility_mat,
                      max_time = 365, time_step = 1)

adm_1_obs_col <- do.call(rbind, adm_1_col)

adm_1_obs_mad <- do.call(rbind, adm_1_mad)

########################
# 3. CREATE SUBFIGURES #
########################

#######################
# Make disease curves #
#######################
  
take_off_3 <- adm_3_obs_col |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'Division') |> 
  distinct(Scale, take_off_perc)

take_off_2 <- adm_2_obs_col |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'District') |> 
  distinct(Scale, take_off_perc)

take_off_1 <- adm_1_obs_col |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'Province') |> 
  distinct(Scale, take_off_perc)

take_off_col <- rbind(take_off_1, take_off_2, take_off_3)

take_off_3_mad <- adm_3_obs_mad |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'Division') |> 
  distinct(Scale, take_off_perc)

take_off_2_mad <- adm_2_obs_mad |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'District') |> 
  distinct(Scale, take_off_perc)

take_off_1_mad <- adm_1_obs_mad |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'Province') |> 
  distinct(Scale, take_off_perc)
  
take_off_mad <- rbind(take_off_1_mad, take_off_2_mad, take_off_3_mad)

############
# Observed #
############

adm_3_obs_col_avg <- adm_3_obs_col |>
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(run_num, time, adm_1) |>
  mutate(incid_I_sum = sum(incid_I)) |>
  distinct(run_num, time, incid_I_sum) |>
  ungroup() |>
  group_by(time, adm_1) |>
  mutate(perc_50 = quantile(incid_I_sum, probs = 0.50),
         perc_95 = quantile(incid_I_sum, probs = 0.75),
         perc_05 = quantile(incid_I_sum, probs = 0.25)) |>
  distinct(adm_1, time, perc_50, perc_95, perc_05) |> 
  ungroup() |>
  mutate(cum_sum_I = cumsum(perc_50),
         cum_sum_I_95 = cumsum(perc_95),
         cum_sum_I_05 = cumsum(perc_05)) |>
  mutate(Scale = 'Division') |>
  dplyr::filter(adm_1 == 'Northern' | adm_1 == 'Western')

adm_2_obs_col_avg <- adm_2_obs_col |>
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(run_num, time, adm_1) |>
  mutate(incid_I_sum = sum(incid_I)) |>
  distinct(run_num, time, incid_I_sum) |>
  ungroup() |>
  group_by(time, adm_1) |>
  mutate(perc_50 = quantile(incid_I_sum, probs = 0.50),
         perc_95 = quantile(incid_I_sum, probs = 0.75),
         perc_05 = quantile(incid_I_sum, probs = 0.25)) |>
  distinct(adm_1, time, perc_50, perc_95, perc_05) |> 
  ungroup() |>
  mutate(cum_sum_I = cumsum(perc_50),
         cum_sum_I_95 = cumsum(perc_95),
         cum_sum_I_05 = cumsum(perc_05)) |>
  mutate(Scale = 'District') |>
  dplyr::filter(adm_1 == 'Northern' | adm_1 == 'Western')

adm_1_obs_col_avg <- adm_1_obs_col |>
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(run_num, time, adm_1) |>
  mutate(incid_I_sum = sum(incid_I)) |>
  distinct(run_num, time, incid_I_sum) |>
  ungroup() |>
  group_by(adm_1, time) |>
  mutate(perc_50 = quantile(incid_I_sum, probs = 0.50),
         perc_95 = quantile(incid_I_sum, probs = 0.75),
         perc_05 = quantile(incid_I_sum, probs = 0.25)) |>
  distinct(adm_1, time, perc_50, perc_95, perc_05) |> 
  ungroup() |>
  mutate(cum_sum_I = cumsum(perc_50),
         cum_sum_I_95 = cumsum(perc_95),
         cum_sum_I_05 = cumsum(perc_05)) |>
  mutate(Scale = 'Province') |>
  dplyr::filter(adm_1 == 'Northern' | adm_1 == 'Western')

line_col_obs_all <- rbind(adm_3_obs_col_avg, adm_2_obs_col_avg, adm_1_obs_col_avg)

adm_3_obs_mad_avg <- adm_3_obs_mad |>
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(run_num, time, adm_1) |>
  mutate(incid_I_sum = sum(incid_I)) |>
  distinct(adm_1, run_num, time, incid_I_sum) |>
  ungroup() |>
  group_by(time, adm_1) |>
  mutate(perc_50 = quantile(incid_I_sum, probs = 0.50),
         perc_95 = quantile(incid_I_sum, probs = 0.75),
         perc_05 = quantile(incid_I_sum, probs = 0.25)) |>
  distinct(adm_1, time, perc_50, perc_95, perc_05) |> 
  ungroup() |>
  mutate(cum_sum_I = cumsum(perc_50)) |>
  mutate(Scale = 'Division') |>
  dplyr::filter(adm_1 == 'Northern' | adm_1 == 'Western')

adm_2_obs_mad_avg <- adm_2_obs_mad |>
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(run_num, time, adm_1) |>
  mutate(incid_I_sum = sum(incid_I)) |>
  distinct(run_num, time, incid_I_sum) |>
  ungroup() |>
  group_by(time, adm_1) |>
  mutate(perc_50 = quantile(incid_I_sum, probs = 0.50),
         perc_95 = quantile(incid_I_sum, probs = 0.75),
         perc_05 = quantile(incid_I_sum, probs = 0.25)) |>
  distinct(adm_1, time, perc_50, perc_95, perc_05) |> 
  ungroup() |>
  mutate(cum_sum_I = cumsum(perc_50)) |>
  mutate(Scale = 'District') |>
  dplyr::filter(adm_1 == 'Northern' | adm_1 == 'Western')

adm_1_obs_mad_avg <- adm_1_obs_mad |>
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(run_num, time, adm_1) |>
  mutate(incid_I_sum = sum(incid_I)) |>
  distinct(run_num, time, incid_I_sum) |>
  ungroup() |>
  group_by(adm_1, time) |>
  mutate(perc_50 = quantile(incid_I_sum, probs = 0.50),
         perc_95 = quantile(incid_I_sum, probs = 0.75),
         perc_05 = quantile(incid_I_sum, probs = 0.25)) |>
  distinct(time, perc_50, perc_95, perc_05) |> 
  ungroup() |>
  mutate(cum_sum_I = cumsum(perc_50)) |>
  mutate(Scale = 'Province') |>
  dplyr::filter(adm_1 == 'Northern' | adm_1 == 'Western')

line_mad_obs_all <- rbind(adm_3_obs_mad_avg, adm_2_obs_mad_avg, adm_1_obs_mad_avg)

######################
# Introduction count #
######################

############
# Observed #
############

adm_3_at_1_obs_col_int <- adm_3_obs_col |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(run_num, time, adm_1) |> 
  mutate(sum_incid_I = sum(incid_I)) |>
  distinct(run_num, time, adm_1, sum_incid_I) |> 
  ungroup() |>
  group_by(run_num, adm_1) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(sum_incid_I),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  dplyr::filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(run_num, time) |>
  group_by(run_num) |>
  arrange(time) |>
  mutate(intro_loc = 'Col',
         Scale = 'Division',
         Count = row_number()) |>
  dplyr::select(run_num, time, adm_1, time, Count, Scale) 

adm_2_at_1_obs_col_int <- adm_2_obs_col |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(run_num, time, adm_1) |> 
  mutate(sum_incid_I = sum(incid_I)) |>
  distinct(run_num, time, adm_1, sum_incid_I) |> 
  ungroup() |>
  group_by(run_num, adm_1) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(sum_incid_I),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  dplyr::filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(run_num, time) |>
  group_by(run_num) |>
  mutate(intro_loc = 'Col',
         Scale = 'District',
         Count = seq(1, 9, 1)) |>
  dplyr::select(run_num, time, adm_1, time, Count, Scale) 

adm_1_obs_col_int <- adm_1_obs_col |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(run_num, time, adm_1) |> 
  mutate(sum_incid_I = sum(incid_I)) |>
  distinct(run_num, time, adm_1, sum_incid_I) |> 
  ungroup() |>
  group_by(run_num, adm_1) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(sum_incid_I),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  dplyr::filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(run_num, time) |>
  group_by(run_num) |>
  mutate(intro_loc = 'Col',
         Scale = 'Province',
         Count = seq(1, 9, 1)) |>
  dplyr::select(run_num, time, adm_1, time, Count, Scale) 

int_col_obs_all <- rbind(adm_1_obs_col_int, adm_2_at_1_obs_col_int, adm_3_at_1_obs_col_int)

data_summary <- function(x) {
  m <- median(x)
  ymin <- quantile(x, probs = 0.25)
  ymax <- quantile(x, probs = 0.75)
  return(c(y = m,ymin = ymin,ymax = ymax))
}

int_col_obs_all <- int_col_obs_all |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 

adm_3_at_1_obs_mad_int <- adm_3_obs_mad |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(run_num, time, adm_1) |> 
  mutate(sum_incid_I = sum(incid_I)) |>
  distinct(run_num, time, adm_1, sum_incid_I) |> 
  ungroup() |>
  group_by(run_num, adm_1) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(sum_incid_I),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  dplyr::filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(run_num, time) |>
  group_by(run_num) |>
  arrange(time) |>
  mutate(intro_loc = 'Col',
         Scale = 'Division',
         Count = row_number()) |>
  dplyr::select(run_num, time, adm_1, time, Count, Scale) 

adm_2_at_1_obs_mad_int <- adm_2_obs_mad |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(run_num, time, adm_1) |> 
  mutate(sum_incid_I = sum(incid_I)) |>
  distinct(run_num, time, adm_1, sum_incid_I) |> 
  ungroup() |>
  group_by(run_num, adm_1) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(sum_incid_I),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  dplyr::filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(run_num, time) |>
  group_by(run_num) |>
  mutate(intro_loc = 'Col',
         Scale = 'District',
         Count = seq(1, 9, 1)) |>
  dplyr::select(run_num, time, adm_1, time, Count, Scale) 

adm_1_obs_mad_int <- adm_1_obs_mad |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(run_num, time, adm_1) |> 
  mutate(sum_incid_I = sum(incid_I)) |>
  distinct(run_num, time, adm_1, sum_incid_I) |> 
  ungroup() |>
  group_by(run_num, adm_1) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(sum_incid_I),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  dplyr::filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(run_num, time) |>
  group_by(run_num) |>
  mutate(intro_loc = 'Col',
         Scale = 'Province',
         Count = seq(1, 9, 1)) |>
  dplyr::select(run_num, time, adm_1, time, Count, Scale) 

int_mad_obs_all <- rbind(adm_1_obs_mad_int, adm_2_at_1_obs_mad_int, adm_3_at_1_obs_mad_int)

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y = m,ymin = ymin,ymax = ymax))
}

int_mad_obs_all <- int_mad_obs_all |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 

adm_1_order_mad <- adm_1_obs_mad_int |>
  group_by(adm_1) |>
  mutate(median = median(time)) |>
  distinct(adm_1, median) |>
  ungroup() |>
  arrange(median) |>
  mutate(Order = row_number())

adm_1_order_col <- adm_1_obs_col_int |>
  group_by(adm_1) |>
  mutate(median = median(time)) |>
  distinct(adm_1, median) |>
  ungroup() |>
  arrange(median) |>
  mutate(Order = row_number())

int_mad_obs_all <- left_join(int_mad_obs_all, adm_1_order_mad, by = c('adm_1' = 'adm_1'))
int_col_obs_all <- left_join(int_col_obs_all, adm_1_order_col, by = c('adm_1' = 'adm_1'))

line_plot_col_obs <- ggplot(int_col_obs_all, aes(x = time, y = fct_reorder(adm_1, Order), fill = Scale)) +
  #geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, 
  #scale="width", width = 0.6, position = position_dodge(width = 0.9)) +
  geom_boxplot(position = position_dodge(width = 0.65), width=0.5, color = 'black', outlier.shape = NA, coef = 0) +
  theme_minimal() + coord_cartesian(xlim = c(0, 110)) +
  scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  theme(legend.position = 'none') +
  ylab('Province') +
  xlab('Time (days)') +
  ggtitle('Province Infection Time') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) 
line_plot_col_obs

line_plot_mad_obs <- ggplot(int_mad_obs_all, aes(x = time, y = fct_reorder(adm_1, Order), fill = Scale)) +
  #geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, 
  #scale="width", width = 0.6, position = position_dodge(width = 0.9)) +
  geom_boxplot(position = position_dodge(width = 0.65), width=0.5, color = 'black', outlier.shape = NA, coef = 0) +
  theme_minimal() + coord_cartesian(xlim = c(0, 110)) +
  scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  theme(legend.position = 'none') +
  ylab('Province') +
  xlab('Time (days)') +
  ggtitle('Province Infection Time') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) 
line_plot_mad_obs

dis_plot_col_obs <- ggplot(line_col_obs_all, aes(x = time, y = perc_50)) +
  geom_ribbon(aes(ymin = perc_05, ymax = perc_95, fill = Scale), alpha = 0.2) +
  geom_line(aes(color = Scale), size = 1.75) + #xlim(0, 100) + ylim(0, 50) +
  theme_minimal() + scale_color_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        strip.text = element_text(size = 30)) +
  ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Province Disease Curves') + 
  facet_wrap(vars(adm_1), nrow = 2, scale = 'free') 

dis_plot_col_obs
dis_plot_mad_obs <- ggplot(line_mad_obs_all, aes(x = time, y = perc_50)) +
  geom_ribbon(aes(ymin = perc_05, ymax = perc_95, fill = Scale), alpha = 0.2) +
  geom_line(aes(color = Scale), size = 1.75) + #xlim(0, 100) + ylim(0, 50) +
  theme_minimal() + scale_color_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        strip.text = element_text(size = 30)) +
  ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Province Disease Curves') + 
  facet_wrap(vars(adm_1), nrow = 2, scale = 'free') 
dis_plot_mad_obs

take_off_col <- take_off_col |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 

take_off_mad <- take_off_mad |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 


take_off_mad_plot <- ggplot(take_off_mad, aes(x=Scale, y=take_off_perc, fill = Scale)) + 
  geom_bar(stat = "identity", width=0.42, color = 'black', alpha = 0.9) +
  theme_minimal() + scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6), limits = c(0, 0.65)) +
  ylab('Proportion') + xlab('Scale') + ggtitle('Epidemic Occurance')

take_off_col_plot <- ggplot(take_off_col, aes(x=Scale, y=take_off_perc, fill = Scale)) + 
  geom_bar(stat = "identity", width=0.42, color = 'black', alpha = 0.9) +
  theme_minimal() + scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6), limits = c(0, 0.65)) +
  ylab('Proportion') + xlab('Scale') + ggtitle('Epidemic Occurance')

line_mad_obs_all$Scale <- factor(line_mad_obs_all$Scale, levels=c('Division', 'District', 'Province'))
legend <- ggplot(data = line_mad_obs_all) + geom_line(aes(x = time, y = perc_50, color = Scale), linewidth = 3.5, alpha = 1) + 
  geom_ribbon(aes(x = time, ymin = perc_05, ymax = perc_95, fill = Scale), alpha = 0.2) +
  theme_minimal() + ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Disease Curves') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 40),
        legend.title = element_text(size = 40),
        legend.key.size = unit(1.2, "cm")) +
  scale_color_manual(values = c( "#41AE76", "#9e9ac8","#4292C6")) +
  scale_fill_manual(values = c( "#41AE76","#9e9ac8", "#4292C6"))

legend_get <- get_legend(legend)

row_1_1 <- cowplot::plot_grid(take_off_col_plot,
                              line_plot_col_obs, dis_plot_col_obs, 
                              nrow = 1, labels = c('(a)', '(b)', '(c)'),
                              rel_widths = c(0.70, 1.1, 0.8),
                              label_size = 34)
row_1_2 <- cowplot::plot_grid(take_off_mad_plot,
                              line_plot_mad_obs, dis_plot_mad_obs,
                              nrow = 1, labels = c('(d)', '(e)', '(f)'),
                              rel_widths = c(0.70, 1.1, 0.8),
                              label_size = 34)

plot <- cowplot::plot_grid(ggplot() + theme_void(), row_1_1,
                           ggplot() + theme_void(), row_1_2, legend_get,
                            nrow = 5, labels = c('Colombo Introduction Event', '',
                                                 'Delft Introduction Event', '', ''),
                            label_size = 34, hjust = 0,
                            rel_heights = c(0.08, 1, 0.08, 1, 0.1))

# Call Outs
line_mad_obs_all |> group_by(Scale) |> mutate(sum= sum(perc_50)) |> distinct(Scale, sum)
line_col_obs_all |> group_by(Scale) |> mutate(sum= sum(perc_50)) |> distinct(Scale, sum)
test <- int_col_obs_all |> group_by(adm_1, Scale) |> mutate(med = median(time)) |> distinct(adm_1, Scale, med)
test <- int_mad_obs_all |> group_by(adm_1, Scale) |> mutate(med = median(time)) |> distinct(adm_1, Scale, med)

max(line_mad_obs_all[line_mad_obs_all$adm_1 == 'Northern' & line_mad_obs_all$Scale == 'Division',]$perc_50)
max(line_mad_obs_all[line_mad_obs_all$adm_1 == 'Northern' & line_mad_obs_all$Scale == 'District',]$perc_50)


ggsave('./figs/figure_4_example.jpg', plot = plot , height = 17, width = 25)

# Save observed plots for sensitivity analysis
save(list = c('line_col_obs_all', 
              'line_mad_obs_all',
              'int_col_obs_all', 
              'int_mad_obs_all'), 
     file = './tmp/figure_4_plots_obs.RData')

################################################################################
################################################################################

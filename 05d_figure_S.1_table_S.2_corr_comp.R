################################################################################
# File Name: 05d_table_figure_S.2_corr_comp                                    #
#                                                                              #
# Purpose:   This file contains code for table S.2, figure S.2 as well as      #
#            text call outs.                                                   #
#                                                                              #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure S.2 and table S.2                                #
#            3. Create S.2 proportion subfigures                               #
#            4. Combine subfigures                                             #
#            5. Text callouts                                                  #
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

######################################
# 2. CREATE FIGURE S.2 AND TABLE S.2 #
######################################

# Load simulated trip count data
adm_3_sim_mobility_dat <- readRDS('./out/adm_3_sim_mobility_dat.rds')
adm_2_sim_mobility_dat <- readRDS('./out/adm_2_sim_mobility_dat.rds')
adm_1_sim_mobility_dat <- readRDS('./out/adm_1_sim_mobility_dat.rds')

# Load distance data
adm_1_dist <- readRDS('./out/adm_1_dist.rds')
adm_2_dist <- readRDS('./out/adm_2_dist.rds')
adm_3_dist <- readRDS('./out/adm_3_dist.rds')

# Load observed trip count data
adm_3_phone_mobility_dat <- readRDS('./out/adm_3_phone_mobility_dat.rds')
adm_2_phone_mobility_dat <- readRDS('./out/adm_2_phone_mobility_dat.rds')
adm_1_phone_mobility_dat <- readRDS('./out/adm_1_phone_mobility_dat.rds')

# Load population data
adm_3_population_dat <- readRDS('./out/adm_3_population_dat.rds')
adm_2_population_dat <- readRDS('./out/adm_2_population_dat.rds')
adm_1_population_dat <- readRDS('./out/adm_1_population_dat.rds')

##########################
# Administrative Level 1 #
##########################

# Merge simulated and observed trip count data
adm_1_long_pred <- left_join(adm_1_sim_mobility_dat, 
                             adm_1_phone_mobility_dat[, c('adm_1_origin', 'adm_1_destination', 'trips_avg')],
                             by = c('adm_1_origin' = 'adm_1_origin', 
                                    'adm_1_destination' = 'adm_1_destination'))
adm_1_long_pred <- left_join(adm_1_long_pred, 
                             adm_1_dist,
                             by = c('adm_1_origin' = 'adm_1_origin', 
                                    'adm_1_destination' = 'adm_1_destination'))
# Merge on origin and destination population data
adm_1_long_pred <- left_join(adm_1_long_pred, 
                             adm_1_population_dat,
                             by = c('adm_1_origin' = 'adm_1'))
adm_1_long_pred <- left_join(adm_1_long_pred, 
                             adm_1_population_dat,
                             by = c('adm_1_destination' = 'adm_1'))
adm_1_long_pred <- adm_1_long_pred |> 
  dplyr::rename('adm_1_origin_pop' = 'population_2020_adm_1.x',
                'adm_1_destination_pop' = 'population_2020_adm_1.y')

# Correlations for table S.2
# Correlations (observed)
cor(adm_1_long_pred$trips_avg, adm_1_long_pred$distance)
cor(adm_1_long_pred$trips_avg, adm_1_long_pred$adm_1_origin_pop)
cor(adm_1_long_pred$trips_avg, adm_1_long_pred$adm_1_destination_pop)
# Correlations (simulated)
cor(adm_1_long_pred$value, adm_1_long_pred$distance)
cor(adm_1_long_pred$value, adm_1_long_pred$adm_1_origin_pop)
cor(adm_1_long_pred$value, adm_1_long_pred$adm_1_destination_pop)

# Comparison grph for figure S.1
sim_comp_1 <- ggplot(data = adm_1_long_pred) + 
  geom_point(aes(x = trips_avg, y = value), size = 10, color = '#4292C6', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  xlim(0, 20000000) + theme(legend.position = 'none',
                            axis.text = element_text(size=20),
                            axis.title = element_text(size=26),
                            plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')

##########################
# Administrative Level 2 #
##########################

# Merge simulated and observed trip count data
adm_2_long_pred <- left_join(adm_2_sim_mobility_dat, 
                             adm_2_phone_mobility_dat[, c('adm_2_origin', 'adm_2_destination', 'trips_avg')],
                             by = c('adm_2_origin' = 'adm_2_origin', 
                                    'adm_2_destination' = 'adm_2_destination'))
adm_2_long_pred <- left_join(adm_2_long_pred, 
                             adm_2_dist,
                             by = c('adm_2_origin' = 'adm_2_origin', 
                                    'adm_2_destination' = 'adm_2_destination'))
# Merge on origin and destination population data
adm_2_long_pred <- left_join(adm_2_long_pred, 
                             adm_2_population_dat,
                             by = c('adm_2_origin' = 'adm_2'))
adm_2_long_pred <- left_join(adm_2_long_pred, 
                             adm_2_population_dat,
                             by = c('adm_2_destination' = 'adm_2'))
adm_2_long_pred <- adm_2_long_pred |>
  dplyr::rename('adm_2_origin_pop' = 'population_2020_adm_2.x',
                'adm_2_destination_pop' = 'population_2020_adm_2.y')

# Correlations for table S.2
# Correlations (observed)
cor(adm_2_long_pred$trips_avg, adm_2_long_pred$distance)
cor(adm_2_long_pred$trips_avg, adm_2_long_pred$adm_2_origin_pop)
cor(adm_2_long_pred$trips_avg, adm_2_long_pred$adm_2_destination_pop)
# Correlations (simulated)
cor(adm_2_long_pred$value, adm_2_long_pred$distance)
cor(adm_2_long_pred$value, adm_2_long_pred$adm_2_origin_pop)
cor(adm_2_long_pred$value, adm_2_long_pred$adm_2_destination_pop)

sim_comp_2 <- ggplot(data = adm_2_long_pred) + 
  geom_point(aes(x = trips_avg, y = value), size = 10, color = '#41AE76', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  xlim(0, 7000000) + theme(legend.position = 'none',
                           axis.text = element_text(size=20),
                           axis.title = element_text(size=26),
                           plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')

##########################
# Administrative Level 3 #
##########################

# Merge simulated and observed trip count data
adm_3_long_pred <- left_join(adm_3_sim_mobility_dat, 
                             adm_3_phone_mobility_dat[, c('adm_3_origin', 'adm_3_destination', 'trips_avg')],
                             by = c('adm_3_origin' = 'adm_3_origin', 
                                    'adm_3_destination' = 'adm_3_destination'))
adm_3_long_pred <- left_join(adm_3_long_pred, 
                             adm_3_dist,
                             by = c('adm_3_origin' = 'adm_3_origin', 
                                    'adm_3_destination' = 'adm_3_destination'))
# Merge on origin and destination population data
adm_3_long_pred <- left_join(adm_3_long_pred, 
                             adm_3_population_dat,
                             by = c('adm_3_origin' = 'adm_3_mobility'))
adm_3_long_pred <- left_join(adm_3_long_pred, 
                             adm_3_population_dat,
                             by = c('adm_3_destination' = 'adm_3_mobility'))
adm_3_long_pred <- adm_3_long_pred |> 
  dplyr::rename('adm_3_origin_pop' = 'population_2020_adm_3.x',
                'adm_3_destination_pop' = 'population_2020_adm_3.y')

# Correlations for table S.2
# Correlations (observed)
cor(adm_3_long_pred$trips_avg, adm_3_long_pred$distance, use = 'pairwise.complete.obs')
cor(adm_3_long_pred$trips_avg, adm_3_long_pred$adm_3_origin_pop, use = 'pairwise.complete.obs')
cor(adm_3_long_pred$trips_avg, adm_3_long_pred$adm_3_destination_pop, use = 'pairwise.complete.obs')
# Correlations (simulated)
cor(adm_3_long_pred$value, adm_3_long_pred$distance)
cor(adm_3_long_pred$value, adm_3_long_pred$adm_3_origin_pop)
cor(adm_3_long_pred$value, adm_3_long_pred$adm_3_destination_pop)

sim_comp_3 <- ggplot(data = adm_3_long_pred) + 
  geom_point(aes(x = trips_avg, y = value), size = 10, color = '#807DBA', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  ylim(0, 600000) + xlim(0, 600000) + theme(legend.position = 'none',
                                            axis.text = element_text(size=20),
                                            axis.title = element_text(size=26),
                                            plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')

#######################################
# 3. CREATE S.2 PROPORTION SUBFIGURES #
#######################################

# Load phone mobility data
adm_3_phone_mobility_long <- readRDS('./out/adm_3_phone_mobility_long.rds')
adm_2_phone_mobility_long <- readRDS('./out/adm_2_phone_mobility_long.rds')
adm_1_phone_mobility_long <- readRDS('./out/adm_1_phone_mobility_long.rds')

# Load simulated mobility data
adm_3_sim_mobility_long <- readRDS('./out/adm_3_sim_mobility_long.rds')
adm_2_sim_mobility_long <- readRDS('./out/adm_2_sim_mobility_long.rds')
adm_1_sim_mobility_long <- readRDS('./out/adm_1_sim_mobility_long.rds')

##########################
# Administrative Level 3 #
##########################

# Merge and Plot Comparison
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, 
                                          adm_3_sim_mobility_long,
                                          by = c('origin' = 'origin',
                                                 'destination' = 'destination'))
# Plot
sim_comp_3_prop <- ggplot(data = adm_3_phone_mobility_long) + 
  geom_point(aes(x = value.y, y = value.x), size = 10, color = '#807DBA', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  ylim(0, 1) + xlim(0, 1) + theme(legend.position = 'none',
                                  axis.text = element_text(size=20),
                                  axis.title = element_text(size=26),
                                  plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')

##########################
# Administrative Level 2 #
##########################

# Merge and Plot Comparison
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, 
                                       adm_2_sim_mobility_long,
                                       by = c('origin' = 'origin',
                                              'destination' = 'destination'))
# Plot
sim_comp_2_prop <- ggplot(data = adm_2_phone_mobility_long) + 
  geom_point(aes(x = value.y, y = value.x), size = 10, color = '#41AE76', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  ylim(0, 1) + xlim(0, 1) + theme(legend.position = 'none',
                                  axis.text = element_text(size=20),
                                  axis.title = element_text(size=26),
                                  plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')

##########################
# Administrative Level 1 #
##########################

# Merge and Plot Comparison
adm_1_phone_mobility_long <- left_join(adm_1_phone_mobility_long, 
                                       adm_1_sim_mobility_long,
                                       by = c('origin' = 'origin',
                                              'destination' = 'destination'))
# Plot
sim_comp_1_prop <- ggplot(data = adm_1_phone_mobility_long) + 
  geom_point(aes(x = value.y, y = value.x), size = 10, color = '#4292C6', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  ylim(0, 1) + xlim(0, 1) + theme(legend.position = 'none',
                                  axis.text = element_text(size=20),
                                  axis.title = element_text(size=26),
                                  plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')

#########################
# 4. COMBINE SUBFIGURES #
#########################

# Count correlations
supp_2.1 <- plot_grid(sim_comp_1 + ggtitle('\n\nProvince'), 
                      sim_comp_2 + ggtitle('\nDistrict'), 
                      sim_comp_3 + ggtitle('\nDivision'), 
                      nrow = 3,
                      labels = c('(a)', '(b)', '(c)'),
                      label_size = 34, hjust = 0)

# Proportion correlations
supp_2.2 <- plot_grid(
  sim_comp_1_prop + ggtitle('\n\nProvince'),
  sim_comp_2_prop + ggtitle('\nDistrict'),
  sim_comp_3_prop + ggtitle('\nDivision'),
  nrow = 3,
  labels = c('(d)', '(e)', '(f)'),
  label_size = 34, hjust = 0)

# Combine all 6 plots
supp_2 <- plot_grid(
  supp_2.1, supp_2.2,
  nrow = 1,
  labels = c('       Trip Count', 'Trip Proportion'),
  label_size = 34, hjust = -1.45)

# Save
ggsave('./figs/figure_S.1_correlations.jpg', plot = supp_2, height = 35, width = 25)

####################
# 5. TEXT CALLOUTS #
####################

# "While the agreement between simulated and observed trip counts was strong 
# (Pearson correlation coefficients of 0.99 for province, 0.97 for district, and 0.83 
# for division levels), the accuracy and fit of these models were poorest for finer 
# spatial scales"
cor(adm_1_long_pred$trips_avg, adm_1_long_pred$value)
cor(adm_2_long_pred$trips_avg, adm_2_long_pred$value)
cor(adm_3_long_pred$trips_avg, adm_3_long_pred$value)

# Supplement:
# "Correlations of 0.99, 0.98, and 0.82 between observed and simulated trip proportions 
# at the province, district, and division levels, respectively"
cor(adm_1_phone_mobility_long$value.y, adm_1_phone_mobility_long$value.x, use = 'pairwise.complete.obs')
cor(adm_2_phone_mobility_long$value.y, adm_2_phone_mobility_long$value.x, use = 'pairwise.complete.obs')
cor(adm_3_phone_mobility_long$value.y, adm_3_phone_mobility_long$value.x, use = 'pairwise.complete.obs')

################################################################################
################################################################################

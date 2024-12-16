################################################################################
# File Name: 05b_create_figure_trip_dist_scatter                               #
#                                                                              #
# Purpose:   Create scatter plots of the trip count - trip distance            #
#            relationship for each administrative level and data source.       #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load data, format, and plot                                    #
#            3. Create sub figures, combine, and save                          #
#            4. Text callouts (used for appendix tables)                       #
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

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

##################################
# 2. LOAD DATA, FORMAT, AND PLOT #
##################################

#####################
# MOBILE PHONE DATA #
#####################

# Load mobile phone data
load('./tmp/adm_phone_mobility_dat.RData')

# Load distance matrices
load('./tmp/adm_dist_mat.RData')

# Set the theme for the figures
theme_plot <- theme(axis.title = element_text(size=26),
                    axis.text = element_text(size=22),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    plot.title = element_text(size=26, hjust = 0.5))

# Merge distance data to trips data
adm_3_phone_mobility_dat <- left_join(adm_3_phone_mobility_dat, adm_3_dist,
                                      by = c('adm_3_origin' = 'adm_3_origin',
                                             'adm_3_destination' = 'adm_3_destination'))
adm_2_phone_mobility_dat <- left_join(adm_2_phone_mobility_dat, adm_2_dist,
                                      by = c('adm_2_origin' = 'adm_2_origin',
                                             'adm_2_destination' = 'adm_2_destination'))
adm_1_phone_mobility_dat <- left_join(adm_1_phone_mobility_dat, adm_1_dist,
                                      by = c('adm_1_origin' = 'adm_1_origin',
                                             'adm_1_destination' = 'adm_1_destination'))

# Plot scatter plots
trips_mp_3 <- ggplot(data = adm_3_phone_mobility_dat) +
  geom_point(aes(x = distance, y = log(trips_avg)), color = '#4292C6', alpha = 0.04, size = 3) + theme_minimal() + 
  theme_plot + ylab('\n\nLog Daily Average Trips') + xlab('Distance (km)') + ylim(-27, 17) + xlim(0, 450)
trips_mp_2 <- ggplot(data = adm_2_phone_mobility_dat) +
  geom_point(aes(x = distance, y = log(trips_avg)), color = '#41AE76', alpha = 0.5, size = 3) + theme_minimal() + 
  theme_plot + ylab('\n\nLog Daily Average Trips') + xlab('Distance (km)') + ylim(-10, 17) + xlim(0, 400)
trips_mp_1 <- ggplot(data = adm_1_phone_mobility_dat) +
  geom_point(aes(x = distance, y = log(trips_avg)), color = '#807DBA', alpha = 0.5, size = 3) + theme_minimal() + 
  theme_plot + ylab('Mobile Phone Data\n\nLog Daily Average Trips') + xlab('Distance (km)') + ylim(0, 17) + xlim(0, 350)

###########################
# SIMULATED MOBILITY DATA #
###########################

# Load simulated mobility data
load('./mobility-spatial-scale/simulated data/mobile_phone_sim_count_dat.RData')

# Reshape matrices to long format
adm_3_phone_mobility_sim_dat <- reshape2::melt(phone_mobility_adm_3_pred, 
                                       id.vars = 'adm_3_origin', 
                                       variable.name = 'adm_3_destination')    
adm_3_phone_mobility_sim_dat <- adm_3_phone_mobility_sim_dat %>% 
  rename('adm_3_origin' = 'Var1',
         'adm_3_destination' = 'Var2')

adm_2_phone_mobility_sim_dat <- reshape2::melt(phone_mobility_adm_2_pred, 
                                               id.vars = 'adm_2_origin', 
                                               variable.name = 'adm_2_destination')    
adm_2_phone_mobility_sim_dat <- adm_2_phone_mobility_sim_dat %>% 
  rename('adm_2_origin' = 'Var1',
         'adm_2_destination' = 'Var2')

adm_1_phone_mobility_sim_dat <- reshape2::melt(phone_mobility_adm_1_pred, 
                                               id.vars = 'adm_1_origin', 
                                               variable.name = 'adm_1_destination')    
adm_1_phone_mobility_sim_dat <- adm_1_phone_mobility_sim_dat %>% 
  rename('adm_1_origin' = 'Var1',
         'adm_1_destination' = 'Var2')

# Merge distance data to trips data
adm_3_phone_mobility_sim_dat <- left_join(adm_3_phone_mobility_sim_dat, adm_3_dist,
                                      by = c('adm_3_origin' = 'adm_3_origin',
                                             'adm_3_destination' = 'adm_3_destination'))
adm_2_phone_mobility_sim_dat <- left_join(adm_2_phone_mobility_sim_dat, adm_2_dist,
                                      by = c('adm_2_origin' = 'adm_2_origin',
                                             'adm_2_destination' = 'adm_2_destination'))
adm_1_phone_mobility_sim_dat <- left_join(adm_1_phone_mobility_sim_dat, adm_1_dist,
                                      by = c('adm_1_origin' = 'adm_1_origin',
                                             'adm_1_destination' = 'adm_1_destination'))

# Plot scatter plots
trips_mp_sim_3 <- ggplot(data = adm_3_phone_mobility_sim_dat) +
  geom_point(aes(x = distance, y = log(value)), color = '#4292C6', alpha = 0.04, size = 3) + theme_minimal() + 
  theme_plot + ylab('\n\nLog Daily Average Trips') + xlab('Distance (km)') + scale_y_continuous(limits=c(-27, 17)) + xlim(0, 450)
trips_mp_sim_2 <- ggplot(data = adm_2_phone_mobility_sim_dat) +
  geom_point(aes(x = distance, y = log(value)), color = '#41AE76', alpha = 0.5, size = 3) + theme_minimal() + 
  theme_plot + ylab('\n\nLog Daily Average Trips') + xlab('Distance (km)') + ylim(-10, 17) + xlim(0, 400)
trips_mp_sim_1 <- ggplot(data = adm_1_phone_mobility_sim_dat) +
  geom_point(aes(x = distance, y = log(value)), color = '#807DBA', alpha = 0.5, size = 3) + theme_minimal() + 
  theme_plot + ylab('Simulated Mobile Phone Data\n\nLog Daily Average Trips') + xlab('Distance (km)') + ylim(0, 17) + xlim(0, 350)

############################################
# 3. CREATE SUB FIGURES, COMBINE, AND SAVE #
############################################

sub_1 <- plot_grid(trips_mp_1 + ggtitle('Administrative Level 1'), trips_mp_sim_1, 
                   nrow = 2,
                   labels = c('(a)', '(d)'),
                   label_size = 26, hjust = 0) 

sub_2 <- plot_grid(trips_mp_2 + ggtitle('Administrative Level 2'), trips_mp_sim_2, 
                   nrow = 2,
                   labels = c('(b)', '(e)'),
                   label_size = 26, hjust = 0)

sub_3 <- plot_grid(trips_mp_3 + ggtitle('Administrative Level 3'), trips_mp_sim_3, 
                   nrow = 2,
                   labels = c('(c)', '(f)'),
                   label_size = 26, hjust = 0)

# Create figure, with labels
figure <- plot_grid(sub_1, sub_2, sub_3,
                    nrow = 1,
                    rel_widths = c(1, 1, 1))

# Save figure 
ggsave('./figs/figure_trip_dist_scatter.jpg', plot = figure, height = 16, width = 25)

####################
# 4. TEXT CALLOUTS #
####################

# Used for appendix tables

# Log-Linear relationships
lm(log(adm_3_survey_mobility_dat$trips) ~ adm_3_survey_mobility_dat$distance)
lm(log(adm_3_fb_mobility_dat$trips_avg) ~ adm_3_fb_mobility_dat$distance)
lm(log(adm_3_phone_mobility_dat$trips_avg) ~ adm_3_phone_mobility_dat$distance)

lm(log(adm_2_survey_mobility_dat$trips) ~ adm_2_survey_mobility_dat$distance)
lm(log(adm_2_fb_mobility_dat$trips_avg) ~ adm_2_fb_mobility_dat$distance)
lm(log(adm_2_phone_mobility_dat$trips_avg) ~ adm_2_phone_mobility_dat$distance)

lm(log(adm_1_survey_mobility_dat$trips) ~ adm_1_survey_mobility_dat$distance)
lm(log(adm_1_fb_mobility_dat$trips_avg) ~ adm_1_fb_mobility_dat$distance)
lm(log(adm_1_phone_mobility_dat$trips_avg) ~ adm_1_phone_mobility_dat$distance)

# Total trips and average trip distance
sum(adm_3_phone_mobility_dat$trips_avg)
sum(adm_3_phone_mobility_dat$distance*adm_3_phone_mobility_dat$trips_avg)/sum(adm_3_phone_mobility_dat$trips_avg)

sum(adm_3_fb_mobility_dat$trips_avg)
sum(adm_3_fb_mobility_dat$distance*adm_3_fb_mobility_dat$trips_avg)/sum(adm_3_fb_mobility_dat$trips_avg)

sum(adm_3_survey_mobility_dat$trips)
sum(adm_3_survey_mobility_dat$distance*adm_3_survey_mobility_dat$trips)/sum(adm_3_survey_mobility_dat$trips)

sum(adm_2_phone_mobility_dat$trips_avg)
sum(adm_2_phone_mobility_dat$distance*adm_2_phone_mobility_dat$trips_avg)/sum(adm_2_phone_mobility_dat$trips_avg)

sum(adm_2_fb_mobility_dat$trips_avg)
sum(adm_2_fb_mobility_dat$distance*adm_2_fb_mobility_dat$trips_avg)/sum(adm_2_fb_mobility_dat$trips_avg)

sum(adm_2_survey_mobility_dat$trips)
sum(adm_2_survey_mobility_dat$distance*adm_2_survey_mobility_dat$trips)/sum(adm_2_survey_mobility_dat$trips)

sum(adm_1_phone_mobility_dat$trips_avg)
sum(adm_1_phone_mobility_dat$distance*adm_1_phone_mobility_dat$trips_avg)/sum(adm_1_phone_mobility_dat$trips_avg)

sum(adm_1_fb_mobility_dat$trips_avg)
sum(adm_1_fb_mobility_dat$distance*adm_1_fb_mobility_dat$trips_avg)/sum(adm_1_fb_mobility_dat$trips_avg)

sum(adm_1_survey_mobility_dat$trips)
sum(adm_1_survey_mobility_dat$distance*adm_1_survey_mobility_dat$trips)/sum(adm_1_survey_mobility_dat$trips)

# Maximum trips
max(adm_3_phone_mobility_dat$trips_avg)
max(adm_3_fb_mobility_dat$trips_avg)
max(adm_3_survey_mobility_dat$trips, na.rm = TRUE)

max(adm_2_phone_mobility_dat$trips_avg)
max(adm_2_fb_mobility_dat$trips_avg)
max(adm_2_survey_mobility_dat$trips, na.rm = TRUE)

max(adm_1_phone_mobility_dat$trips_avg)
max(adm_1_fb_mobility_dat$trips_avg)
max(adm_1_survey_mobility_dat$trips, na.rm = TRUE)

# Maximum distance
max(adm_3_phone_mobility_dat$distance)
max(adm_3_fb_mobility_dat$distance)
max(adm_3_survey_mobility_dat$distance)

max(adm_2_phone_mobility_dat$distance)
max(adm_2_fb_mobility_dat$distance)
max(adm_2_survey_mobility_dat$distance)

max(adm_1_phone_mobility_dat$distance)
max(adm_1_fb_mobility_dat$distance)
max(adm_1_survey_mobility_dat$distance)

################################################################################
################################################################################

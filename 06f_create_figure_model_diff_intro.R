################################################################################
# File Name: 04f_create_figure_model_diff_intro                                #
#                                                                              #
# Purpose:   Create figures that describe differences between models with      #
#            various spatial levels of mobility data, exploring introduction   #
#            locations.                                                        #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load figure data and create sub plots                          #
#            3. Create figure                                                  #
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
library(dplyr)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

############################################
# 2. LOAD FIGURE DATA AND CREATE SUB PLOTS #
############################################

# Load model results
load('./tmp/introduction_location_model_results.RData')

# Load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Add on administrative name vectors
adm_1_at_1_mp$adm_1 <- adm_1_name_vec
adm_2_at_1_mp$adm_2 <- adm_2_name_vec
adm_3_at_1_mp$adm_3 <- adm_3_name_vec
adm_2_at_2_mp$adm_2 <- adm_2_name_vec
adm_3_at_2_mp$adm_3 <- adm_3_name_vec

# Add on population vectors
adm_1_at_1_mp$adm_1_pop <- adm_1_pop_vec
adm_2_at_1_mp$adm_2_pop <- adm_2_pop_vec
adm_3_at_1_mp$adm_3_pop <- adm_3_pop_vec
adm_2_at_2_mp$adm_2_pop <- adm_2_pop_vec
adm_3_at_2_mp$adm_3_pop <- adm_3_pop_vec

# Add on administrative level crosswalks
adm_2_at_1_mp <- left_join(adm_2_at_1_mp, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_3_at_1_mp <- left_join(adm_3_at_1_mp, adm_3_x_walk, by = c('adm_3' = 'adm_3'))
adm_3_at_2_mp <- left_join(adm_3_at_2_mp, adm_3_x_walk, by = c('adm_3' = 'adm_3'))

# Merge together different model results at various levels
adm_1_adm_3 <- left_join(adm_3_at_1_mp, adm_1_at_1_mp, by = c('adm_1' = 'adm_1'))
adm_1_adm_2 <- left_join(adm_2_at_1_mp, adm_1_at_1_mp, by = c('adm_1' = 'adm_1'))
adm_2_adm_3 <- left_join(adm_3_at_2_mp, adm_2_at_2_mp, by = c('adm_2' = 'adm_2'))

# Calculate magnitude and timing differences
# Admin 1 - Admin 3
adm_1_adm_3$intro_diff <- adm_1_adm_3$intro_time.y - adm_1_adm_3$intro_time.x
adm_1_adm_3$mag_diff <- adm_1_adm_3$magnitude.y - adm_1_adm_3$magnitude.x

# Admin 1 - Admin 2
adm_1_adm_2$intro_diff <- adm_1_adm_2$intro_time.y - adm_1_adm_2$intro_time.x
adm_1_adm_2$mag_diff <- adm_1_adm_2$magnitude.y - adm_1_adm_2$magnitude.x

# Admin 2 - Admin 3
adm_2_adm_3$intro_diff <- adm_2_adm_3$intro_time.y - adm_2_adm_3$intro_time.x
adm_2_adm_3$mag_diff <- adm_2_adm_3$magnitude.y - adm_2_adm_3$magnitude.x

# Set plot theme
theme_plot <- theme(axis.title = element_text(size=26),
                    axis.text = element_text(size=22),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    plot.title = element_text(size=30, hjust = 0.5))

# Create sub plots
plot_1 <- ggplot(data = adm_1_adm_3) +
  geom_point(aes(x = log(adm_3_pop), y = mag_diff), color = '#4292C6', alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = log(adm_3_pop), y = mag_diff), method = "lm", se = TRUE, color = '#4292C6', linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Introduction Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-6000000, 6000000,2000000), limits = c(-7000000, 7000000))


plot_2 <- ggplot(data = adm_1_adm_3) +
  geom_point(aes(x = log(adm_3_pop), y = intro_diff), color = '#F16913', alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = log(adm_3_pop), y = intro_diff), method = "lm", se = TRUE, color = '#F16913', linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Introduction Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-60,60,20), limits = c(-70, 70)) 


plot_3 <- ggplot(data = adm_1_adm_2) +
  geom_point(aes(x = log(adm_2_pop), y = mag_diff), color = '#4292C6', alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = log(adm_2_pop), y = mag_diff), method = "lm", se = TRUE, color = '#4292C6', linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Introduction Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-6000000, 6000000,2000000), limits = c(-7000000, 7000000))


plot_4 <- ggplot(data = adm_1_adm_2) +
  geom_point(aes(x = log(adm_2_pop), y = intro_diff), color = '#F16913', alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = log(adm_2_pop), y = intro_diff), method = "lm", se = TRUE, color = '#F16913', linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Introduction Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-60,60,20), limits = c(-70, 70)) 


plot_5 <- ggplot(data = adm_2_adm_3) +
  geom_point(aes(x = log(adm_3_pop), y = mag_diff), color = '#4292C6', alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = log(adm_3_pop), y = mag_diff), method = "lm", se = TRUE, color = '#4292C6', linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Introduction Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-6000000, 6000000,2000000), limits = c(-7000000, 7000000))


plot_6 <- ggplot(data = adm_2_adm_3) +
  geom_point(aes(x = log(adm_3_pop), y = intro_diff), color = '#F16913', alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = log(adm_3_pop), y = intro_diff), method = "lm", se = TRUE, color = '#F16913', linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Introduction Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-60,60,20), limits = c(-70, 70)) 

####################
# 3. CREATE FIGURE #
####################

figure <- plot_grid(plot_3 + ggtitle('Admin 1 - Admin 2'), 
                    plot_1 + ggtitle('Admin 1 - Admin 3'), 
                    plot_5 + ggtitle('Admin 2 - Admin 3'),
                    plot_4, plot_2, plot_6,
                    nrow = 2,
                    labels = c('(a)', '(b)', '(c)', 
                               '(d)', '(e)', '(f)'),
                    label_size = 26, hjust = 0)

# Save figure
ggsave('./figs/figure_model_diff_intro.jpg', plot = figure, height = 18, width = 26)

# Create poster figure
theme_plot <- theme(axis.title = element_text(size=50),
                    axis.text = element_text(size=35),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    plot.title = element_text(size=54, hjust = 0.5))

plot_1_intro <- ggplot(data = adm_1_adm_3) +
  geom_point(aes(x = log(adm_3_pop), y = mag_diff), color = '#4292C6', alpha = 0.3, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = log(adm_3_pop), y = mag_diff), method = "lm", se = TRUE, color = '#4292C6', linewidth = 4) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Intro. Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-6000000, 6000000,2000000), limits = c(-7000000, 7000000))


plot_2_intro <- ggplot(data = adm_1_adm_3) +
  geom_point(aes(x = log(adm_3_pop), y = intro_diff), color = '#F16913', alpha = 0.3, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = log(adm_3_pop), y = intro_diff), method = "lm", se = TRUE, color = '#F16913', linewidth = 4) + 
  theme_minimal() + ylab('Difference in Arrival Timing') + xlab('Intro. Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-60,60,20), limits = c(-70, 70)) 

save(list = c('plot_1_intro', 'plot_2_intro'), file = './tmp/intro_figs.RData')

################################################################################
################################################################################


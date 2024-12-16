################################################################################
# File Name: 05f_create_figure_model_diff                                      #
#                                                                              #
# Purpose:   Create figures that describe differences between models with      #
#            various spatial levels of mobility data, exploring introduction   #
#            locations, latent period, transmissibility, and duration of       #
#            infection.                                                        #
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

###################################
# Load introduction location data #
###################################

load('./tmp/introduction_location_model_results.RData')

# Load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Add on administrative name vectors
adm_1_at_1_mp$adm_1 <- adm_1_name_vec
adm_2_at_1_mp$adm_2 <- adm_2_name_vec
adm_1_at_1_sim$adm_1 <- adm_1_name_vec
adm_2_at_1_sim$adm_2 <- adm_2_name_vec

# Add on population vectors
adm_1_at_1_mp$adm_1_pop <- adm_1_pop_vec
adm_2_at_1_mp$adm_2_pop <- adm_2_pop_vec
adm_1_at_1_sim$adm_1_pop <- adm_1_pop_vec
adm_2_at_1_sim$adm_2_pop <- adm_2_pop_vec

# Add on administrative level crosswalks
adm_2_at_1_mp <- left_join(adm_2_at_1_mp, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_2_at_1_sim <- left_join(adm_2_at_1_sim, adm_2_x_walk, by = c('adm_2' = 'adm_2'))

# Merge together different model results at various levels
adm_1_adm_2_mp <- left_join(adm_2_at_1_mp, adm_1_at_1_mp, by = c('adm_1' = 'adm_1'))
adm_1_adm_2_sim <- left_join(adm_2_at_1_sim, adm_1_at_1_sim, by = c('adm_1' = 'adm_1'))

# Calculate magnitude and timing differences
# Mobile Phone Data
adm_1_adm_2_mp$intro_diff <- adm_1_adm_2_mp$intro_time.y - adm_1_adm_2_mp$intro_time.x
adm_1_adm_2_mp$mag_diff <- adm_1_adm_2_mp$magnitude.y - adm_1_adm_2_mp$magnitude.x

# Simulated Mobile Phone Data
adm_1_adm_2_sim$intro_diff <- adm_1_adm_2_sim$intro_time.y - adm_1_adm_2_sim$intro_time.x
adm_1_adm_2_sim$mag_diff <- adm_1_adm_2_sim$magnitude.y - adm_1_adm_2_sim$magnitude.x

# Set plot theme
theme_plot <- theme(axis.title = element_text(size=26),
                    axis.text = element_text(size=22),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    plot.title = element_text(size=30, hjust = 0.5))

# Create sub plots
mp_mag_int <- ggplot(data = adm_1_adm_2_mp) +
  geom_point(aes(x = log(adm_2_pop), y = mag_diff), color = 'darkgray', alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = log(adm_2_pop), y = mag_diff), method = "lm", se = TRUE, color = 'darkgray', linewidth = 2.5) + 
  theme_minimal() + ylab('Observed Mobility Data\n\nDifference in Magnitude') + xlab('Introduction Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000))

mp_inv_int <- ggplot(data = adm_1_adm_2_mp) +
  geom_point(aes(x = log(adm_2_pop), y = intro_diff), color = '#807DBA', alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = log(adm_2_pop), y = intro_diff), method = "lm", se = TRUE, color = '#807DBA', linewidth = 2.5) + 
  theme_minimal() + ylab('Observed Mobility Data\n\nDifference in Introduction Timing') + xlab('Introduction Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-80,80,40), limits = c(-100, 100))  

sim_mag_int <- ggplot(data = adm_1_adm_2_sim) +
  geom_point(aes(x = log(adm_2_pop), y = mag_diff), color = 'darkgray', alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = log(adm_2_pop), y = mag_diff), method = "lm", se = TRUE, color = 'darkgray', linewidth = 2.5) + 
  theme_minimal() + ylab('Simulated Mobility Data\n\nDifference in Magnitude') + xlab('Introduction Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000))

sim_inv_int <- ggplot(data = adm_1_adm_2_sim) +
  geom_point(aes(x = log(adm_2_pop), y = intro_diff), color = '#807DBA', alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = log(adm_2_pop), y = intro_diff), method = "lm", se = TRUE, color = '#807DBA', linewidth = 2.5) + 
  theme_minimal() + ylab('Simulated Mobility Data\n\nDifference in Introduction Timing') + xlab('Introduction Location Log Population') + theme_plot +
  scale_y_continuous(breaks = seq(-80,80,40), limits = c(-100, 100)) 

#############################
# Load tranmissibility data #
#############################

load('./tmp/transmissibility_model_results.RData')

# Mobile Phone Data
# Join comparison model results (Admin 2 and Admin 1)
adm_1_adm_2_col_mp <- left_join(adm_2_at_1_mp_col, adm_1_at_1_mp_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_2_mad_mp <- left_join(adm_2_at_1_mp_mad, adm_1_at_1_mp_mad, by = c('intro_loc' = 'intro_loc'))
# Create label variable
adm_1_adm_2_col_mp$`Introduction Location` <- 'Colombo (Urban)'
adm_1_adm_2_mad_mp$`Introduction Location` <- 'Madhu (Rural)'
# Append urban and rural introduction location results
adm_1_adm_2_mp <- rbind(adm_1_adm_2_col_mp, adm_1_adm_2_mad_mp)

# Simulated Mobile Phone Data
# Join comparison model results (Admin 2 and Admin 1)
adm_1_adm_2_col_sim <- left_join(adm_2_at_1_sim_col, adm_1_at_1_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_2_mad_sim <- left_join(adm_2_at_1_sim_mad, adm_1_at_1_sim_mad, by = c('intro_loc' = 'intro_loc'))
# Create label variable
adm_1_adm_2_col_sim$`Introduction Location` <- 'Colombo (Urban)'
adm_1_adm_2_mad_sim$`Introduction Location` <- 'Madhu (Rural)'
# Append urban and rural introduction location results
adm_1_adm_2_sim <- rbind(adm_1_adm_2_col_sim, adm_1_adm_2_mad_sim)

# Calculate magnitude and timing differences
# Mobile Phone Data
# Admin 1 - Admin 2
adm_1_adm_2_mp$intro_diff <- adm_1_adm_2_mp$intro_time.y - adm_1_adm_2_mp$intro_time.x
adm_1_adm_2_mp$mag_diff <- adm_1_adm_2_mp$magnitude.y - adm_1_adm_2_mp$magnitude.x

# Simulated Mobile Phone Data
# Admin 1 - Admin 2
adm_1_adm_2_sim$intro_diff <- adm_1_adm_2_sim$intro_time.y - adm_1_adm_2_sim$intro_time.x
adm_1_adm_2_sim$mag_diff <- adm_1_adm_2_sim$magnitude.y - adm_1_adm_2_sim$magnitude.x

# Set plot theme
theme_plot <- theme(axis.title = element_text(size=26),
                    axis.text = element_text(size=22),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.text = element_text(size = 26),
                    legend.title = element_text(size = 26),
                    plot.title = element_text(size=30, hjust = 0.5))

# Create sub plots
mp_mag_trans <- ggplot(data = adm_1_adm_2_mp) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Reproduction Number') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000))

mp_inv_trans <- ggplot(data = adm_1_adm_2_mp) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Reproduction Number') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-80,80,40), limits = c(-100, 100)) 

sim_mag_trans <- ggplot(data = adm_1_adm_2_sim) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Reproduction Number') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000))

sim_inv_trans <- ggplot(data = adm_1_adm_2_sim) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Reproduction Number') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-80,80,40), limits = c(-100, 100)) 

# Create legends
leg_1 <- ggplot(data = adm_1_adm_2_mp) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 4) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('R_0 Value') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'bottom') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000))

leg_2 <- ggplot(data = adm_1_adm_2_mp) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 4) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('R_0 Value') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'bottom') + 
  scale_y_continuous(breaks = seq(-80,80,40), limits = c(-100, 100)) 

# Grab legends
legend_1 <- get_legend(leg_1)
legend_2 <- get_legend(leg_2)

######################
# Load duration data #
######################

load('./tmp/duration_model_results.RData')

# Mobile Phone Data
# Join comparison model results (Admin 2 and Admin 1)
adm_1_adm_2_col_mp <- left_join(adm_2_at_1_mp_col, adm_1_at_1_mp_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_2_mad_mp <- left_join(adm_2_at_1_mp_mad, adm_1_at_1_mp_mad, by = c('intro_loc' = 'intro_loc'))
# Create label variable
adm_1_adm_2_col_mp$`Introduction Location` <- 'Colombo (Urban)'
adm_1_adm_2_mad_mp$`Introduction Location` <- 'Madhu (Rural)'
# Append urban and rural introduction location results
adm_1_adm_2_mp <- rbind(adm_1_adm_2_col_mp, adm_1_adm_2_mad_mp)

# Simulated Mobile Phone Data
# Join comparison model results (Admin 2 and Admin 1)
adm_1_adm_2_col_sim <- left_join(adm_2_at_1_sim_col, adm_1_at_1_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_2_mad_sim <- left_join(adm_2_at_1_sim_mad, adm_1_at_1_sim_mad, by = c('intro_loc' = 'intro_loc'))
# Create label variable
adm_1_adm_2_col_sim$`Introduction Location` <- 'Colombo (Urban)'
adm_1_adm_2_mad_sim$`Introduction Location` <- 'Madhu (Rural)'
# Append urban and rural introduction location results
adm_1_adm_2_sim <- rbind(adm_1_adm_2_col_sim, adm_1_adm_2_mad_sim)

# Calculate magnitude and timing differences
# Mobile Phone Data
# Admin 1 - Admin 2
adm_1_adm_2_mp$intro_diff <- adm_1_adm_2_mp$intro_time.y - adm_1_adm_2_mp$intro_time.x
adm_1_adm_2_mp$mag_diff <- adm_1_adm_2_mp$magnitude.y - adm_1_adm_2_mp$magnitude.x

# Simulated Mobile Phone Data
# Admin 1 - Admin 2
adm_1_adm_2_sim$intro_diff <- adm_1_adm_2_sim$intro_time.y - adm_1_adm_2_sim$intro_time.x
adm_1_adm_2_sim$mag_diff <- adm_1_adm_2_sim$magnitude.y - adm_1_adm_2_sim$magnitude.x

# Create sub plots
mp_mag_dur <- ggplot(data = adm_1_adm_2_mp) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Infectiousness Duration (days)') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000))

mp_inv_dur <- ggplot(data = adm_1_adm_2_mp) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Infectiousness Duration (days)') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-80,80,40), limits = c(-100, 100))  

sim_mag_dur <- ggplot(data = adm_1_adm_2_sim) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Infectiousness Duration (days)') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000))

sim_inv_dur <- ggplot(data = adm_1_adm_2_sim) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Infectiousness Duration (days)') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-80,80,40), limits = c(-100, 100)) 

####################
# Load latent data #
####################

load('./tmp/latent_model_results.RData')

# Mobile Phone Data
# Join comparison model results (Admin 2 and Admin 1)
adm_1_adm_2_col_mp <- left_join(adm_2_at_1_mp_col, adm_1_at_1_mp_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_2_mad_mp <- left_join(adm_2_at_1_mp_mad, adm_1_at_1_mp_mad, by = c('intro_loc' = 'intro_loc'))
# Create label variable
adm_1_adm_2_col_mp$`Introduction Location` <- 'Colombo (Urban)'
adm_1_adm_2_mad_mp$`Introduction Location` <- 'Madhu (Rural)'
# Append urban and rural introduction location results
adm_1_adm_2_mp <- rbind(adm_1_adm_2_col_mp, adm_1_adm_2_mad_mp)

# Simulated Mobile Phone Data
# Join comparison model results (Admin 2 and Admin 1)
adm_1_adm_2_col_sim <- left_join(adm_2_at_1_sim_col, adm_1_at_1_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_2_mad_sim <- left_join(adm_2_at_1_sim_mad, adm_1_at_1_sim_mad, by = c('intro_loc' = 'intro_loc'))
# Create label variable
adm_1_adm_2_col_sim$`Introduction Location` <- 'Colombo (Urban)'
adm_1_adm_2_mad_sim$`Introduction Location` <- 'Madhu (Rural)'
# Append urban and rural introduction location results
adm_1_adm_2_sim <- rbind(adm_1_adm_2_col_sim, adm_1_adm_2_mad_sim)

# Calculate magnitude and timing differences
# Mobile Phone Data
# Admin 1 - Admin 2
adm_1_adm_2_mp$intro_diff <- adm_1_adm_2_mp$intro_time.y - adm_1_adm_2_mp$intro_time.x
adm_1_adm_2_mp$mag_diff <- adm_1_adm_2_mp$magnitude.y - adm_1_adm_2_mp$magnitude.x

# Simulated Mobile Phone Data
# Admin 1 - Admin 2
adm_1_adm_2_sim$intro_diff <- adm_1_adm_2_sim$intro_time.y - adm_1_adm_2_sim$intro_time.x
adm_1_adm_2_sim$mag_diff <- adm_1_adm_2_sim$magnitude.y - adm_1_adm_2_sim$magnitude.x

# Create sub plots
mp_mag_lat <- ggplot(data = adm_1_adm_2_mp) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Latent Period (days)') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000))

mp_inv_lat <- ggplot(data = adm_1_adm_2_mp) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Latent Period (days)') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-80,80,40), limits = c(-100, 100)) 

sim_mag_lat <- ggplot(data = adm_1_adm_2_sim) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Latent Period (days)') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000))

sim_inv_lat <- ggplot(data = adm_1_adm_2_sim) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Latent Period (days)') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-80,80,40), limits = c(-100, 100)) 

figure_1 <- plot_grid(mp_mag_int + ggtitle('Altered Introduction \nLocation'), 
                    mp_mag_lat + ggtitle('Altered Latent Period\n'),
                    mp_mag_trans + ggtitle('Altered Transmissibility\n'),
                    mp_mag_dur + ggtitle('Altered Infectiousness \nDuration'),
                    sim_mag_int + ggtitle('\n'), 
                    sim_mag_lat + ggtitle('\n'),
                    sim_mag_trans + ggtitle('\n'),
                    sim_mag_dur + ggtitle('\n'),
                    nrow = 2,
                    labels = c('(a)', '(b)', '(c)', '(d)', 
                               '(e)', '(f)', '(g)', '(h)'),
                    label_size = 26, hjust = 0)

figure_2 <- plot_grid(mp_inv_int + ggtitle('Altered Introduction \nLocation'), 
                      mp_inv_lat + ggtitle('Altered Latent Period\n'),
                      mp_inv_trans + ggtitle('Altered Transmissibility\n'),
                      mp_inv_dur + ggtitle('Altered Infectiousness \nDuration'),
                      sim_inv_int + ggtitle('\n'), 
                      sim_inv_lat + ggtitle('\n'),
                      sim_inv_trans + ggtitle('\n'),
                      sim_inv_dur + ggtitle('\n'),
                      nrow = 2,
                      labels = c('(i)', '(j)', '(k)', '(l)', 
                                 '(m)', '(n)', '(o)', '(p)'),
                      label_size = 26, hjust = 0)

figure <- plot_grid(figure_1, legend_1, figure_2, legend_2,
                      nrow = 4, rel_heights = c(1, 0.10, 1, 0.10))


# Save figure
ggsave('./figs/figure_model_2_1.jpg', plot = figure, height = 28, width = 26)



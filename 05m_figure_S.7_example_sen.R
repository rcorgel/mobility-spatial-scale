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

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load model 
source('./mobility-spatial-scale/04_metapop_model.R')

#################################
# 2. SIMULATE EXAMPLE EPIDEMICS #
#################################

# First, load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Load simulated mobility data
load('./mobility-spatial-scale/simulated data/mobile_phone_sim_prop_dat.RData')
load('./tmp/rescale_phone_mobility_dat.RData')

##########################
# Administrative Level 3 #
##########################

# Create object for mobility data
mobility_dat_adm_3 <- list(adm_3_phone_mobility_mat, as.matrix(adm_3_phone_mobility_mat_rescale_adm_1))

adm_3_sim_col <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                      R_0 = 1.8, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                      adm_name_vec = adm_3_name_vec, adm_level = '3', 
                                      pop_vec = adm_3_pop_vec, intro_adm = 'Colombo', intro_num = 1,
                                      adm_x_walk = adm_3_x_walk, travel_mat = mobility_dat_adm_3[[2]], 
                                      max_time = 365, time_step = 1, mobility = TRUE)

adm_3_sim_mad <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                      R_0 = 1.8, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                      adm_name_vec = adm_3_name_vec, adm_level = '3', 
                                      pop_vec = adm_3_pop_vec, intro_adm = 'Madhu', intro_num = 1,
                                      adm_x_walk = adm_3_x_walk, travel_mat = mobility_dat_adm_3[[2]], 
                                      max_time = 365, time_step = 1, mobility = TRUE)

##########################
# Administrative Level 2 #
##########################

# Create object for mobility data
mobility_dat_adm_2 <- list(adm_2_phone_mobility_mat, as.matrix(adm_2_phone_mobility_mat_rescale))

adm_2_sim_col <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                      R_0 = 1.8, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                      adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                      pop_vec = adm_2_pop_vec, intro_adm = 'Colombo', intro_num = 1,
                                      adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_2[[2]], 
                                      max_time = 365, time_step = 1, mobility = TRUE)

adm_2_sim_mad <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                      R_0 = 1.8, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                      adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                      pop_vec = adm_2_pop_vec, intro_adm = 'Madhu', intro_num = 1,
                                      adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_2[[2]], 
                                      max_time = 365, time_step = 1, mobility = TRUE)

########################
# 3. CREATE SUBFIGURES #
########################

# Load observed results
load('./tmp/figure_4_plots_obs.RData')

###########################
# Make introduction lines #
###########################

############
# Rescaled #
############

adm_3_sim_col_avg <- adm_3_sim_col |>
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(time, adm_3) |>
  mutate(avg_incid_I_adm_3 = mean(incid_I)) |>
  distinct(time, adm_3, avg_incid_I_adm_3) |> 
  ungroup() |>
  group_by(time) |> 
  mutate(avg_incid_I = sum(avg_incid_I_adm_3)) |>
  distinct(time, avg_incid_I) |> ungroup() |>
  mutate(cum_sum_I = cumsum(avg_incid_I)) |>
  mutate(Scale = 'Division')

adm_2_sim_col_avg <- adm_2_sim_col |>
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(time, adm_2) |>
  mutate(avg_incid_I_adm_2 = mean(incid_I)) |>
  distinct(time, adm_2, avg_incid_I_adm_2) |> 
  ungroup() |>
  group_by(time) |> 
  mutate(avg_incid_I = sum(avg_incid_I_adm_2)) |>
  distinct(time, avg_incid_I) |> ungroup() |>
  mutate(cum_sum_I = cumsum(avg_incid_I)) |>
  mutate(Scale = 'District')

adm_1_sim_col_avg <- line_col_obs_all[line_col_obs_all$Scale == 'Province',]

line_col_sim_all <- rbind(adm_3_sim_col_avg, adm_2_sim_col_avg, adm_1_sim_col_avg)

adm_3_sim_mad_avg <- adm_3_sim_mad |>
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(time, adm_3) |>
  mutate(avg_incid_I_adm_3 = mean(incid_I)) |>
  distinct(time, adm_3, avg_incid_I_adm_3) |> 
  ungroup() |>
  group_by(time) |> 
  mutate(avg_incid_I = sum(avg_incid_I_adm_3)) |>
  distinct(time, avg_incid_I) |> ungroup() |>
  mutate(cum_sum_I = cumsum(avg_incid_I)) |>
  mutate(Scale = 'Division')

adm_2_sim_mad_avg <- adm_2_sim_mad |>
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  group_by(time, adm_2) |>
  mutate(avg_incid_I_adm_2 = mean(incid_I)) |>
  distinct(time, adm_2, avg_incid_I_adm_2) |> 
  ungroup() |>
  group_by(time) |> 
  mutate(avg_incid_I = sum(avg_incid_I_adm_2)) |>
  distinct(time, avg_incid_I) |> ungroup() |>
  mutate(cum_sum_I = cumsum(avg_incid_I)) |>
  mutate(Scale = 'District')

adm_1_sim_mad_avg <- line_mad_obs_all[line_mad_obs_all$Scale == 'Province',]

line_mad_sim_all <- rbind(adm_3_sim_mad_avg, adm_2_sim_mad_avg, adm_1_sim_mad_avg)

######################
# Introduction count #
######################

############
# Rescaled #
############

adm_3_at_1_sim_col_int <- adm_3_sim_col |>
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

adm_2_at_1_sim_col_int <- adm_2_sim_col |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Calculate average across simulations
  group_by(time, adm_2) |>
  mutate(avg_incid_I_adm_2 = mean(incid_I)) |>
  distinct(time, adm_2, adm_1, avg_incid_I_adm_2) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(time, adm_1) |> 
  mutate(avg_incid_I_adm_1 = sum(avg_incid_I_adm_2)) |>
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
         Scale = 'District',
         Count = seq(1, 9, 1)) |>
  dplyr::select(time, adm_1, time, Count, Scale)

adm_1_sim_col_int <- int_col_obs_all[int_col_obs_all$Scale == 'Province',]

int_col_sim_all <- rbind(adm_1_sim_col_int, adm_2_at_1_sim_col_int, adm_3_at_1_sim_col_int)

adm_3_at_1_sim_mad_int <- adm_3_sim_mad |>
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

adm_2_at_1_sim_mad_int <- adm_2_sim_mad |>
  # Restrict to simulations that took off
  group_by(run_num) |>
  filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Calculate average across simulations
  group_by(time, adm_2) |>
  mutate(avg_incid_I_adm_2 = mean(incid_I)) |>
  distinct(time, adm_2, adm_1, avg_incid_I_adm_2) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(time, adm_1) |> 
  mutate(avg_incid_I_adm_1 = sum(avg_incid_I_adm_2)) |>
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
         Scale = 'District',
         Count = seq(1, 9, 1)) |>
  dplyr::select(time, adm_1, time, Count, Scale)

adm_1_sim_mad_int <- int_mad_obs_all[int_mad_obs_all$Scale == 'Province',]

int_mad_sim_all <- rbind(adm_1_sim_mad_int, adm_2_at_1_sim_mad_int, adm_3_at_1_sim_mad_int)





line_plot_col_obs <- ggplot(data = int_col_obs_all) + geom_line(aes(x = time, y = Count, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Count') + xlab('Time (days)') + ggtitle('Units Infected') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_y_continuous(breaks=c(0, 3, 6, 9)) +
  scale_color_manual(values = c("#41AE76", "#9e9ac8", "#4292C6"))


line_plot_mad_obs <- ggplot(data = int_mad_obs_all) + geom_line(aes(x = time, y = Count, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Count') + xlab('Time (days)') + ggtitle('Units Infected') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_y_continuous(breaks=c(0, 3, 6, 9)) +
  scale_color_manual(values = c("#41AE76", "#9e9ac8", "#4292C6"))

line_plot_col_sim <- ggplot(data = int_col_sim_all) + geom_line(aes(x = time, y = Count, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Count') + xlab('Time (days)') + ggtitle('Units Infected') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_y_continuous(breaks=c(0, 3, 6, 9)) +
  scale_color_manual(values = c("#41AE76", "#9e9ac8", "#4292C6"))


line_plot_mad_sim <- ggplot(data = int_mad_sim_all) + geom_line(aes(x = time, y = Count, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Count') + xlab('Time (days)') + ggtitle('Units Infected') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_y_continuous(breaks=c(0, 3, 6, 9)) +
  scale_color_manual(values = c("#41AE76", "#9e9ac8", "#4292C6"))

dis_plot_col_obs <- ggplot(data = line_col_obs_all) + geom_line(aes(x = time, y = avg_incid_I, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Disease Curves') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_color_manual(values = c("#41AE76", "#9e9ac8", "#4292C6"))


dis_plot_mad_obs <- ggplot(data = line_mad_obs_all) + geom_line(aes(x = time, y = avg_incid_I, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Disease Curves') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_color_manual(values = c("#41AE76", "#9e9ac8", "#4292C6"))

dis_plot_col_sim <- ggplot(data = line_col_sim_all) + geom_line(aes(x = time, y = avg_incid_I, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Disease Curves') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_color_manual(values = c("#41AE76", "#9e9ac8", "#4292C6"))


dis_plot_mad_sim <- ggplot(data = line_mad_sim_all) + geom_line(aes(x = time, y = avg_incid_I, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Disease Curves') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_color_manual(values = c("#41AE76", "#9e9ac8", "#4292C6"))


legend <- ggplot(data = line_mad_sim_all) + geom_line(aes(x = time, y = avg_incid_I, color = Scale), linewidth = 3.5, alpha = 0.8) + 
  theme_minimal() + ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Disease Curves') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_color_manual(values = c("#41AE76", "#9e9ac8", "#4292C6"))
legend_get <- get_legend(legend)

row_1_1 <- cowplot::plot_grid(line_plot_col_obs, dis_plot_col_obs,
                              nrow = 1, labels = c('', ''))
row_1_2 <- cowplot::plot_grid(line_plot_mad_obs, dis_plot_mad_obs, 
                              nrow = 1, labels = c('', ''))
row_2_1 <- cowplot::plot_grid(line_plot_col_sim, dis_plot_col_sim, 
                              nrow = 1, labels = c('', ''))
row_2_2 <- cowplot::plot_grid(line_plot_mad_sim, dis_plot_mad_sim, 
                              nrow = 1, labels = c('', ''))

figure_4 <- cowplot::plot_grid(ggplot() + theme_void(), ggplot() + theme_void(),
                               row_1_1, row_1_2,
                               ggplot() + theme_void(), ggplot() + theme_void(),
                               row_2_1, row_2_2,
                               nrow = 4, ncol = 2, 
                               rel_heights = c(0.10, 1, 0.10, 1),
                               labels = c('Observed Data, Colombo Introduction', 
                                          'Observed Data, Madhu Introduction', 
                                          '(a)', '(b)', 
                                          'Rescaled Data, Colombo Introduction', 
                                          ' Data, Madhu Introduction', 
                                          '(c)', '(d)'),
                               label_size = 26, hjust = -0.5)    

figure_4_final <- cowplot::plot_grid(figure_4, legend_get, nrow = 2, rel_heights = c(1, 0.05))

ggsave('./figs/SENS_TEST.jpg', plot = figure_4_final, height = 13, width = 25)

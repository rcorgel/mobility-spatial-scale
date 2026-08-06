################################################################################
# File Name: 05j_figure_4                                                      #
#                                                                              #
# Purpose:   Create figure 4 for the manuscript.                               #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load simulated epidemics                                       #
#            3. Create subfigures                                              #
#            4. Create final figure                                            #
#                                                                              #
# Project:   Mobility Spatial Scale                                            #
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

###############################
# 2. LOAD SIMULATED EPIDEMICS #
###############################

adm_3_obs_col <- readRDS('./out/adm_3_obs_col_1.5.rds')
adm_2_obs_col <- readRDS('./out/adm_2_obs_col_1.5.rds')
adm_1_obs_col <- readRDS('./out/adm_1_obs_col_1.5.rds')

adm_3_obs_mad <- readRDS('./out/adm_3_obs_del_1.5.rds')
adm_2_obs_mad <- readRDS('./out/adm_2_obs_del_1.5.rds')
adm_1_obs_mad <- readRDS('./out/adm_1_obs_del_1.5.rds')

########################
# 3. CREATE SUBFIGURES #
########################

########################
# EPIDEMIC PROBABILITY #
########################

# Colombo Introduction Event
# Admin 3
take_off_3 <- adm_3_obs_col |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'Division') |> 
  distinct(Scale, take_off_perc)

# Admin 2
take_off_2 <- adm_2_obs_col |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'District') |> 
  distinct(Scale, take_off_perc)

# Admin 1
take_off_1 <- adm_1_obs_col |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'Province') |> 
  distinct(Scale, take_off_perc)

# Combine
take_off_col <- rbind(take_off_1, take_off_2, take_off_3)

# Sevanagala Introduction Event
# Admin 3
take_off_3_mad <- adm_3_obs_mad |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'Division') |> 
  distinct(Scale, take_off_perc)

# Admin 2
take_off_2_mad <- adm_2_obs_mad |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'District') |> 
  distinct(Scale, take_off_perc)

# Admin 1
take_off_1_mad <- adm_1_obs_mad |> group_by(run_num) |>
  mutate(sum = sum(incid_I)) |> 
  distinct(run_num, sum) |>
  ungroup() |>
  mutate(take_off = ifelse(sum > 100, 1, 0),
         take_off_perc = sum(take_off) / 100,
         Scale = 'Province') |> 
  distinct(Scale, take_off_perc)
  
# Combine
take_off_mad <- rbind(take_off_1_mad, take_off_2_mad, take_off_3_mad)

###################
# EPIDEMIC CURVES #
###################

# Colombo Introduction Event
# Admin 3
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
  dplyr::filter(adm_1 == 'Uva' | adm_1 == 'Western')

# Admin 2
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
  dplyr::filter(adm_1 == 'Uva' | adm_1 == 'Western')

# Admin 1
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
  dplyr::filter(adm_1 == 'Uva' | adm_1 == 'Western')

# Combine
line_col_obs_all <- rbind(adm_3_obs_col_avg, adm_2_obs_col_avg, adm_1_obs_col_avg)

# Sevanagala Introduction Event
# Admin 3
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
  dplyr::filter(adm_1 == 'Uva' | adm_1 == 'Western')

# Admin 2
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
  dplyr::filter(adm_1 == 'Uva' | adm_1 == 'Western')

# Admin 1
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
  dplyr::filter(adm_1 == 'Uva' | adm_1 == 'Western')

# Combine
line_mad_obs_all <- rbind(adm_3_obs_mad_avg, adm_2_obs_mad_avg, adm_1_obs_mad_avg)

#######################
# INTRODUCTION TIMING #
#######################

# Colombo Introduction Event
# Admin 3
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

# Amdin 2
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

# Admin 1
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

# Combine
int_col_obs_all <- rbind(adm_1_obs_col_int, adm_2_at_1_obs_col_int, adm_3_at_1_obs_col_int)

# Create data summary function
data_summary <- function(x) {
  m <- median(x)
  ymin <- quantile(x, probs = 0.25)
  ymax <- quantile(x, probs = 0.75)
  return(c(y = m,ymin = ymin,ymax = ymax))
}

# Re-arrange scales
int_col_obs_all <- int_col_obs_all |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 

# Sevanagala Introduction Event
# Admin 3
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

# Admin 2
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

# Admin 1
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

# Combine
int_mad_obs_all <- rbind(adm_1_obs_mad_int, adm_2_at_1_obs_mad_int, adm_3_at_1_obs_mad_int)

# Re-arrange scales
int_mad_obs_all <- int_mad_obs_all |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 

# Create Sevanagala order set
adm_1_order_mad <- adm_1_obs_mad_int |>
  group_by(adm_1) |>
  mutate(median = median(time)) |>
  distinct(adm_1, median) |>
  ungroup() |>
  arrange(median) |>
  mutate(Order = row_number())

# Create Colombo order set
adm_1_order_col <- adm_1_obs_col_int |>
  group_by(adm_1) |>
  mutate(median = median(time)) |>
  distinct(adm_1, median) |>
  ungroup() |>
  arrange(median) |>
  mutate(Order = row_number())

# Merge on order sets to data
int_mad_obs_all <- left_join(int_mad_obs_all, adm_1_order_mad, by = c('adm_1' = 'adm_1'))
int_col_obs_all <- left_join(int_col_obs_all, adm_1_order_col, by = c('adm_1' = 'adm_1'))

########
# PLOT #
########

# Plot introduction timing
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
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) 
line_plot_mad_obs

# Plot disease curves
dis_plot_col_obs <- ggplot(line_col_obs_all, aes(x = time, y = perc_50)) +
  geom_ribbon(aes(ymin = perc_05, ymax = perc_95, fill = Scale), alpha = 0.2) +
  geom_line(aes(color = Scale), size = 1.75) + #xlim(0, 100) + ylim(0, 50) +
  theme_minimal() + scale_color_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
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
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        strip.text = element_text(size = 30)) +
  ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Province Disease Curves') + 
  facet_wrap(vars(adm_1), nrow = 2, scale = 'free') 
dis_plot_mad_obs

# Re-order epidemic probability
take_off_col <- take_off_col |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 

take_off_mad <- take_off_mad |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 

# Plot E\epidemic probability
take_off_mad_plot <- ggplot(take_off_mad, aes(x=Scale, y=take_off_perc, fill = Scale)) + 
  geom_bar(stat = "identity", width=0.42, color = 'black', alpha = 0.9) +
  theme_minimal() + scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
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
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6), limits = c(0, 0.65)) +
  ylab('Proportion') + xlab('Scale') + ggtitle('Epidemic Occurance')

# Create legend
line_mad_obs_all$Scale <- factor(line_mad_obs_all$Scale, levels=c('Division', 'District', 'Province'))
legend <- ggplot(data = line_mad_obs_all) + geom_line(aes(x = time, y = perc_50, color = Scale), linewidth = 3.5, alpha = 1) + 
  geom_ribbon(aes(x = time, ymin = perc_05, ymax = perc_95, fill = Scale), alpha = 0.2) +
  theme_minimal() + ylab('Incident Cases') + xlab('Time (days)') + ggtitle('Disease Curves') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 40),
        legend.title = element_text(size = 40),
        legend.key.size = unit(1.2, "cm")) +
  scale_color_manual(values = c( "#41AE76", "#9e9ac8","#4292C6")) +
  scale_fill_manual(values = c( "#41AE76","#9e9ac8", "#4292C6"))

legend_get <- get_legend(legend)

##########################
# 4. CREATE FINAL FIGURE #
##########################

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
                                                 'Sevanagala Introduction Event', '', ''),
                            label_size = 34, hjust = 0,
                            rel_heights = c(0.08, 1, 0.08, 1, 0.1))

# CALL OUTS #
line_mad_obs_all |> group_by(Scale) |> mutate(sum= sum(perc_50)) |> distinct(Scale, sum)
line_col_obs_all |> group_by(Scale) |> mutate(sum= sum(perc_50)) |> distinct(Scale, sum)
test <- int_col_obs_all |> group_by(adm_1, Scale) |> mutate(med = median(time)) |> distinct(adm_1, Scale, med)
test <- int_mad_obs_all |> group_by(adm_1, Scale) |> mutate(med = median(time)) |> distinct(adm_1, Scale, med)

max(line_col_obs_all[line_col_obs_all$adm_1 == 'Western' & line_col_obs_all$Scale == 'Division',]$perc_50)
max(line_col_obs_all[line_col_obs_all$adm_1 == 'Western' & line_col_obs_all$Scale == 'District',]$perc_50)

test <- int_mad_obs_all |>
  group_by(adm_1, Scale) |>
  mutate(median = median(time)) |>
  distinct(adm_1, median) |>
  ungroup() |>
  arrange(median) |>
  mutate(Order = row_number())

test_2 <- int_col_obs_all |>
  group_by(adm_1, Scale) |>
  mutate(median = median(time)) |>
  distinct(adm_1, median) |>
  ungroup() |>
  arrange(median) |>
  mutate(Order = row_number())

ggsave('./figs/figure_4_example.jpg', plot = plot , height = 17, width = 25)

################################################################################
################################################################################

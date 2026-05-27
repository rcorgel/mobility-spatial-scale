################################################################################
# File Name: 05l_figure_5_sensitivity                                          #
#                                                                              #
# Purpose:   Create figure 5 for the manuscript.                               #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load simulated epidemics                                       #
#            3. Create sub figures                                             #
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
library(parallel)
library(ggpubr)

# Set the seed
set.seed(123456)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load model 
source('./mobility-spatial-scale/04_metapop_model.R')

###############################
# 2. LOAD SIMULATED EPIDEMICS #
###############################

# First, load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')
load('./tmp/rescale_phone_mobility_dat.RData')
adm_3_x_walk_new <- readRDS('./out/adm_3_x_walk_new.rds')

# Load results
# Sevanagala Introduction
# R_0 = 1.5
adm_3_obs_del <- readRDS('./out/adm_3_obs_del_1.5.rds')
adm_1_obs_del <- readRDS('./out/adm_1_obs_del_1.5.rds')

# R_0 = 1.1
adm_3_obs_del_1.1 <- readRDS('./out/adm_3_obs_del_1.1.rds')
adm_1_obs_del_1.1 <- readRDS('./out/adm_1_obs_del_1.1.rds')

# R_0 = 3.0
adm_3_obs_del_3.0 <- readRDS('./out/adm_3_obs_del_3.0.rds')
adm_1_obs_del_3.0 <- readRDS('./out/adm_1_obs_del_3.0.rds')

# Rescaled data
adm_3_at_1_sen_del_1.5 <- readRDS('./out/adm_3_at_1_sen_del_1.5.rds')
adm_3_at_1_sen_del_1.1 <- readRDS('./out/adm_3_at_1_sen_del_1.1.rds')
adm_3_at_1_sen_del_3.0 <- readRDS('./out/adm_3_at_1_sen_del_3.0.rds')

#########################
# 3. CREATE SUB FIGURES #
#########################

################################
# CREATE AGGREGATION FUNCTIONS #
################################

# Aggregate simulation to the adm 1 scale
agg_to_adm_1 <- function(results, intro_loc, scale, trans) {
  results |> group_by(time, adm_1, run_num) |>
    # Calculate incidence at Admin 1
    mutate(incid_I_adm_1 = sum(incid_I)) |>
    distinct(time, run_num, adm_1, incid_I_adm_1) |> 
    ungroup() |>
    # Calculate burn out and drop runs that did not take off
    group_by(run_num) |>
    mutate(run_total = sum(incid_I_adm_1)) |>
    ungroup() |>
    mutate(burn_out_perc = sum(run_total > 100) / n()) |>
    filter(run_total > 100) |>
    # Calculate total epidemic size
    group_by(run_num) |>
    mutate(magnitude = sum(incid_I_adm_1)) |>
    ungroup() |>
    group_by(run_num, adm_1) |>
    # Calculate cumulative cases by Admin 1
    # Indicate when cumulative cases > 1 for each unit
    mutate(cum_I = cumsum(incid_I_adm_1),
           intro = ifelse(cum_I > 1, 1, 0)) |>
    # Indicate the first instance when cumulative > 1
    mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
    # Filter to first instance for all Admin 1
    filter(intro_first == TRUE) |>
    ungroup() |>
    group_by(run_num) |>
    arrange(time) |>
    slice(1:9) |>
    mutate(time = max(time),
           intro_loc = intro_loc,
           Scale = scale,
           Trans = trans) |>
    distinct(run_num, time, magnitude, intro_loc, Scale, Trans)
}

# Administrative level spatial introduction to the adm 1 scale
invasion_run_adm_1 <- function(results, intro_loc, scale, type) {
  results |> 
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
    mutate(intro_loc = intro_loc,
           Scale = scale,
           `Mobility Data Type` = type,
           Count = row_number()) |>
    dplyr::select(run_num, time, adm_1, time, Count, Scale, `Mobility Data Type`)
}

##################################
# AGGREGATE OBSERVED SIMULATIONS #
##################################

#############
# R_0 = 1.5 #
#############

adm_3_at_1_del <- agg_to_adm_1(results = adm_3_obs_del, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '1.5') 
adm_3_at_1_del_inv <- invasion_run_adm_1(results = adm_3_obs_del, intro_loc = 'Sevanagala', scale = 'Division', type = 'Original') 
adm_1_at_1_del <- agg_to_adm_1(results = adm_1_obs_del, intro_loc = 'Sevanagala', scale = '1', trans = '1.5') 
adm_1_at_1_del_inv <- invasion_run_adm_1(results = adm_1_obs_del, intro_loc = 'Sevanagala', scale = 'Province', type = 'Original') 

#############
# R_0 = 1.1 #
#############

adm_3_at_1_del_1.1 <- agg_to_adm_1(results = adm_3_obs_del_1.1, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '1.1') 
adm_3_at_1_del_inv_1.1 <- invasion_run_adm_1(results = adm_3_obs_del_1.1, intro_loc = 'Sevanagala', scale = 'Division', type = 'Original') 
adm_1_at_1_del_1.1 <- agg_to_adm_1(results = adm_1_obs_del_1.1, intro_loc = 'Sevanagala', scale = '1', trans = '1.1') 
adm_1_at_1_del_inv_1.1 <- invasion_run_adm_1(results = adm_1_obs_del_1.1, intro_loc = 'Sevanagala', scale = 'Province', type = 'Original') 

#############
# R_0 = 3.0 #
#############

adm_3_at_1_del_3.0 <- agg_to_adm_1(results = adm_3_obs_del_3.0, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '3.0') 
adm_3_at_1_del_inv_3.0 <- invasion_run_adm_1(results = adm_3_obs_del_3.0, intro_loc = 'Sevanagala', scale = 'Division', type = 'Original') 
adm_1_at_1_del_3.0 <- agg_to_adm_1(results = adm_1_obs_del_3.0, intro_loc = 'Sevanagala', scale = '1', trans = '3.0') 
adm_1_at_1_del_inv_3.0 <- invasion_run_adm_1(results = adm_1_obs_del_3.0, intro_loc = 'Sevanagala', scale = 'Province', type = 'Original') 

##################################
# AGGREGATE RESCALED SIMULATIONS #
##################################

#############
# R_0 = 1.5 #
#############

adm_3_at_1_del_sen <- agg_to_adm_1(results = adm_3_at_1_sen_del_1.5, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '1.5') 
adm_3_at_1_del_sen_inv <- invasion_run_adm_1(results =  adm_3_at_1_sen_del_1.5, intro_loc = 'Sevanagala', scale = 'Division', type = 'Rescaled') 

#############
# R_0 = 1.1 #
#############

adm_3_at_1_del_sen_1.1 <- agg_to_adm_1(results = adm_3_at_1_sen_del_1.1, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '1.1') 
adm_3_at_1_del_sen_inv_1.1 <- invasion_run_adm_1(results =  adm_3_at_1_sen_del_1.1, intro_loc = 'Sevanagala', scale = 'Division', type = 'Rescaled') 

#############
# R_0 = 3.0 #
#############

adm_3_at_1_del_sen_3.0 <- agg_to_adm_1(results = adm_3_at_1_sen_del_3.0, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '3.0') 
adm_3_at_1_del_sen_inv_3.0 <- invasion_run_adm_1(results =  adm_3_at_1_sen_del_3.0, intro_loc = 'Sevanagala', scale = 'Division', type = 'Rescaled') 

# Add data type variable to the aggregated data

adm_3_at_1_del <- adm_3_at_1_del |> mutate(`Mobility Data Type` = 'Original')
adm_1_at_1_del <- adm_1_at_1_del |> mutate(`Mobility Data Type` = 'Original')

adm_3_at_1_del_1.1 <- adm_3_at_1_del_1.1 |> mutate(`Mobility Data Type` = 'Original')
adm_1_at_1_del_1.1 <- adm_1_at_1_del_1.1 |> mutate(`Mobility Data Type` = 'Original')

adm_3_at_1_del_3.0 <- adm_3_at_1_del_3.0 |> mutate(`Mobility Data Type` = 'Original')
adm_1_at_1_del_3.0 <- adm_1_at_1_del_3.0 |> mutate(`Mobility Data Type` = 'Original')

adm_3_at_1_del_sen <- adm_3_at_1_del_sen |> mutate(`Mobility Data Type` = 'Rescaled')
adm_3_at_1_del_sen_1.1 <- adm_3_at_1_del_sen_1.1 |> mutate(`Mobility Data Type` = 'Rescaled')
adm_3_at_1_del_sen_3.0 <- adm_3_at_1_del_sen_3.0 |> mutate(`Mobility Data Type` = 'Rescaled')

###############
# FORMAT DATA #
###############

#############
# R_0 = 1.5 #
#############

# Combine admin 3, admin 1, and the rescaled data
int_del_obs_all_1 <- rbind(adm_3_at_1_del_inv, adm_1_at_1_del_inv, adm_3_at_1_del_sen_inv)

# Create order set
adm_1_order_del <- adm_1_at_1_del_inv |>
  group_by(adm_1) |>
  mutate(median = median(time)) |>
  distinct(adm_1, median) |>
  ungroup() |>
  arrange(median) |>
  mutate(Order = row_number())

# Calculate the median
del_1.5 <- int_del_obs_all_1 |>
  group_by(adm_1, Scale, `Mobility Data Type`) |>
  mutate(median = median(time)) |>
  distinct(adm_1, Scale, `Mobility Data Type`, median)

# Separate province
del_1.5_prov <- del_1.5 |> filter(Scale == 'Province')

# Merge on province
del_1.5 <- left_join(del_1.5, del_1.5_prov[,c(1, 4)], by = 'adm_1')

# Calculate RMSE
del_1.5_med <- del_1.5 |> group_by(Scale, `Mobility Data Type`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale, `Mobility Data Type`, rmse) |>
  mutate(trans = '1.5')

#############
# R_0 = 1.1 #
#############

# Combine admin 3, admin 1, and the rescaled data
int_del_obs_all_1.1 <- rbind(adm_3_at_1_del_inv_1.1, adm_1_at_1_del_inv_1.1, adm_3_at_1_del_sen_inv_1.1)

# Calculate the median
del_1.1 <- int_del_obs_all_1.1 |>
  group_by(adm_1, Scale, `Mobility Data Type`) |>
  mutate(median = median(time)) |>
  distinct(adm_1, Scale, `Mobility Data Type`, median)

# Separate province
del_1.1_prov <- del_1.1 |> filter(Scale == 'Province')

# Merge on province
del_1.1 <- left_join(del_1.1, del_1.1_prov[,c(1, 4)], by = 'adm_1')

# Calculate RMSE
del_1.1_med <- del_1.1 |> group_by(Scale, `Mobility Data Type`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale, `Mobility Data Type`, rmse) |>
  mutate(trans = '1.1')

#############
# R_0 = 3.0 #
#############

# Combine admin 3, admin 1, and the rescaled data
int_del_obs_all_3.0 <- rbind(adm_3_at_1_del_inv_3.0, adm_1_at_1_del_inv_3.0, adm_3_at_1_del_sen_inv_3.0)

# Calculate the median
del_3.0 <- int_del_obs_all_3.0 |>
  group_by(adm_1, Scale, `Mobility Data Type`) |>
  mutate(median = median(time)) |>
  distinct(adm_1, Scale, `Mobility Data Type`, median)

# Separate province
del_3.0_prov <- del_3.0 |> filter(Scale == 'Province')

# Merge on province
del_3.0 <- left_join(del_3.0, del_3.0_prov[,c(1, 4)], by = 'adm_1')

# Calculate RMSE
del_3.0_med <- del_3.0 |> group_by(Scale, `Mobility Data Type`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale, `Mobility Data Type`, rmse) |>
  mutate(trans = '3.0')

###################
# CREATE BAR PLOT #
###################

# Combine RMSEs
rmse_del <- rbind(del_1.5_med, del_1.1_med, del_3.0_med)
rmse_del <- rmse_del |> dplyr::filter(Scale != 'Province')
rmse_del <- rmse_del |> ungroup() |>
  mutate(`Mobility Data Type` = factor(`Mobility Data Type`, levels=c(  "Original", "Rescaled"))) |>
  mutate(trans = factor(trans, levels=c('1.1', '1.5', '3.0'))) |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) |>
  dplyr::filter(Scale != 'District')

# Plot
rmse_plot_del_obs_1 <- ggplot(rmse_del) +
  geom_bar(aes(x = trans, y = rmse, fill = `Mobility Data Type`, linetype = `Mobility Data Type`), 
           color = 'black', stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) +
  labs(x = expression(~ R[0]), y = "RMSE (days)", fill = "Scale") +
  theme_minimal() + scale_y_continuous(limits = c(0, 42), breaks = c(0, 10, 20, 30, 40)) +
  scale_linetype_manual(values = c('Original' = 'solid', 'Rescaled' = 'dashed')) +
  scale_fill_manual(values = c('Rescaled'="grey", 'Original'="#41AE76")) +
  guides(linetype = guide_legend(override.aes = list(fill = "white")))  + ggtitle('Difference by Transmissibility') +
  theme(plot.title = element_text(size=30, hjust = 0.5),
        axis.title = element_text(size=28),
        axis.text = element_text(size=26),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 28)) 
rmse_plot_del_obs_1 

###################
# CREATE BOX PLOT #
###################

# Merge on order set
int_mad_obs_all_1 <- left_join(int_del_obs_all_1, adm_1_order_del, by = c('adm_1' = 'adm_1'))

# Relable variables
int_mad_obs_all_1$Scale <- ifelse(int_mad_obs_all_1$Scale == 'Division' & 
                                    int_mad_obs_all_1$`Mobility Data Type` == 'Rescaled', 
                                  'Rescaled Division', int_mad_obs_all_1$Scale)

# Reorder variables
int_mad_obs_all_1 <- int_mad_obs_all_1 |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", 'Rescaled Division', "Province")),
         `Mobility Data Type` = factor(`Mobility Data Type`, levels=c("Rescaled",  "Original"))) 

# Plot
line_plot_mad_obs_1 <- ggplot(int_mad_obs_all_1, aes(x = time, y = fct_reorder(adm_1, Order), fill = Scale, linetype = `Mobility Data Type`)) +
  #geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, 
  #scale="width", width = 0.6, position = position_dodge(width = 0.9)) +
  geom_boxplot(width=0.5, color = 'black', outlier.shape = NA, coef = 0, alpha = 0.8) +
  theme_minimal() + coord_cartesian(xlim = c(0, 110)) +
  scale_fill_manual(values = c('Rescaled Division'="grey", 'Division'="#41AE76",'Province'= "#4292C6")) +
  scale_linetype_manual(values = c('Original' = 'solid', 'Rescaled' = 'dashed')) +
  ylab('Province') +
  xlab('Time (days)') +
  ggtitle('Province Infection Time') +
  theme(plot.title = element_text(size=30, hjust = 0.5),
        axis.title = element_text(size=28),
        axis.text = element_text(size=26),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 28)) 
line_plot_mad_obs_1

# Reoder variables again
int_mad_obs_all_1 <- int_mad_obs_all_1 |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", 'Rescaled Division', "Province"))) 

# Create legend
legend <- ggplot(int_mad_obs_all_1, aes(x = time, y = fct_reorder(adm_1, Order), fill = Scale)) +
  #geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, 
  #scale="width", width = 0.6, position = position_dodge(width = 0.9)) +
  geom_bar(stat = "identity", position = "dodge", width=0.5, color = 'black', linewidth = 1, alpha = 0.8) +
  theme_minimal() + coord_cartesian(xlim = c(0, 110)) +
  scale_fill_manual(values = c('Rescaled Division'="grey", 'Division'="#41AE76",'Province'= "#4292C6")) +
  ylab('Province') +
  xlab('Time (days)') +
  ggtitle('Province Infection Time') +
  theme(plot.title = element_text(size=30, hjust = 0.5),
        axis.title = element_text(size=28),
        axis.text = element_text(size=26),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 40),
        legend.title = element_text(size = 40),
        legend.key.size = unit(1.2, "cm"),
        legend.position = 'bottom') 

# Grab legend
legend_get <- get_legend(legend)

# Remove extra data to free up space
remove(adm_3_obs_del, adm_3_obs_del_1.1, adm_3_obs_del_3.0, adm_3_at_1_sen_del_1.5,
       adm_3_at_1_sen_del_1.1, adm_3_at_1_sen_del_3.0)

####################################
# CREATE 25 RANDOM LOCATIONS PLOTS #
####################################

#############
# ADM 3 OBS #
#############

## LOAD 25 RANDOM LOCATION RESULTS ##
adm_3_random <- readRDS('./out/adm_3_random.rds')

# Create empty list
inv_list <- NULL

# Loop through introduction scenarios
for (i in 1:25) {
  print(i)
  inv_dat <- adm_3_random[[i]]
  inv_dat <- inv_dat |>
    # Restrict to simulations that took off
    group_by(run_num) |>
    dplyr::filter(sum(incid_I) > 100) |>
    ungroup() |>
    # Sum to relevant spatial scale
    group_by(run_num, time, adm_1) |> 
    mutate(sum_incid_I = sum(incid_I)) |>
    distinct(run_num, intro_num, time, adm_1, sum_incid_I) |> 
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
    mutate(intro_loc = intro_num,
           Scale = 'Division',
           `Mobility Data Type` = 'Observed',
           Count = row_number()) |>
    dplyr::select(run_num, time, intro_loc, adm_1, time, Count, 
                  Scale, `Mobility Data Type`)
  
  inv_list[[i]] <- inv_dat 
  remove(inv_dat)
}

# Save
saveRDS(inv_list, file = './out/adm_3_random_inv.rds')
remove(adm_3_random, inv_list, inv_dat)

# Calculate median arrival time (ADM 3 OBS)
adm_3_random_inv <- readRDS('out/adm_3_random_inv.rds')
adm_3_random_inv_all <- do.call(rbind, adm_3_random_inv)

# Calculate median by introduction location and province
all_3_1.5 <- adm_3_random_inv_all |>
  group_by(adm_1, intro_loc, Scale, `Mobility Data Type`) |>
  mutate(median = median(time),
         Scale = 'Division',
         `Mobility Data Type` = 'Observed') |>
  distinct(adm_1, intro_loc, Scale, `Mobility Data Type`, median)

# Merge on Adm 3 names
adm_3_names <- as.data.frame(adm_3_name_vec)
adm_3_names <- adm_3_names |> mutate(intro_loc = row_number())

all_3_1.5 <- left_join(all_3_1.5, adm_3_names, by = 'intro_loc')

all_3_1.5 <- left_join(all_3_1.5, adm_3_x_walk, by = c('adm_3_name_vec' = 'adm_3'))

#################
# ADM 3 RESCALE #
#################

## LOAD 25 RANDOM LOCATION RESULTS ##
adm_3_random <- readRDS('./out/adm_3_random_rescale.rds')

# Create empty list
inv_list <- NULL

# Loop through introduction scenarios
for (i in 1:25) {
  print(i)
  inv_dat <- adm_3_random[[i]]
  inv_dat <- inv_dat |>
    # Restrict to simulations that took off
    group_by(run_num) |>
    dplyr::filter(sum(incid_I) > 100) |>
    ungroup() |>
    # Sum to relevant spatial scale
    group_by(run_num, time, adm_1) |> 
    mutate(sum_incid_I = sum(incid_I)) |>
    distinct(run_num, intro_num, time, adm_1, sum_incid_I) |> 
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
    mutate(intro_loc = intro_num,
           Scale = 'Division',
           `Mobility Data Type` = 'Observed',
           Count = row_number()) |>
    dplyr::select(run_num, time, intro_loc, adm_1, time, Count, 
                  Scale, `Mobility Data Type`)
  
  inv_list[[i]] <- inv_dat 
  remove(inv_dat)
}

# Save
saveRDS(inv_list, file = './out/adm_3_random_rescale_inv.rds')
remove(adm_3_random, inv_list, inv_dat)

# Calculate median arrival time (ADM 3 RESCALE)
adm_3_random_rescale_inv <- readRDS('out/adm_3_random_rescale_inv.rds')
adm_3_random_rescale_inv_all <- do.call(rbind, adm_3_random_rescale_inv)

# Merge on introduction locations
intro_nums <- readRDS(file = './out/intro_nums_adm_3.rds')
intro_nums <- as.data.frame(intro_nums)
intro_nums <- intro_nums |> mutate(intro_loc = row_number())

adm_3_random_rescale_inv_all <- left_join(adm_3_random_rescale_inv_all, intro_nums, 
                                          by = c('intro_loc' = 'intro_loc'))

# Calculate median by introduction location and province
all_rescale_3_1.5 <- adm_3_random_rescale_inv_all |>
  group_by(adm_1, intro_nums, Scale, `Mobility Data Type`) |>
  mutate(median = median(time),
         Scale = 'Division',
         `Mobility Data Type` = 'Rescaled') |>
  distinct(adm_1, intro_nums, Scale, `Mobility Data Type`, median) |>
  dplyr::rename('intro_loc' = 'intro_nums')

# Merge on Adm 3 names
all_rescale_3_1.5 <- left_join(all_rescale_3_1.5, adm_3_names, by = 'intro_loc')

all_rescale_3_1.5 <- left_join(all_rescale_3_1.5, adm_3_x_walk, by = c('adm_3_name_vec' = 'adm_3'))

#############
# ADM 3 NEW #
#############

## LOAD 25 RANDOM LOCATION RESULTS ##
adm_3_random <- readRDS('./out/adm_3_random.rds')

# Create empty list
inv_list <- NULL

# Loop through introduction scenarios
for (i in 1:25) {
  print(i)
  inv_dat <- adm_3_random[[i]]
  inv_dat <- inv_dat |> dplyr::select(-c(adm_2, adm_1))
  inv_dat <- left_join(inv_dat, adm_3_x_walk_new, by = 'adm_3')
  inv_dat <- inv_dat |>
    # Restrict to simulations that took off
    group_by(run_num) |>
    dplyr::filter(sum(incid_I) > 100) |>
    ungroup() |>
    # Sum to relevant spatial scale
    group_by(run_num, time, adm_1) |> 
    mutate(sum_incid_I = sum(incid_I)) |>
    distinct(run_num, intro_num, time, adm_1, sum_incid_I) |> 
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
    mutate(intro_loc = intro_num,
           Scale = 'Division',
           `Mobility Data Type` = 'Observed',
           Count = row_number()) |>
    dplyr::select(run_num, time, intro_loc, adm_1, time, Count, 
                  Scale, `Mobility Data Type`)
  
  inv_list[[i]] <- inv_dat 
  remove(inv_dat)
}

# Save
saveRDS(inv_list, file = './out/adm_3_random_new_inv.rds')
remove(adm_3_random, inv_list, inv_dat)

#############
# ADM 3 NEW #
#############

## LOAD 25 RANDOM LOCATION RESULTS ##
adm_3_random <- readRDS('./out/adm_3_random.rds')

# Create empty list
inv_list <- NULL

# Loop through introduction scenarios
for (i in 1:25) {
  print(i)
  inv_dat <- adm_3_random[[i]]
  inv_dat <- inv_dat |> dplyr::select(-c(adm_2, adm_1))
  inv_dat <- left_join(inv_dat, adm_3_x_walk_new, by = 'adm_3')
  inv_dat <- inv_dat |>
    # Restrict to simulations that took off
    group_by(run_num) |>
    dplyr::filter(sum(incid_I) > 100) |>
    ungroup() |>
    # Sum to relevant spatial scale
    group_by(run_num, time, adm_1) |> 
    mutate(sum_incid_I = sum(incid_I)) |>
    distinct(run_num, intro_num, time, adm_1, sum_incid_I) |> 
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
    mutate(intro_loc = intro_num,
           Scale = 'Division',
           `Mobility Data Type` = 'Observed',
           Count = row_number()) |>
    dplyr::select(run_num, time, intro_loc, adm_1, time, Count, 
                  Scale, `Mobility Data Type`)
  
  inv_list[[i]] <- inv_dat 
  remove(inv_dat)
}

# Save
saveRDS(inv_list, file = './out/adm_3_random_new_inv.rds')
remove(adm_3_random, inv_list, inv_dat)

# Median arrival time (ADM 3 NEW)
adm_3_random_new_inv <- readRDS('out/adm_3_random_new_inv.rds')
adm_3_random_new_inv_all <- do.call(rbind, adm_3_random_new_inv)

# Calculate median by introduction location and province
all_new_3_1.5 <- adm_3_random_new_inv_all |>
  group_by(adm_1, intro_loc, Scale, `Mobility Data Type`) |>
  mutate(median = median(time),
         Scale = 'Division',
         `Mobility Data Type` = 'Observed') |>
  distinct(adm_1, intro_loc, Scale, `Mobility Data Type`, median)

# Merge on Adm 3 names
adm_3_names <- as.data.frame(adm_3_name_vec)
adm_3_names <- adm_3_names |> mutate(intro_loc = row_number())

all_new_3_1.5 <- left_join(all_new_3_1.5, adm_3_names, by = 'intro_loc')

all_new_3_1.5 <- left_join(all_new_3_1.5, adm_3_x_walk_new, by = c('adm_3_name_vec' = 'adm_3'))

#####################
# ADM 3 NEW RESCALE #
#####################

## LOAD 25 RANDOM LOCATION RESULTS ##
adm_3_random <- readRDS('./out/adm_3_random_rescale_new.rds')

# Create empty list
inv_list <- NULL

# Loop through introduction scenarios
for (i in 1:25) {
  print(i)
  inv_dat <- adm_3_random[[i]]
  inv_dat <- inv_dat |> dplyr::select(-c(adm_2, adm_1))
  inv_dat <- left_join(inv_dat, adm_3_x_walk_new, by = 'adm_3')
  inv_dat <- inv_dat |>
    # Restrict to simulations that took off
    group_by(run_num) |>
    dplyr::filter(sum(incid_I) > 100) |>
    ungroup() |>
    # Sum to relevant spatial scale
    group_by(run_num, time, adm_1) |> 
    mutate(sum_incid_I = sum(incid_I)) |>
    distinct(run_num, intro_num, time, adm_1, sum_incid_I) |> 
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
    mutate(intro_loc = intro_num,
           Scale = 'Division',
           `Mobility Data Type` = 'Observed',
           Count = row_number()) |>
    dplyr::select(run_num, time, intro_loc, adm_1, time, Count, 
                  Scale, `Mobility Data Type`)
  
  inv_list[[i]] <- inv_dat 
  remove(inv_dat)
}

# Save
saveRDS(inv_list, file = './out/adm_3_random_rescale_new_inv.rds')
remove(adm_3_random, inv_list, inv_dat)

# Median arrival time (ADM 3 RESCALE NEW)
adm_3_random_rescale_new_inv <- readRDS('out/adm_3_random_rescale_new_inv.rds')
adm_3_random_rescale_new_inv_all <- do.call(rbind, adm_3_random_rescale_new_inv)

# Calculate median by introduction location and province
all_rescale_new_3_1.5 <- adm_3_random_rescale_new_inv_all |>
  group_by(adm_1, intro_loc, Scale, `Mobility Data Type`) |>
  mutate(median = median(time),
         Scale = 'Division',
         `Mobility Data Type` = 'Rescaled') |>
  distinct(adm_1, intro_loc, Scale, `Mobility Data Type`, median)

# Merge on Adm 3 names
all_rescale_new_3_1.5 <- left_join(all_rescale_new_3_1.5, adm_3_names, by = 'intro_loc')

all_rescale_new_3_1.5 <- left_join(all_rescale_new_3_1.5, adm_3_x_walk_new, by = c('adm_3_name_vec' = 'adm_3'))

# Median arrival time (ADM 1)
adm_1_random_inv <- readRDS('out/adm_1_random_inv.rds')
adm_1_random_inv_all <- do.call(rbind, adm_1_random_inv)

all_1_1.5 <- adm_1_random_inv_all |>
  group_by(adm_1, intro_loc, Scale, `Mobility Data Type`) |>
  mutate(median = median(time),
         Scale = 'Province',
         `Mobility Data Type` = 'Observed') |>
  distinct(adm_1, intro_loc, Scale, `Mobility Data Type`, median)

adm_1_names <- as.data.frame(adm_1_name_vec)
adm_1_names <- adm_1_names |> mutate(intro_loc = row_number())

all_1_1.5 <- left_join(all_1_1.5, adm_1_names, by = 'intro_loc')

# Median arrival time (ADM 1 NEW)
adm_1_random_new_inv <- readRDS('out/adm_1_random_new_inv.rds')
adm_1_random_new_inv_all <- do.call(rbind, adm_1_random_new_inv)


all_new_1_1.5 <- adm_1_random_new_inv_all |>
  group_by(adm_1, intro_loc, Scale, `Mobility Data Type`) |>
  mutate(median = median(time),
         Scale = 'Province',
         `Mobility Data Type` = 'Observed') |>
  distinct(adm_1, intro_loc, Scale, `Mobility Data Type`, median)

#############################################################
# MERGE RESULTS TOGETHER FOR ORIGINAL AND NEW SPATIAL UNITS #
#############################################################

# MERGE ADM 3 to ADM 1
all_3_1.5 <- left_join(all_3_1.5, all_1_1.5, by = c('adm_1.y' = 'adm_1_name_vec',
                                                    'adm_1.x' = 'adm_1'))
all_rescale_3_1.5 <- left_join(all_rescale_3_1.5, all_1_1.5, by = c('adm_1.y' = 'adm_1_name_vec',
                                                                    'adm_1.x' = 'adm_1'))

# Calculate RMSE between adm 1 and 3
rmse_adm_3_obs <- all_3_1.5 |> group_by(Scale.x, intro_loc.x, `Mobility Data Type.x`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale.x, intro_loc.x, `Mobility Data Type.x`, rmse) |>
  mutate(trans = '1.5')

# Calculate RMSE between adm 1 and 3 (rescaled)
rmse_adm_3_rescale <- all_rescale_3_1.5 |> group_by(Scale.x, intro_loc.x, `Mobility Data Type.x`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale.x, intro_loc.x, `Mobility Data Type.x`, rmse) |>
  mutate(trans = '1.5')

# Append
rmse <- rbind(rmse_adm_3_obs, rmse_adm_3_rescale)
rmse <- left_join(rmse, adm_3_names, by = c('intro_loc.x' = 'intro_loc'))
rmse <- left_join(rmse, adm_3_adm_1_phone_leave, 
                  by = c('adm_3_name_vec' = 'origin'))

# MERGE ADM 3 to ADM 1 (NEW)
all_new_1_1.5$adm_1 <- as.numeric(all_new_1_1.5$adm_1)
all_new_3_1.5 <- left_join(all_new_3_1.5, all_new_1_1.5, by = c('adm_1.y' = 'intro_loc',
                                                                'adm_1.x' = 'adm_1'))
all_rescale_new_3_1.5 <- left_join(all_rescale_new_3_1.5, all_new_1_1.5, by = c('adm_1.y' = 'intro_loc',
                                                                    'adm_1.x' = 'adm_1'))

# Calculate RMSE between adm 1 and 3
rmse_adm_3_obs_new <- all_new_3_1.5 |> group_by(Scale.x, intro_loc, `Mobility Data Type.x`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale.x, intro_loc, `Mobility Data Type.x`, rmse) |>
  mutate(trans = '1.5')

# Calculate RMSE between adm 1 and 3 (rescaled)
rmse_adm_3_rescale_new <- all_rescale_new_3_1.5 |> group_by(Scale.x, intro_loc, `Mobility Data Type.x`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale.x, intro_loc, `Mobility Data Type.x`, rmse) |>
  mutate(trans = '1.5')

# Append
rmse_new <- rbind(rmse_adm_3_obs_new, rmse_adm_3_rescale_new)
rmse_new <- left_join(rmse_new, adm_3_names, by = c('intro_loc' = 'intro_loc'))

## PLOT ##

# Create an order set (original)
rmse_obs <- rmse |> dplyr::filter(`Mobility Data Type.x` == 'Observed')
rmse_order <- rmse_obs |> ungroup() |>
  arrange(rmse) |>
  mutate(order = row_number()) |>
  distinct(intro_loc.x, order)
rmse <- left_join(rmse, rmse_order, by = c('intro_loc.x' = 'intro_loc.x'))

# Create an order set (new)
rmse_obs_new <- rmse_new |> dplyr::filter(`Mobility Data Type.x` == 'Observed')
rmse_order_new <- rmse_obs_new |> ungroup() |>
  arrange(rmse) |>
  mutate(order = row_number()) |>
  distinct(intro_loc, order)
rmse_new <- left_join(rmse_new, rmse_order, by = c('intro_loc' = 'intro_loc.x'))

# Original RMSE
rmse_random <- ggplot(rmse) + 
  geom_line(aes(x = reorder(as.character(intro_loc.x), order), y = rmse, group = intro_loc.x), linetype = 'dashed', linewidth = 1.1) +
  geom_point(aes(x = reorder(as.character(intro_loc.x), order), y = rmse, color = `Mobility Data Type.x`), size = 7, alpha = 0.8) +
  scale_color_manual('Data Type', values = c("Observed" = "#41AE76", "Rescaled" = "grey")) +
  theme_minimal() +
  ylab('RMSE (days)') +
  xlab('Introduction Location') +
  ggtitle('Difference by Location') +
  theme(plot.title = element_text(size=30, hjust = 0.5),
        axis.title = element_text(size=28),
        axis.text = element_text(size=26),
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 28),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none') +
  scale_y_continuous(limits = c(0, 25)) +
  scale_x_discrete(labels = c('262', '2')) +
  scale_x_discrete(breaks = levels(reorder(as.character(rmse$intro_loc.x), rmse$order))[seq(1, n_distinct(rmse$intro_loc.x), by = 5)])
rmse_random

# New RMSE
rmse_random_new <- ggplot(rmse_new) + 
  geom_line(aes(x = reorder(as.character(intro_loc), order), y = rmse, group = intro_loc), linetype = 'dashed', linewidth = 1.1) +
  geom_point(aes(x = reorder(as.character(intro_loc), order), y = rmse, color = `Mobility Data Type.x`), size = 7, alpha = 0.8) +
  scale_color_manual('Data Type', values = c("Observed" = "#41AE76", "Rescaled" = "grey")) +
  theme_minimal() +
  ylab('RMSE (days)') +
  xlab('Introduction Location') +
  ggtitle('Difference by Reassigned Locations') +
  theme(plot.title = element_text(size=30, hjust = 0.5),
        axis.title = element_text(size=28),
        axis.text = element_text(size=26),
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 28),
        panel.grid.minor = element_blank(),
        #panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none') +
  scale_y_continuous(limits = c(0, 25)) +
  scale_x_discrete(breaks = levels(reorder(as.character(rmse_new$intro_loc), rmse_new$order))[seq(1, n_distinct(rmse_new$intro_loc), by = 5)])
rmse_random_new 

######################################
# PLOT NESTED PROBABILITY OF LEAVING #
######################################

################
# PLOT SCATTER #
################

# Load mobility data for each data source
adm_3_phone_mobility_long <- readRDS('./out/adm_3_phone_mobility_long.rds')
adm_1_phone_mobility_long <- readRDS('./out/adm_1_phone_mobility_long_new.rds')
adm_3_x_walk_new <- readRDS('./out/adm_3_x_walk_new.rds')

# Load population data
adm_3_population_dat <- readRDS('./out/adm_3_population_dat.rds')
#adm_1_population_dat_new <- readRDS('./out/adm_1_population_dat_new.rds')

# Merge on origin
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_3_x_walk_new[, c(1, 3)],
                                       by = c('origin' = 'adm_3'))

# Merge on destination
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_3_x_walk_new[, c(1, 3)], 
                                       by = c('destination' = 'adm_3'))

# Calculate out of province travel at the admin 3 unit
adm_3_adm_1_phone <- adm_3_phone_mobility_long |>
  group_by(origin, adm_1.x, adm_1.y) |>
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_1.x, adm_1.y, adm_3_sum, .keep_all = FALSE) |>
  dplyr::rename('adm_1_origin' = 'adm_1.x',
                'adm_1_destination' = 'adm_1.y')
adm_3_adm_1_phone$adm_1_origin <- as.character(adm_3_adm_1_phone$adm_1_origin)
adm_3_adm_1_phone$adm_1_destination <- as.character(adm_3_adm_1_phone$adm_1_destination)

# Merge on origin from admin 1 and travel probabillity
adm_3_adm_1_phone <- left_join(adm_3_adm_1_phone, 
                               adm_1_phone_mobility_long, 
                               by = c('adm_1_origin' = 'origin',
                                      'adm_1_destination' = 'destination'))

# Calculate 1 - stays to get the leave probability and difference between units
adm_3_adm_1_phone_leave <- adm_3_adm_1_phone |>
  dplyr::filter(adm_1_origin == adm_1_destination) |>
  mutate(adm_3_leave = 1 - adm_3_sum,
         adm_1_leave = 1 - value,
         difference = adm_3_leave - adm_1_leave)

# Merge on admin 3 population
adm_3_adm_1_phone_leave <- left_join(adm_3_adm_1_phone_leave, adm_3_population_dat[, c(1, 4)], 
                                     by = c('origin' = 'adm_3_mobility'))

# Create scatter plot by population
plot_3_1_scatter <- ggplot(adm_3_adm_1_phone_leave) +
  geom_point(aes(x = log(population_2020_adm_3), y = difference*100, 
                 fill = difference*100), color = '#565656', alpha = 0.85, size = 8, shape = 21) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = log(population_2020_adm_3), y = difference*100), color = '#565656', 
              method = "loess", se = TRUE, linewidth = 4, alpha = 0.25) + 
  ylim(-0.30*100, 0.60*100) +
  ggtitle('Difference in Reassigned Province\nLeave Probability (Division - Province)\n') +
  xlab('Log Population (Division)') + 
  ylab('Leave Probability Difference (%)') +
  theme_minimal() +
  theme(
    axis.title = element_text(size=28),
    axis.text = element_text(size=26),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 28),
    legend.key.height = unit(3.3, 'cm'),
    legend.key.width = unit(1.4, 'cm'),
    plot.title = element_text(size=30, hjust = 0.5)) +
  scale_fill_gradient2('Difference (%)\n',
                       low = '#41AE76', mid = "white", high = '#4292C6',
                       midpoint = 0, limits=c(-0.30*100, 0.60*100)) +
  scale_x_continuous(limits = c(6, 13), breaks = c(6, 8, 10, 12))
plot_3_1_scatter 

############
# PLOT MAP #
############

choropleth_1_new <- readRDS('./out/adm_1_new_shape.rds')
choropleth_3_mobility <- left_join(choropleth_3_mobility, adm_3_adm_1_phone_leave[ c('origin', 'difference')], by = 
                                     c('adm_3_mobility' = 'origin'))
plot_3_1_map <- ggplot() +
  geom_sf(data = choropleth_3_mobility, aes(fill = difference), color= 'black', linewidth = 0.25, alpha = 1) +
  geom_sf(data = choropleth_1_new, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(legend.position = 'none',
                                     plot.title = element_text(size = 30, hjust = 0.5),
                                     legend.text = element_text(size = 24),
                                     legend.title = element_text(size = 24),
                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#41AE76', mid = "white", high = '#4292C6',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.30, 0.60)) 
plot_3_1_map 

##########################
# 4. CREATE FINAL FIGURE #
##########################

col_1 <- cowplot::plot_grid(rmse_plot_del_obs_1, line_plot_mad_obs_1, rmse_random,
                            nrow = 1, labels = c('(a)', '(b)', '(c)'),
                            label_size = 34, hjust = 0)

col_2 <- cowplot::plot_grid(plot_3_1_scatter,
                            ggplot() + theme_void(), 
                            plot_3_1_map,
                            nrow = 1, labels = c('', '', ''),
                            label_size = 34, hjust = 0,
                            rel_widths = c(1.8, 0.2, 1))

col_3 <- cowplot::plot_grid(col_2, rmse_random_new,
                            nrow = 1, labels = c('(d)', '(e)'),
                            label_size = 34, hjust = 0,
                            rel_widths = c( 1.33, 0.67))


figure_5 <- cowplot::plot_grid(col_1,
                               ggplot() + theme_void(),
                               col_3,
                               legend_get,
                               nrow = 4, rel_heights = c(1, 0.05, 1, 0.1),
                               labels = c('', ''),
                               label_size = 26, hjust = 0)                            
# Save plot
ggsave('./figs/figure_5_sensitivity.jpg', plot = figure_5, height = 19, width = 25)

################################################################################
################################################################################

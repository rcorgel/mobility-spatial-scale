################################################################################
# File Name: 05i_figure_4_example                                              #
#                                                                              #
# Purpose:   Create figure 4 for the manuscript.                               #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Simulate example epidemics                                     #
#            3. Create figures                                                 #
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
library(parallel)
library(ggpubr)

# Set the seed
set.seed(123456)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load model 
source('./mobility-spatial-scale/04_metapop_model.R')

load('./tmp/rescale_phone_mobility_dat.RData')

#################################
# 2. SIMULATE EXAMPLE EPIDEMICS #
#################################

# First, load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Load results
adm_3_obs_col <- readRDS('./out/adm_3_obs_col_1.5.rds')
adm_2_obs_col <- readRDS('./out/adm_2_obs_col_1.5.rds')
adm_1_obs_col <- readRDS('./out/adm_1_obs_col_1.5.rds')
adm_3_obs_del <- readRDS('./out/adm_3_obs_del_1.5.rds')
adm_2_obs_del <- readRDS('./out/adm_2_obs_del_1.5.rds')
adm_1_obs_del <- readRDS('./out/adm_1_obs_del_1.5.rds')

adm_3_obs_col_1.1 <- readRDS('./out/adm_3_obs_col_1.1.rds')
adm_2_obs_col_1.1 <- readRDS('./out/adm_2_obs_col_1.1.rds')
adm_1_obs_col_1.1 <- readRDS('./out/adm_1_obs_col_1.1.rds')
adm_3_obs_del_1.1 <- readRDS('./out/adm_3_obs_del_1.1.rds')
adm_2_obs_del_1.1 <- readRDS('./out/adm_2_obs_del_1.1.rds')
adm_1_obs_del_1.1 <- readRDS('./out/adm_1_obs_del_1.1.rds')

adm_3_obs_col_3.0 <- readRDS('./out/adm_3_obs_col_3.0.rds')
adm_2_obs_col_3.0 <- readRDS('./out/adm_2_obs_col_3.0.rds')
adm_1_obs_col_3.0 <- readRDS('./out/adm_1_obs_col_3.0.rds')
adm_3_obs_del_3.0 <- readRDS('./out/adm_3_obs_del_3.0.rds')
adm_2_obs_del_3.0 <- readRDS('./out/adm_2_obs_del_3.0.rds')
adm_1_obs_del_3.0 <- readRDS('./out/adm_1_obs_del_3.0.rds')

adm_3_at_2_sen_col_1.5 <- readRDS('./out/adm_3_at_2_sen_col_1.5.rds')
adm_3_at_1_sen_col_1.5 <- readRDS('./out/adm_3_at_1_sen_col_1.5.rds')
adm_2_sen_col_1.5 <- readRDS('./out/adm_2_sen_col_1.5.rds')

adm_3_at_2_sen_del_1.5 <- readRDS('./out/adm_3_at_2_sen_del_1.5.rds')
adm_3_at_1_sen_del_1.5 <- readRDS('./out/adm_3_at_1_sen_del_1.5.rds')
adm_2_sen_del_1.5 <- readRDS('./out/adm_2_sen_del_1.5.rds')

adm_3_at_2_sen_col_1.1 <- readRDS('./out/adm_3_at_2_sen_col_1.1.rds')
adm_3_at_1_sen_col_1.1 <- readRDS('./out/adm_3_at_1_sen_col_1.1.rds')
adm_2_sen_col_1.1 <- readRDS('./out/adm_2_sen_col_1.1.rds')

adm_3_at_2_sen_del_1.1 <- readRDS('./out/adm_3_at_2_sen_del_1.1.rds')
adm_3_at_1_sen_del_1.1 <- readRDS('./out/adm_3_at_1_sen_del_1.1.rds')
adm_2_sen_del_1.1 <- readRDS('./out/adm_2_sen_del_1.1.rds')

adm_3_at_2_sen_col_3.0 <- readRDS('./out/adm_3_at_2_sen_col_3.0.rds')
adm_3_at_1_sen_col_3.0 <- readRDS('./out/adm_3_at_1_sen_col_3.0.rds')
adm_2_sen_col_3.0 <- readRDS('./out/adm_2_sen_col_3.0.rds')

adm_3_at_2_sen_del_3.0 <- readRDS('./out/adm_3_at_2_sen_del_3.0.rds')
adm_3_at_1_sen_del_3.0 <- readRDS('./out/adm_3_at_1_sen_del_3.0.rds')
adm_2_sen_del_3.0 <- readRDS('./out/adm_2_sen_del_3.0.rds')

################################
# CREATE AGGREGATION FUNCTIONS #
################################

# Aggregate simulation to the adm 2 scale
agg_to_adm_2 <- function(results, intro_loc, scale, trans) {
  results |> group_by(time, adm_2, run_num) |>
    # Calculate incidence at Admin 2
    mutate(incid_I_adm_2 = sum(incid_I)) |>
    distinct(time, run_num, adm_2, incid_I_adm_2) |> 
    ungroup() |>
    # Calculate burn out and drop runs that did not take off
    group_by(run_num) |>
    mutate(run_total = sum(incid_I_adm_2)) |>
    ungroup() |>
    mutate(burn_out_perc = sum(run_total > 100) / n()) |>
    filter(run_total > 100) |>
    # Calculate total epidemic size
    group_by(run_num) |>
    mutate(magnitude = sum(incid_I_adm_2)) |>
    ungroup() |>
    group_by(run_num, adm_2) |>
    # Calculate cumulative cases by Admin 2
    # Indicate when cumulative cases > 1 for each unit
    mutate(cum_I = cumsum(incid_I_adm_2),
           intro = ifelse(cum_I > 1, 1, 0)) |>
    # Indicate the first instance when cumulative > 1
    mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
    # Filter to first instance for all Admin 2
    filter(intro_first == TRUE) |>
    ungroup() |>
    group_by(run_num) |>
    arrange(time) |>
    slice(1:25) |>
    mutate(time = max(time),
           intro_loc = intro_loc,
           Scale = scale,
           Trans = trans) |>
    distinct(run_num, time, magnitude, intro_loc, Scale, Trans)
}

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

# Administrative level spatial introduction to the adm 2 scale
invasion_run_adm_2 <- function(results, intro_loc, scale, type) {
  results |> 
    # Restrict to simulations that took off
    group_by(run_num) |>
    dplyr::filter(sum(incid_I) > 100) |>
    ungroup() |>
    # Sum to relevant spatial scale
    group_by(run_num, time, adm_2) |> 
    mutate(sum_incid_I = sum(incid_I)) |>
    distinct(run_num, time, adm_2, sum_incid_I) |> 
    ungroup() |>
    group_by(run_num, adm_2) |> 
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
    dplyr::select(run_num, time, adm_2, time, Count, Scale, `Mobility Data Type`)
}

############################
# RUN OBSERVED SIMULATIONS #
############################

#############
# R_0 = 1.5 #
#############

# Adm 3 Colombo introduction, R_0 = 1.5
adm_3_at_2_col <- agg_to_adm_2(results = adm_3_obs_col, intro_loc = 'Colombo', scale = '3 at 2', trans = '1.5') 
adm_3_at_2_col_inv <- invasion_run_adm_2(results = adm_3_obs_col, intro_loc = 'Colombo', scale = 'Division', type = 'Original') 
adm_3_at_1_col <- agg_to_adm_1(results = adm_3_obs_col, intro_loc = 'Colombo', scale = '3 at 1', trans = '1.5') 
adm_3_at_1_col_inv <- invasion_run_adm_1(results = adm_3_obs_col, intro_loc = 'Colombo', scale = 'Division', type = 'Original') 

# Adm 3 Sevanagala introduction, R_0 = 1.5
adm_3_at_2_del <- agg_to_adm_2(results = adm_3_obs_del, intro_loc = 'Sevanagala', scale = '3 at 2', trans = '1.5') 
adm_3_at_2_del_inv <- invasion_run_adm_2(results = adm_3_obs_del, intro_loc = 'Sevanagala', scale = 'Division', type = 'Original') 
adm_3_at_1_del <- agg_to_adm_1(results = adm_3_obs_del, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '1.5') 
adm_3_at_1_del_inv <- invasion_run_adm_1(results = adm_3_obs_del, intro_loc = 'Sevanagala', scale = 'Division', type = 'Original') 

# Adm 2 Colombo introduction, R_0 = 1.5
adm_2_at_2_col <- agg_to_adm_2(results = adm_2_obs_col, intro_loc = 'Colombo', scale = '2', trans = '1.5') 
adm_2_at_2_col_inv <- invasion_run_adm_2(results = adm_2_obs_col, intro_loc = 'Colombo', scale = 'District', type = 'Original') 
adm_2_at_1_col <- agg_to_adm_1(results = adm_2_obs_col, intro_loc = 'Colombo', scale = '2 at 1', trans = '1.5') 
adm_2_at_1_col_inv <- invasion_run_adm_1(results = adm_2_obs_col, intro_loc = 'Colombo', scale = 'District', type = 'Original') 

# Adm 2 Sevanagala introduction, R_0 = 1.5
adm_2_at_2_del <- agg_to_adm_2(results = adm_2_obs_del, intro_loc = 'Sevanagala', scale = '2', trans = '1.5') 
adm_2_at_2_del_inv <- invasion_run_adm_2(results = adm_2_obs_del, intro_loc = 'Sevanagala', scale = 'District', type = 'Original') 
adm_2_at_1_del <- agg_to_adm_1(results = adm_2_obs_del, intro_loc = 'Sevanagala', scale = '2 at 1', trans = '1.5') 
adm_2_at_1_del_inv <- invasion_run_adm_1(results = adm_2_obs_del, intro_loc = 'Sevanagala', scale = 'District', type = 'Original') 

# Adm 1 Colombo introduction, R_0 = 1.5
adm_1_at_1_col <- agg_to_adm_1(results = adm_1_obs_col, intro_loc = 'Colombo', scale = '1', trans = '1.5') 
adm_1_at_1_col_inv <- invasion_run_adm_1(results = adm_1_obs_col, intro_loc = 'Colombo', scale = 'Province', type = 'Original') 

# Adm 1 Sevanagala introduction, R_0 = 1.5
adm_1_at_1_del <- agg_to_adm_1(results = adm_1_obs_del, intro_loc = 'Sevanagala', scale = '1', trans = '1.5') 
adm_1_at_1_del_inv <- invasion_run_adm_1(results = adm_1_obs_del, intro_loc = 'Sevanagala', scale = 'Province', type = 'Original') 

#############
# R_0 = 1.1 #
#############

# Adm 3 Colombo introduction, R_0 = 1.1
adm_3_at_2_col_1.1 <- agg_to_adm_2(results = adm_3_obs_col_1.1, intro_loc = 'Colombo', scale = '3 at 2', trans = '1.1') 
adm_3_at_2_col_inv_1.1 <- invasion_run_adm_2(results = adm_3_obs_col_1.1, intro_loc = 'Colombo', scale = 'Division', type = 'Original') 
adm_3_at_1_col_1.1 <- agg_to_adm_1(results = adm_3_obs_col_1.1, intro_loc = 'Colombo', scale = '3 at 1', trans = '1.1') 
adm_3_at_1_col_inv_1.1 <- invasion_run_adm_1(results = adm_3_obs_col_1.1, intro_loc = 'Colombo', scale = 'Division', type = 'Original') 

# Adm 3 Sevanagala introduction, R_0 = 1.1
adm_3_at_2_del_1.1 <- agg_to_adm_2(results = adm_3_obs_del_1.1, intro_loc = 'Sevanagala', scale = '3 at 2', trans = '1.1') 
adm_3_at_2_del_inv_1.1 <- invasion_run_adm_2(results = adm_3_obs_del_1.1, intro_loc = 'Sevanagala', scale = 'Division', type = 'Original') 
adm_3_at_1_del_1.1 <- agg_to_adm_1(results = adm_3_obs_del_1.1, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '1.1') 
adm_3_at_1_del_inv_1.1 <- invasion_run_adm_1(results = adm_3_obs_del_1.1, intro_loc = 'Sevanagala', scale = 'Division', type = 'Original') 

# Adm 2 Colombo introduction, R_0 = 1.1
adm_2_at_2_col_1.1 <- agg_to_adm_2(results = adm_2_obs_col_1.1, intro_loc = 'Colombo', scale = '2', trans = '1.1') 
adm_2_at_2_col_inv_1.1 <- invasion_run_adm_2(results = adm_2_obs_col_1.1, intro_loc = 'Colombo', scale = 'District', type = 'Original') 
adm_2_at_1_col_1.1 <- agg_to_adm_1(results = adm_2_obs_col_1.1, intro_loc = 'Colombo', scale = '2 at 1', trans = '1.1') 
adm_2_at_1_col_inv_1.1 <- invasion_run_adm_1(results = adm_2_obs_col_1.1, intro_loc = 'Colombo', scale = 'District', type = 'Original') 

# Adm 2 Sevanagala introduction, R_0 = 1.1
adm_2_at_2_del_1.1 <- agg_to_adm_2(results = adm_2_obs_del_1.1, intro_loc = 'Sevanagala', scale = '2', trans = '1.1') 
adm_2_at_2_del_inv_1.1 <- invasion_run_adm_2(results = adm_2_obs_del_1.1, intro_loc = 'Sevanagala', scale = 'District', type = 'Original') 
adm_2_at_1_del_1.1 <- agg_to_adm_1(results = adm_2_obs_del_1.1, intro_loc = 'Sevanagala', scale = '2 at 1', trans = '1.1') 
adm_2_at_1_del_inv_1.1 <- invasion_run_adm_1(results = adm_2_obs_del_1.1, intro_loc = 'Sevanagala', scale = 'District', type = 'Original') 

# Adm 1 Colombo introduction, R_0 = 1.1
adm_1_at_1_col_1.1 <- agg_to_adm_1(results = adm_1_obs_col_1.1, intro_loc = 'Colombo', scale = '1', trans = '1.1') 
adm_1_at_1_col_inv_1.1 <- invasion_run_adm_1(results = adm_1_obs_col_1.1, intro_loc = 'Colombo', scale = 'Province', type = 'Original') 

# Adm 1 Sevanagala introduction, R_0 = 1.1
adm_1_at_1_del_1.1 <- agg_to_adm_1(results = adm_1_obs_del_1.1, intro_loc = 'Sevanagala', scale = '1', trans = '1.1') 
adm_1_at_1_del_inv_1.1 <- invasion_run_adm_1(results = adm_1_obs_del_1.1, intro_loc = 'Sevanagala', scale = 'Province', type = 'Original') 

#############
# R_0 = 3.0 #
#############

# Adm 3 Colombo introduction, R_0 = 3.0
adm_3_at_2_col_3.0 <- agg_to_adm_2(results = adm_3_obs_col_3.0, intro_loc = 'Colombo', scale = '3 at 2', trans = '3.0') 
adm_3_at_2_col_inv_3.0 <- invasion_run_adm_2(results = adm_3_obs_col_3.0, intro_loc = 'Colombo', scale = 'Division', type = 'Original') 
adm_3_at_1_col_3.0 <- agg_to_adm_1(results = adm_3_obs_col_3.0, intro_loc = 'Colombo', scale = '3 at 1', trans = '3.0') 
adm_3_at_1_col_inv_3.0 <- invasion_run_adm_1(results = adm_3_obs_col_3.0, intro_loc = 'Colombo', scale = 'Division', type = 'Original') 

# Adm 3 Sevanagala introduction, R_0 = 3.0
adm_3_at_2_del_3.0 <- agg_to_adm_2(results = adm_3_obs_del_3.0, intro_loc = 'Sevanagala', scale = '3 at 2', trans = '3.0') 
adm_3_at_2_del_inv_3.0 <- invasion_run_adm_2(results = adm_3_obs_del_3.0, intro_loc = 'Sevanagala', scale = 'Division', type = 'Original') 
adm_3_at_1_del_3.0 <- agg_to_adm_1(results = adm_3_obs_del_3.0, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '3.0') 
adm_3_at_1_del_inv_3.0 <- invasion_run_adm_1(results = adm_3_obs_del_3.0, intro_loc = 'Sevanagala', scale = 'Division', type = 'Original') 

# Adm 2 Colombo introduction, R_0 = 3.0
adm_2_at_2_col_3.0 <- agg_to_adm_2(results = adm_2_obs_col_3.0, intro_loc = 'Colombo', scale = '2', trans = '3.0') 
adm_2_at_2_col_inv_3.0 <- invasion_run_adm_2(results = adm_2_obs_col_3.0, intro_loc = 'Colombo', scale = 'District', type = 'Original') 
adm_2_at_1_col_3.0 <- agg_to_adm_1(results = adm_2_obs_col_3.0, intro_loc = 'Colombo', scale = '2 at 1', trans = '3.0') 
adm_2_at_1_col_inv_3.0 <- invasion_run_adm_1(results = adm_2_obs_col_3.0, intro_loc = 'Colombo', scale = 'District', type = 'Original') 

# Adm 2 Sevanagala introduction, R_0 = 3.0
adm_2_at_2_del_3.0 <- agg_to_adm_2(results = adm_2_obs_del_3.0, intro_loc = 'Sevanagala', scale = '2', trans = '3.0') 
adm_2_at_2_del_inv_3.0 <- invasion_run_adm_2(results = adm_2_obs_del_3.0, intro_loc = 'Sevanagala', scale = 'District', type = 'Original') 
adm_2_at_1_del_3.0 <- agg_to_adm_1(results = adm_2_obs_del_3.0, intro_loc = 'Sevanagala', scale = '2 at 1', trans = '3.0') 
adm_2_at_1_del_inv_3.0 <- invasion_run_adm_1(results = adm_2_obs_del_3.0, intro_loc = 'Sevanagala', scale = 'District', type = 'Original') 

# Adm 1 Colombo introduction, R_0 = 3.0
adm_1_at_1_col_3.0 <- agg_to_adm_1(results = adm_1_obs_col_3.0, intro_loc = 'Colombo', scale = '1', trans = '3.0') 
adm_1_at_1_col_inv_3.0 <- invasion_run_adm_1(results = adm_1_obs_col_3.0, intro_loc = 'Colombo', scale = 'Province', type = 'Original') 

# Adm 1 Sevanagala introduction, R_0 = 3.0
adm_1_at_1_del_3.0 <- agg_to_adm_1(results = adm_1_obs_del_3.0, intro_loc = 'Sevanagala', scale = '1', trans = '3.0') 
adm_1_at_1_del_inv_3.0 <- invasion_run_adm_1(results = adm_1_obs_del_3.0, intro_loc = 'Sevanagala', scale = 'Province', type = 'Original') 

############################
# RUN RESCALED SIMULATIONS #
############################

#############
# R_0 = 1.5 #
#############

# Adm 3 at 2 Colombo introduction, R_0 = 1.5
adm_3_at_2_col_sen <- agg_to_adm_2(results = adm_3_at_2_sen_col_1.5, intro_loc = 'Colombo', scale = '3 at 2', trans = '1.5') 
adm_3_at_2_col_sen_inv <- invasion_run_adm_2(results = adm_3_at_2_sen_col_1.5, intro_loc = 'Colombo', scale = 'Division', type = 'Rescaled') 

# Adm 3 at 2 Sevanagala introduction, R_0 = 1.5
adm_3_at_2_del_sen <- agg_to_adm_2(results = adm_3_at_2_sen_del_1.5, intro_loc = 'Sevanagala', scale = '3 at 2', trans = '1.5') 
adm_3_at_2_del_sen_inv <- invasion_run_adm_2(results = adm_3_at_2_sen_del_1.5, intro_loc = 'Sevanagala', scale = 'Division', type = 'Rescaled') 

# Adm 3 at 1 Colombo introduction, R_0 = 1.5
adm_3_at_1_col_sen <- agg_to_adm_1(results = adm_3_at_1_sen_col_1.5, intro_loc = 'Colombo', scale = '3 at 1', trans = '1.5')
adm_3_at_1_col_sen_inv <- invasion_run_adm_1(results = adm_3_at_1_sen_col_1.5, intro_loc = 'Colombo', scale = 'Division', type = 'Rescaled') 

# Adm 3 at 1 Sevanagala introduction, R_0 = 1.5
adm_3_at_1_del_sen <- agg_to_adm_1(results = adm_3_at_1_sen_del_1.5, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '1.5') 
adm_3_at_1_del_sen_inv <- invasion_run_adm_1(results =  adm_3_at_1_sen_del_1.5, intro_loc = 'Sevanagala', scale = 'Division', type = 'Rescaled') 

# Adm 2 at 1 Colombo introduction, R_0 = 1.5
adm_2_at_1_col_sen <- agg_to_adm_1(results = adm_2_sen_col_1.5, intro_loc = 'Colombo', scale = '2 at 1', trans = '1.5') 
adm_2_at_1_col_sen_inv <- invasion_run_adm_1(results =  adm_2_sen_col_1.5, intro_loc = 'Colombo', scale = 'District', type = 'Rescaled') 

# Adm 2 Sevanagala introduction, R_0 = 1.5
adm_2_at_1_del_sen <- agg_to_adm_1(results = adm_2_sen_del_1.5, intro_loc = 'Sevanagala', scale = '2 at 1', trans = '1.5') 
adm_2_at_1_del_sen_inv <- invasion_run_adm_1(results = adm_2_sen_del_1.5, intro_loc = 'Sevanagala', scale = 'District', type = 'Rescaled') 

#############
# R_0 = 1.1 #
#############

# Adm 3 at 2 Colombo introduction, R_0 = 1.1
adm_3_at_2_col_sen_1.1 <- agg_to_adm_2(results = adm_3_at_2_sen_col_1.1, intro_loc = 'Colombo', scale = '3 at 2', trans = '1.1') 
adm_3_at_2_col_sen_inv_1.1 <- invasion_run_adm_2(results = adm_3_at_2_sen_col_1.1, intro_loc = 'Colombo', scale = 'Division', type = 'Rescaled') 

# Adm 3 at 2 Sevanagala introduction, R_0 = 1.1
adm_3_at_2_del_sen_1.1 <- agg_to_adm_2(results = adm_3_at_2_sen_del_1.1, intro_loc = 'Sevanagala', scale = '3 at 2', trans = '1.1') 
adm_3_at_2_del_sen_inv_1.1 <- invasion_run_adm_2(results = adm_3_at_2_sen_del_1.1, intro_loc = 'Sevanagala', scale = 'Division', type = 'Rescaled') 

# Adm 3 at 1 Colombo introduction, R_0 = 1.1
adm_3_at_1_col_sen_1.1 <- agg_to_adm_1(results = adm_3_at_1_sen_col_1.1, intro_loc = 'Colombo', scale = '3 at 1', trans = '1.1') 
adm_3_at_1_col_sen_inv_1.1 <- invasion_run_adm_1(results = adm_3_at_1_sen_col_1.1, intro_loc = 'Colombo', scale = 'Division', type = 'Rescaled') 

# Adm 3 at 1 Sevanagala introduction, R_0 = 1.1
adm_3_at_1_del_sen_1.1 <- agg_to_adm_1(results = adm_3_at_1_sen_del_1.1, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '1.1') 
adm_3_at_1_del_sen_inv_1.1 <- invasion_run_adm_1(results =  adm_3_at_1_sen_del_1.1, intro_loc = 'Sevanagala', scale = 'Division', type = 'Rescaled') 

# Adm 2 at 1 Colombo introduction, R_0 = 1.1
adm_2_at_1_col_sen_1.1 <- agg_to_adm_1(results = adm_2_sen_col_1.1, intro_loc = 'Colombo', scale = '2 at 1', trans = '1.1') 
adm_2_at_1_col_sen_inv_1.1 <- invasion_run_adm_1(results =  adm_2_sen_col_1.1, intro_loc = 'Colombo', scale = 'District', type = 'Rescaled') 

# Adm 2 Sevanagala introduction, R_0 = 1.1
adm_2_at_1_del_sen_1.1 <- agg_to_adm_1(results = adm_2_sen_del_1.1, intro_loc = 'Sevanagala', scale = '2 at 1', trans = '1.1') 
adm_2_at_1_del_sen_inv_1.1 <- invasion_run_adm_1(results = adm_2_sen_del_1.1, intro_loc = 'Sevanagala', scale = 'District', type = 'Rescaled') 

#############
# R_0 = 3.0 #
#############

# Adm 3 at 2 Colombo introduction, R_0 = 3.0
adm_3_at_2_col_sen_3.0 <- agg_to_adm_2(results = adm_3_at_2_sen_col_3.0, intro_loc = 'Colombo', scale = '3 at 2', trans = '3.0') 
adm_3_at_2_col_sen_inv_3.0 <- invasion_run_adm_2(results = adm_3_at_2_sen_col_3.0, intro_loc = 'Colombo', scale = 'Division', type = 'Rescaled') 

# Adm 3 at 2 Sevanagala introduction, R_0 = 3.0
adm_3_at_2_del_sen_3.0 <- agg_to_adm_2(results = adm_3_at_2_sen_del_3.0, intro_loc = 'Sevanagala', scale = '3 at 2', trans = '3.0') 
adm_3_at_2_del_sen_inv_3.0 <- invasion_run_adm_2(results = adm_3_at_2_sen_del_3.0, intro_loc = 'Sevanagala', scale = 'Division', type = 'Rescaled') 

# Adm 3 at 1 Colombo introduction, R_0 = 3.0
adm_3_at_1_col_sen_3.0 <- agg_to_adm_1(results = adm_3_at_1_sen_col_3.0, intro_loc = 'Colombo', scale = '3 at 1', trans = '3.0') 
adm_3_at_1_col_sen_inv_3.0 <- invasion_run_adm_1(results = adm_3_at_1_sen_col_3.0, intro_loc = 'Colombo', scale = 'Division', type = 'Rescaled') 

# Adm 3 at 1 Sevanagala introduction, R_0 = 3.0
adm_3_at_1_del_sen_3.0 <- agg_to_adm_1(results = adm_3_at_1_sen_del_3.0, intro_loc = 'Sevanagala', scale = '3 at 1', trans = '3.0') 
adm_3_at_1_del_sen_inv_3.0 <- invasion_run_adm_1(results =  adm_3_at_1_sen_del_3.0, intro_loc = 'Sevanagala', scale = 'Division', type = 'Rescaled') 

# Adm 2 at 1 Colombo introduction, R_0 = 3.0
adm_2_at_1_col_sen_3.0 <- agg_to_adm_1(results = adm_2_sen_col_3.0, intro_loc = 'Colombo', scale = '2 at 1', trans = '3.0') 
adm_2_at_1_col_sen_inv_3.0 <- invasion_run_adm_1(results =  adm_2_sen_col_3.0, intro_loc = 'Colombo', scale = 'District', type = 'Rescaled') 

# Adm 2 Sevanagala introduction, R_0 = 3.0
adm_2_at_1_del_sen_3.0 <- agg_to_adm_1(results = adm_2_sen_del_3.0, intro_loc = 'Sevanagala', scale = '2 at 1', trans = '3.0') 
adm_2_at_1_del_sen_inv_3.0 <- invasion_run_adm_1(results = adm_2_sen_del_3.0, intro_loc = 'Sevanagala', scale = 'District', type = 'Rescaled') 

#####################
# 3. CREATE FIGURES #
#####################

# Add additional variable to data
adm_3_at_1_col <- adm_3_at_1_col |> mutate(`Mobility Data Type` = 'Original')
adm_3_at_1_del <- adm_3_at_1_del |> mutate(`Mobility Data Type` = 'Original')
adm_3_at_2_col <- adm_3_at_2_col |> mutate(`Mobility Data Type` = 'Original')
adm_3_at_2_del <- adm_3_at_2_del |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_2_col <- adm_2_at_2_col |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_2_del <- adm_2_at_2_del |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_1_col <- adm_2_at_1_col |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_1_del <- adm_2_at_1_del |> mutate(`Mobility Data Type` = 'Original')
adm_1_at_1_col <- adm_1_at_1_col |> mutate(`Mobility Data Type` = 'Original')
adm_1_at_1_del <- adm_1_at_1_del |> mutate(`Mobility Data Type` = 'Original')

adm_3_at_1_col_1.1 <- adm_3_at_1_col_1.1 |> mutate(`Mobility Data Type` = 'Original')
adm_3_at_1_del_1.1 <- adm_3_at_1_del_1.1 |> mutate(`Mobility Data Type` = 'Original')
adm_3_at_2_col_1.1 <- adm_3_at_2_col_1.1 |> mutate(`Mobility Data Type` = 'Original')
adm_3_at_2_del_1.1 <- adm_3_at_2_del_1.1 |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_2_col_1.1 <- adm_2_at_2_col_1.1 |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_2_del_1.1 <- adm_2_at_2_del_1.1 |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_1_col_1.1 <- adm_2_at_1_col_1.1 |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_1_del_1.1 <- adm_2_at_1_del_1.1 |> mutate(`Mobility Data Type` = 'Original')
adm_1_at_1_col_1.1 <- adm_1_at_1_col_1.1 |> mutate(`Mobility Data Type` = 'Original')
adm_1_at_1_del_1.1 <- adm_1_at_1_del_1.1 |> mutate(`Mobility Data Type` = 'Original')

adm_3_at_1_col_3.0 <- adm_3_at_1_col_3.0 |> mutate(`Mobility Data Type` = 'Original')
adm_3_at_1_del_3.0 <- adm_3_at_1_del_3.0 |> mutate(`Mobility Data Type` = 'Original')
adm_3_at_2_col_3.0 <- adm_3_at_2_col_3.0 |> mutate(`Mobility Data Type` = 'Original')
adm_3_at_2_del_3.0 <- adm_3_at_2_del_3.0 |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_2_col_3.0 <- adm_2_at_2_col_3.0 |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_2_del_3.0 <- adm_2_at_2_del_3.0 |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_1_col_3.0 <- adm_2_at_1_col_3.0 |> mutate(`Mobility Data Type` = 'Original')
adm_2_at_1_del_3.0 <- adm_2_at_1_del_3.0 |> mutate(`Mobility Data Type` = 'Original')
adm_1_at_1_col_3.0 <- adm_1_at_1_col_3.0 |> mutate(`Mobility Data Type` = 'Original')
adm_1_at_1_del_3.0 <- adm_1_at_1_del_3.0 |> mutate(`Mobility Data Type` = 'Original')

adm_3_at_1_col_sen <- adm_3_at_1_col_sen |> mutate(`Mobility Data Type` = 'Rescaled')
adm_3_at_1_del_sen <- adm_3_at_1_del_sen |> mutate(`Mobility Data Type` = 'Rescaled')
adm_3_at_2_col_sen <- adm_3_at_2_col_sen |> mutate(`Mobility Data Type` = 'Rescaled')
adm_3_at_2_del_sen <- adm_3_at_2_del_sen |> mutate(`Mobility Data Type` = 'Rescaled')
adm_2_at_1_col_sen <- adm_2_at_1_col_sen |> mutate(`Mobility Data Type` = 'Rescaled')
adm_2_at_1_del_sen <- adm_2_at_1_del_sen |> mutate(`Mobility Data Type` = 'Rescaled')

adm_3_at_1_col_sen_1.1 <- adm_3_at_1_col_sen_1.1 |> mutate(`Mobility Data Type` = 'Rescaled')
adm_3_at_1_del_sen_1.1 <- adm_3_at_1_del_sen_1.1 |> mutate(`Mobility Data Type` = 'Rescaled')
adm_3_at_2_col_sen_1.1 <- adm_3_at_2_col_sen_1.1 |> mutate(`Mobility Data Type` = 'Rescaled')
adm_3_at_2_del_sen_1.1 <- adm_3_at_2_del_sen_1.1 |> mutate(`Mobility Data Type` = 'Rescaled')
adm_2_at_1_col_sen_1.1 <- adm_2_at_1_col_sen_1.1 |> mutate(`Mobility Data Type` = 'Rescaled')
adm_2_at_1_del_sen_1.1 <- adm_2_at_1_del_sen_1.1 |> mutate(`Mobility Data Type` = 'Rescaled')

adm_3_at_1_col_sen_3.0 <- adm_3_at_1_col_sen_3.0 |> mutate(`Mobility Data Type` = 'Rescaled')
adm_3_at_1_del_sen_3.0 <- adm_3_at_1_del_sen_3.0 |> mutate(`Mobility Data Type` = 'Rescaled')
adm_3_at_2_col_sen_3.0 <- adm_3_at_2_col_sen_3.0 |> mutate(`Mobility Data Type` = 'Rescaled')
adm_3_at_2_del_sen_3.0 <- adm_3_at_2_del_sen_3.0 |> mutate(`Mobility Data Type` = 'Rescaled')
adm_2_at_1_col_sen_3.0 <- adm_2_at_1_col_sen_3.0 |> mutate(`Mobility Data Type` = 'Rescaled')
adm_2_at_1_del_sen_3.0 <- adm_2_at_1_del_sen_3.0 |> mutate(`Mobility Data Type` = 'Rescaled')

# Combine data
plot_dat_intro <- rbind(adm_1_at_1_col_1.1, adm_1_at_1_col, adm_1_at_1_col_3.0, 
                        adm_2_at_1_col_1.1, adm_2_at_1_col, adm_2_at_1_col_3.0,
                        adm_2_at_2_col_1.1, adm_2_at_2_col, adm_2_at_2_col_3.0, 
                        adm_3_at_1_col_1.1, adm_3_at_1_col, adm_3_at_1_col_3.0,
                        adm_3_at_2_col_1.1, adm_3_at_2_col, adm_3_at_2_col_3.0,
                        adm_1_at_1_del_1.1, adm_1_at_1_del, adm_1_at_1_del_3.0, 
                        adm_2_at_1_del_1.1, adm_2_at_1_del, adm_2_at_1_del_3.0,
                        adm_2_at_2_del_1.1, adm_2_at_2_del, adm_2_at_2_del_3.0, 
                        adm_3_at_1_del_1.1, adm_3_at_1_del, adm_3_at_1_del_3.0,
                        adm_3_at_2_del_1.1, adm_3_at_2_del, adm_3_at_2_del_3.0,
                        adm_3_at_1_col_sen_1.1, adm_3_at_1_col_sen, adm_3_at_1_col_sen_3.0,
                        adm_3_at_1_del_sen_1.1, adm_3_at_1_del_sen, adm_3_at_1_del_sen_3.0,
                        adm_3_at_2_col_sen_1.1, adm_3_at_2_col_sen, adm_3_at_2_col_sen_3.0,
                        adm_3_at_2_del_sen_1.1, adm_3_at_2_del_sen, adm_3_at_2_del_sen_3.0,
                        adm_2_at_1_col_sen_1.1, adm_2_at_1_col_sen, adm_2_at_1_col_sen_3.0,
                        adm_2_at_1_del_sen_1.1, adm_2_at_1_del_sen, adm_2_at_1_del_sen_3.0)

# Reorder scale variable
plot_dat_intro$Scale_order <- ordered(plot_dat_intro$Scale, levels = c("1", "2 at 1", "3 at 1", "2", "3 at 2"))

# Create indroduction plot
intro_trans <- ggplot(plot_dat_intro) + 
  geom_boxplot(aes(x = Scale_order, y = time, 
                   fill = Scale_order, linetype = `Mobility Data Type`), 
               outliers = FALSE, width = 0.6) +
  #scale_alpha_discrete(range = c(1, 0)) +
  facet_wrap(vars(intro_loc, Trans)) +
  scale_fill_manual(values = c('2'="#9e9ac8", '3 at 2'="#41AE76",'1'= "#4292C6", 
                               '2 at 1' = '#9e9ac8', '3 at 1' = '#41AE76')) +
  theme_bw() + 
  xlab('Scale') +
  ylab('\nTime (days)') +
  ggtitle('Disease Arrival Time by Introduction Location & Transmissibility') +
  scale_y_continuous(limits = c(0, 500),                
                     breaks = c(0, 50, 100, 150, 200, 250)) +
  coord_cartesian(ylim = c(0, 300)) +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        strip.text = element_text(size = 30, color = "black"),
        strip.background = element_rect(fill = "white")) +
  guides(fill = "none")
intro_trans

# Creata magnitude plot
plot_dat_mag <- rbind(adm_1_at_1_col_1.1, adm_1_at_1_col, adm_1_at_1_col_3.0, 
                        adm_2_at_1_col_1.1, adm_2_at_1_col, adm_2_at_1_col_3.0,
                        adm_3_at_1_col_1.1, adm_3_at_1_col, adm_3_at_1_col_3.0,
                        adm_1_at_1_del_1.1, adm_1_at_1_del, adm_1_at_1_del_3.0, 
                        adm_2_at_1_del_1.1, adm_2_at_1_del, adm_2_at_1_del_3.0,
                        adm_3_at_1_del_1.1, adm_3_at_1_del, adm_3_at_1_del_3.0)

plot_dat_mag$Scale <- ifelse(plot_dat_mag$Scale == '2 at 1', 'District', plot_dat_mag$Scale)
plot_dat_mag$Scale <- ifelse(plot_dat_mag$Scale == '3 at 1', 'Division', plot_dat_mag$Scale)
plot_dat_mag$Scale <- ifelse(plot_dat_mag$Scale == '1', 'Province', plot_dat_mag$Scale)

plot_dat_mag <- plot_dat_mag |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 


mag_trans <- ggplot(plot_dat_mag) + geom_boxplot(aes(x = Scale, y = magnitude / sum(adm_1_pop_vec), 
                                                     fill = Scale, color = Scale), 
                                    outliers = F, width = 0.6, size = 1) +
  facet_wrap(vars(intro_loc, Trans)) +
  scale_fill_manual(values = c('District'="#9e9ac8",'Province'= "#4292C6", 'Division' = '#41AE76')) +
  scale_color_manual(values = c('District'="#9e9ac8",'Province'= "#4292C6", 'Division' = '#41AE76')) +
  theme_bw() + 
  xlab('Scale') +
  ylab('\nProportion Infected') +
  ggtitle('Epidemic Magnitude by Introduction Location & Transmissibility') +
  scale_y_continuous(limits = c(0, 1),                
                     breaks = c(0, 0.50, 1)) +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        strip.text = element_text(size = 30, color = "black"),
        strip.background = element_rect(fill = "white")) 


plot <- cowplot::plot_grid(intro_trans,
                           ggplot() + theme_void(),
                           mag_trans,
                           nrow = 3,
                           label_size = 34, hjust = 0,
                           labels = c('(a)', '', '(b)'),
                           rel_heights = c(1, 0.05, 0.9))

ggsave('./figs/figure_S.7.jpg', plot = mag_trans , height = 14, width = 25)


int_del_obs_all <- rbind(adm_3_at_2_del_inv, adm_2_at_2_del_inv, adm_3_at_2_del_sen_inv)

int_col_obs_all <- rbind(adm_3_at_2_col_inv, adm_2_at_2_col_inv, adm_3_at_2_col_sen_inv)

adm_2_order_del <- adm_2_at_2_del_inv |>
  group_by(adm_2) |>
  mutate(median = median(time)) |>
  distinct(adm_2, median) |>
  ungroup() |>
  arrange(median) |>
  mutate(Order = row_number())

adm_2_order_col <- adm_2_at_2_col_inv |>
  group_by(adm_2) |>
  mutate(median = median(time)) |>
  distinct(adm_2, median) |>
  ungroup() |>
  arrange(median) |>
  mutate(Order = row_number())

# dist_mad <- as.data.frame(1 - log(adm_1_phone_mobility_mat[5,]))
# dist_mad$name <- rownames(dist_mad)
# names(dist_mad)[1] <- "dist"
# 
# dist_col <- as.data.frame(1 - log(adm_1_phone_mobility_mat[9,]))
# dist_col$name <- rownames(dist_col)
# names(dist_col)[1] <- "dist"

int_mad_obs_all <- left_join(int_del_obs_all, adm_2_order_del, by = c('adm_2' = 'adm_2'))
int_col_obs_all <- left_join(int_col_obs_all, adm_2_order_col, by = c('adm_2' = 'adm_2'))

line_plot_col_obs <- ggplot(int_col_obs_all, aes(x = time, y = fct_reorder(adm_2, Order), fill = Scale, linetype = `Mobility Data Type`)) +
  #geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, 
  #scale="width", width = 0.6, position = position_dodge(width = 0.9)) +
  geom_boxplot(position = position_dodge(width = 0.65), width=0.5, color = 'black', outlier.shape = NA, coef = 0) +
  theme_minimal() + coord_cartesian(xlim = c(0, 140)) +
  scale_fill_manual(values = c('District'="#41AE76", 'Division'="#9e9ac8",'Province'= "#4292C6")) +
  theme(legend.position = 'none') +
  ylab('District') +
  xlab('Time (days)') +
  ggtitle('Districts Infected') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) 
line_plot_col_obs

line_plot_mad_obs <- ggplot(int_mad_obs_all, aes(x = time, y = fct_reorder(adm_2, Order), fill = Scale, linetype = `Mobility Data Type`)) +
  #geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, 
  #scale="width", width = 0.6, position = position_dodge(width = 0.9)) +
  geom_boxplot(position = position_dodge(width = 0.65), width=0.5, color = 'black', outlier.shape = NA, coef = 0) +
  theme_minimal() + coord_cartesian(xlim = c(0, 140)) +
  scale_fill_manual(values = c('District'="#41AE76", 'Division'="#9e9ac8",'Province'= "#4292C6")) +
  theme(legend.position = 'none') +
  ylab('District') +
  xlab('Time (days)') +
  ggtitle('Delft: Disease Arrival Time by District') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) 
line_plot_mad_obs

int_del_obs_all_1 <- rbind(adm_3_at_1_del_inv, adm_2_at_1_del_inv, adm_1_at_1_del_inv, adm_3_at_1_del_sen_inv, adm_2_at_1_del_sen_inv)

int_col_obs_all_1 <- rbind(adm_3_at_1_col_inv, adm_2_at_1_col_inv, adm_1_at_1_col_inv, adm_3_at_1_col_sen_inv, adm_2_at_1_col_sen_inv)

adm_1_order_del <- adm_1_at_1_del_inv |>
  group_by(adm_1) |>
  mutate(median = median(time)) |>
  distinct(adm_1, median) |>
  ungroup() |>
  arrange(median) |>
  mutate(Order = row_number())

adm_1_order_col <- adm_1_at_1_col_inv |>
  group_by(adm_1) |>
  mutate(median = median(time)) |>
  distinct(adm_1, median) |>
  ungroup() |>
  arrange(median) |>
  mutate(Order = row_number())

del_1.5 <- int_del_obs_all_1 |>
  group_by(adm_1, Scale, `Mobility Data Type`) |>
  mutate(median = median(time)) |>
  distinct(adm_1, Scale, `Mobility Data Type`, median)

del_1.5_prov <- del_1.5 |> filter(Scale == 'Province')

del_1.5 <- left_join(del_1.5, del_1.5_prov[,c(1, 4)], by = 'adm_1')

del_1.5_med <- del_1.5 |> group_by(Scale, `Mobility Data Type`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale, `Mobility Data Type`, rmse) |>
  mutate(trans = '1.5')

col_1.5 <- int_col_obs_all_1 |>
  group_by(adm_1, Scale, `Mobility Data Type`) |>
  mutate(median = median(time)) |>
  distinct(adm_1, Scale, `Mobility Data Type`, median)

col_1.5_prov <- col_1.5 |> filter(Scale == 'Province')

col_1.5 <- left_join(col_1.5, col_1.5_prov[,c(1, 4)], by = 'adm_1')

col_1.5_med <- col_1.5 |> group_by(Scale, `Mobility Data Type`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale, `Mobility Data Type`, rmse) |>
  mutate(trans = '1.5')




int_del_obs_all_1.1 <- rbind(adm_3_at_1_del_inv_1.1, adm_2_at_1_del_inv_1.1, 
                             adm_1_at_1_del_inv_1.1, adm_3_at_1_del_sen_inv_1.1, 
                             adm_2_at_1_del_sen_inv_1.1)

int_col_obs_all_1.1 <- rbind(adm_3_at_1_col_inv_1.1, adm_2_at_1_col_inv_1.1, 
                             adm_1_at_1_col_inv_1.1, adm_3_at_1_col_sen_inv_1.1, 
                             adm_2_at_1_col_sen_inv_1.1)

del_1.1 <- int_del_obs_all_1.1 |>
  group_by(adm_1, Scale, `Mobility Data Type`) |>
  mutate(median = median(time)) |>
  distinct(adm_1, Scale, `Mobility Data Type`, median)

del_1.1_prov <- del_1.1 |> filter(Scale == 'Province')

del_1.1 <- left_join(del_1.1, del_1.1_prov[,c(1, 4)], by = 'adm_1')

del_1.1_med <- del_1.1 |> group_by(Scale, `Mobility Data Type`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale, `Mobility Data Type`, rmse) |>
  mutate(trans = '1.1')

col_1.1 <- int_col_obs_all_1.1 |>
  group_by(adm_1, Scale, `Mobility Data Type`) |>
  mutate(median = median(time)) |>
  distinct(adm_1, Scale, `Mobility Data Type`, median)

col_1.1_prov <- col_1.1 |> filter(Scale == 'Province')

col_1.1 <- left_join(col_1.1, col_1.1_prov[,c(1, 4)], by = 'adm_1')

col_1.1_med <- col_1.1 |> group_by(Scale, `Mobility Data Type`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale, `Mobility Data Type`, rmse) |>
  mutate(trans = '1.1')




int_del_obs_all_3.0 <- rbind(adm_3_at_1_del_inv_3.0, adm_2_at_1_del_inv_3.0, 
                             adm_1_at_1_del_inv_3.0, adm_3_at_1_del_sen_inv_3.0, 
                             adm_2_at_1_del_sen_inv_3.0)

int_col_obs_all_3.0 <- rbind(adm_3_at_1_col_inv_3.0, adm_2_at_1_col_inv_3.0, 
                             adm_1_at_1_col_inv_3.0, adm_3_at_1_col_sen_inv_3.0, 
                             adm_2_at_1_col_sen_inv_3.0)

del_3.0 <- int_del_obs_all_3.0 |>
  group_by(adm_1, Scale, `Mobility Data Type`) |>
  mutate(median = median(time)) |>
  distinct(adm_1, Scale, `Mobility Data Type`, median)

del_3.0_prov <- del_3.0 |> filter(Scale == 'Province')

del_3.0 <- left_join(del_3.0, del_3.0_prov[,c(1, 4)], by = 'adm_1')

del_3.0_med <- del_3.0 |> group_by(Scale, `Mobility Data Type`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale, `Mobility Data Type`, rmse) |>
  mutate(trans = '3.0')

col_3.0 <- int_col_obs_all_3.0 |>
  group_by(adm_1, Scale, `Mobility Data Type`) |>
  mutate(median = median(time)) |>
  distinct(adm_1, Scale, `Mobility Data Type`, median)

col_3.0_prov <- col_3.0 |> filter(Scale == 'Province')

col_3.0 <- left_join(col_3.0, col_3.0_prov[,c(1, 4)], by = 'adm_1')

col_3.0_med <- col_3.0 |> group_by(Scale, `Mobility Data Type`) |>
  mutate(rmse = sqrt(mean((median.x - median.y)^2))) |>
  distinct(Scale, `Mobility Data Type`, rmse) |>
  mutate(trans = '3.0')



rmse_col <- rbind(col_1.5_med, col_1.1_med, col_3.0_med)
rmse_col <- rmse_col |> dplyr::filter(Scale != 'Province')
rmse_col <- rmse_col |> ungroup() |>
  mutate(`Mobility Data Type` = factor(`Mobility Data Type`, levels=c( "Rescaled", "Original"))) |>
  mutate(trans = factor(trans, levels=c('3.0', '1.5', '1.1'))) |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 
  

rmse_plot_col_obs_1 <- ggplot(rmse_col) +
  geom_bar_pattern(aes(x = trans, y = rmse, fill = Scale, pattern = `Mobility Data Type`), 
                   color = 'black', stat = "identity", position = "dodge", linewidth = 1) +
  labs(x = expression(~ R[0]), y = "RMSE (days)", fill = "Scale") +
  theme_minimal() +  coord_flip() + scale_y_continuous(limits = c(0, 45), breaks = c(0, 10, 20, 30, 40)) +
  scale_pattern_manual(values = c('Original' = 'none', 'Rescaled' = 'stripe')) +
  scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  guides(linetype = guide_legend(override.aes = list(fill = "white"))) + ggtitle('\nDifference from Province Model') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) 
  
    
library(ggpattern)
rmse_del <- rbind(del_1.5_med, del_1.1_med, del_3.0_med)
rmse_del <- rmse_del |> dplyr::filter(Scale != 'Province')
rmse_del <- rmse_del |> ungroup() |>
  mutate(`Mobility Data Type` = factor(`Mobility Data Type`, levels=c( "Rescaled", "Original"))) |>
  mutate(trans = factor(trans, levels=c('3.0', '1.5', '1.1'))) |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 


rmse_plot_mad_obs_1 <- ggplot(rmse_del) +
  geom_bar_pattern(aes(x = trans, y = rmse, fill = Scale, pattern = `Mobility Data Type`), 
           color = 'black', stat = "identity", position = "dodge", linewidth = 1) +
  labs(x = expression(~ R[0]), y = "RMSE (days)", fill = "Scale") +
  theme_minimal() +  coord_flip() + scale_y_continuous(limits = c(0, 45), breaks = c(0, 10, 20, 30, 40)) +
  scale_pattern_manual(values = c('Original' = 'none', 'Rescaled' = 'stripe')) +
  scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  guides(linetype = guide_legend(override.aes = list(fill = "white")))  + ggtitle('\nDifference from Province Model') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) 

  
# dist_mad <- as.data.frame(1 - log(adm_1_phone_mobility_mat[5,]))
# dist_mad$name <- rownames(dist_mad)
# names(dist_mad)[1] <- "dist"
# 
# dist_col <- as.data.frame(1 - log(adm_1_phone_mobility_mat[9,]))
# dist_col$name <- rownames(dist_col)
# names(dist_col)[1] <- "dist"

int_mad_obs_all_1 <- left_join(int_del_obs_all_1, adm_1_order_del, by = c('adm_1' = 'adm_1'))
int_col_obs_all_1 <- left_join(int_col_obs_all_1, adm_1_order_col, by = c('adm_1' = 'adm_1'))

int_col_obs_all_1 <- int_col_obs_all_1 |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 


line_plot_col_obs_1 <- ggplot(int_col_obs_all_1, aes(x = time, y = fct_reorder(adm_1, Order), fill = Scale, pattern = `Mobility Data Type`)) +
  #geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, 
  #scale="width", width = 0.6, position = position_dodge(width = 0.9)) +
  geom_boxplot_pattern(position = position_dodge(width = 0.75), width=0.5, color = 'black', outlier.shape = NA, coef = 0, linewidth = 1) +
  theme_minimal() + coord_cartesian(xlim = c(0, 110)) +
  scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  scale_pattern_manual(values = c('Original' = 'none', 'Rescaled' = 'stripe')) +
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
line_plot_col_obs_1

int_mad_obs_all_1 <- int_mad_obs_all_1 |> ungroup() |>
  mutate(Scale = factor(Scale, levels=c("Division", "District", "Province"))) 


line_plot_mad_obs_1 <- ggplot(int_mad_obs_all_1, aes(x = time, y = fct_reorder(adm_1, Order), fill = Scale, pattern = `Mobility Data Type`)) +
  #geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, 
  #scale="width", width = 0.6, position = position_dodge(width = 0.9)) +
  geom_boxplot_pattern(position = position_dodge(width = 0.75), width=0.5, color = 'black', outlier.shape = NA, coef = 0, linewidth = 1) +
  theme_minimal() + coord_cartesian(xlim = c(0, 110)) +
  scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  scale_pattern_manual(values = c('Original' = 'none', 'Rescaled' = 'stripe')) +
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
line_plot_mad_obs_1


legend <- ggplot(int_mad_obs_all_1, aes(x = mean(time), y = fct_reorder(adm_1, Order), fill = Scale, pattern = `Mobility Data Type`)) +
  #geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, 
  #scale="width", width = 0.6, position = position_dodge(width = 0.9)) +
  geom_bar_pattern(aes(fill = Scale, pattern = `Mobility Data Type`), 
                   color = 'black', stat = "identity", position = "dodge", linewidth = 1,
                   pattern_density = 0.1,
                   pattern_spacing = 0.5,
                   pattern_key_scale_factor = 0.6,
                   pattern_fill = "black",
                   pattern_angle = 45) +
  theme_minimal() + coord_cartesian(xlim = c(0, 110)) +
  scale_fill_manual(values = c('District'="#9e9ac8", 'Division'="#41AE76",'Province'= "#4292C6")) +
  scale_pattern_manual(values = c('Original' = 'none', 'Rescaled' = 'stripe')) +
  ylab('Province') + 
  xlab('Time (days)') +
  ggtitle('Province Infection Time') +
  theme(plot.title = element_text(size=34, hjust = 0.5),
        axis.title = element_text(size=34),
        axis.text = element_text(size=30),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 38),
        legend.title = element_text(size = 44),
        legend.key.width = unit(2, "cm")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))


legend_get <- get_legend(legend)

row_1_1 <- cowplot::plot_grid(ggplot() + theme_void(), ggplot() + theme_void(),
                              line_plot_col_obs_1, line_plot_mad_obs_1, 
                              rmse_plot_col_obs_1, rmse_plot_mad_obs_1,
                              nrow = 3, labels = c('Colombo Introduction Event', 'Sevanagala Introduction Event',
                                                   '(a)', '(b)', '', ''),
                              label_size = 34,
                              rel_heights = c(0.05, 1, 0.45))


plot <- cowplot::plot_grid(row_1_1, legend_get,
                           nrow = 2, rel_heights = c(1, 0.1),
                           label_size = 34, hjust = 0)

ggsave('./figs/figure_5_spectrum_new.jpg', plot = plot, height = 25, width = 25)


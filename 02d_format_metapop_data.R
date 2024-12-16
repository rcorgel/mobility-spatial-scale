################################################################################
# File Name: 02d_format_metapop_data                                           #
# Purpose:   Format and save data used in the metapopulation model.            #                                                  
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Format population data                                            #
#         3. Create admin name vectors                                         #
#         4. Create admin level crosswalks                                     #
#         5. Save data                                                         #
#                                                                              #
# Project: Sri Lanka Spatial Aggregation                                       #
# Author: Ronan Corgel                                                         #
################################################################################

####################
# 1. SET-UP SCRIPT #
####################

# Start with a clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggplot2)
library(reshape2)
library(assertr)

# Set the seed
set.seed(12345)

# Set directory 
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project')

#############################
# 2. FORMAT POPULATION DATA #
#############################

# Load population data for all administrative units
load('./tmp/adm_population_dat.RData')

# Administrative Unit 3
adm_3_population_dat <- adm_3_population_dat |>
  arrange(adm_3_mobility)
adm_3_pop_vec <- adm_3_population_dat$population_2020_adm_3

# Administrative Unit 2
adm_2_population_dat <- adm_2_population_dat |>
  arrange(adm_2)
adm_2_pop_vec <- adm_2_population_dat$population_2020_adm_2

# Administrative Unit 1
adm_1_population_dat <- adm_1_population_dat |>
  arrange(adm_1)
adm_1_pop_vec <- adm_1_population_dat$population_2020_adm_1

################################
# 3. CREATE ADMIN NAME VECTORS #
################################

# Administrative Unit 3
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')
adm_3_name_vec <- colnames(adm_3_phone_mobility_mat)

# Administrative Unit 2
load('./tmp/fmt_adm_2_phone_mobility_dat.RData')
adm_2_name_vec <- colnames(adm_2_phone_mobility_mat)

# Administrative Unit 1
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')
adm_1_name_vec <- colnames(adm_1_phone_mobility_mat)

####################################
# 4. CREATE ADMIN LEVEL CROSSWALKS #
####################################

# Load admin crosswalk
admin_xwalk <- readRDS('./tmp/admin_xwalk.rds')

# Administrative Unit 3
adm_3_x_walk <- admin_xwalk |> 
  group_by(adm_3) |>
  dplyr::distinct(adm_3, .keep_all = TRUE) |>
  dplyr::select(c('adm_3', 'adm_2', 'adm_1')) 

# Administrative Unit 2
adm_2_x_walk <- admin_xwalk |> 
  group_by(adm_2) |>
  dplyr::distinct(adm_2, .keep_all = TRUE) |>
  dplyr::select(c('adm_2', 'adm_1')) 

# Administrative Unit 1
# Do not need to do this for admin 1

################
# 5. SAVE DATA #
################

# Administrative Unit 3
save(list = c('adm_3_phone_mobility_mat',
              'adm_3_name_vec', 'adm_3_pop_vec', 'adm_3_x_walk'), 
     file = './tmp/adm_3_metapop_dat.RData')

# Administrative Unit 2
save(list = c('adm_2_phone_mobility_mat', 
              'adm_2_name_vec', 'adm_2_pop_vec', 'adm_2_x_walk'), 
     file = './tmp/adm_2_metapop_dat.RData')

# Administrative Unit 1
save(list = c('adm_1_phone_mobility_mat',
              'adm_1_name_vec', 'adm_1_pop_vec'), 
     file = './tmp/adm_1_metapop_dat.RData')

################################################################################
################################################################################

################################################################################
# File Name: 02f_format_metapop_data                                           #
# Purpose:   Format and save data used in the metapopulation model.            #                                                  
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Format population data                                            #
#         3. Create admin name vectors                                         #
#         4. Create admin level crosswalks                                     #
#         5. Save data                                                         #
#                                                                              #
# Project: Mobility Spatial Scale                                              #
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
adm_1_population_dat <- readRDS('./out/adm_1_population_dat.rds')
adm_2_population_dat <- readRDS('./out/adm_2_population_dat.rds')
adm_3_population_dat <- readRDS('./out/adm_3_population_dat.rds')

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

## Create new population data ##

# Load new crosswalk
admin_xwalk_new <- readRDS(file = './out/admin_xwalk_new.rds')

# Merge to Admin 3 data
adm_3_population_dat_new <- left_join(adm_3_population_dat, admin_xwalk_new, 
                                  by = c('adm_3_mobility' = 'adm_3'))

# Sum to admin 2
adm_2_population_dat_new <- adm_3_population_dat_new |>
  group_by(adm_2.y, adm_1.y) |>
  mutate(population_2020_adm_2 = sum(population_2020_adm_3)) |>
  distinct(adm_2.y, adm_1.y, population_2020_adm_2) |>
  dplyr::rename('adm_2' = 'adm_2.y',
                'adm_1' = 'adm_1.y') |>
  arrange(adm_2)
adm_2_pop_vec_new <- adm_2_population_dat_new$population_2020_adm_2

# Sum to admin 1
adm_1_population_dat_new <- adm_3_population_dat_new |>
  group_by(adm_1.y) |>
  mutate(population_2020_adm_1 = sum(population_2020_adm_3)) |>
  distinct(adm_1.y, population_2020_adm_1) |>
  dplyr::rename('adm_1' = 'adm_1.y') |>
  arrange(adm_1)
adm_1_pop_vec_new <- adm_1_population_dat_new$population_2020_adm_1

################################
# 3. CREATE ADMIN NAME VECTORS #
################################

# Administrative Unit 3
adm_3_phone_mobility_mat <- readRDS('./out/adm_3_phone_mobility_mat.rds')
adm_3_name_vec <- colnames(adm_3_phone_mobility_mat)
sorted_vector <- sort(adm_3_name_vec)
adm_3_name_vec == sorted_vector # confirm sorting is alphabetical
adm_3_population_dat$adm_3_mobility == sorted_vector # confirm population is alphabetical

# Administrative Unit 2
adm_2_phone_mobility_mat <- readRDS('./out/adm_2_phone_mobility_mat.rds')
adm_2_name_vec <- colnames(adm_2_phone_mobility_mat)
sorted_vector <- sort(adm_2_name_vec)
adm_2_name_vec == sorted_vector # confirm sorting is alphabetical
adm_2_population_dat$adm_2 == sorted_vector # confirm population is alphabetical

# Administrative Unit 2 (NEW)
adm_2_phone_mobility_mat_new <- readRDS('./out/adm_2_phone_mobility_mat_new.rds')
adm_2_name_vec_new <- colnames(adm_2_phone_mobility_mat_new)
sorted_vector_new <- order(as.numeric(adm_2_name_vec_new))
adm_2_name_vec_new == sorted_vector_new # confirm sorting is alphabetical
adm_2_population_dat_new$adm_2 == sorted_vector_new # confirm population is alphabetical

# Administrative Unit 1
adm_1_phone_mobility_mat <- readRDS('./out/adm_1_phone_mobility_mat.rds')
adm_1_name_vec <- colnames(adm_1_phone_mobility_mat)
sorted_vector <- sort(adm_1_name_vec)
adm_1_name_vec == sorted_vector # confirm sorting is alphabetical
adm_1_population_dat$adm_1 == sorted_vector # confirm population is alphabetical

# Administrative Unit 1 (NEW)
adm_1_phone_mobility_mat_new <- readRDS('./out/adm_1_phone_mobility_mat_new.rds')
adm_1_name_vec_new <- colnames(adm_1_phone_mobility_mat_new)
sorted_vector_new <- order(as.numeric(adm_1_name_vec_new))
adm_1_name_vec == sorted_vector # confirm sorting is alphabetical
adm_1_population_dat$adm_1 == sorted_vector # confirm population is alphabetical

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

# Administrative Unit 3 (NEW)
adm_3_x_walk_new <- admin_xwalk_new |> 
  group_by(adm_3) |>
  dplyr::distinct(adm_3, .keep_all = TRUE) |>
  dplyr::select(c('adm_3', 'adm_2', 'adm_1')) 

# Administrative Unit 2
adm_2_x_walk <- admin_xwalk |> 
  group_by(adm_2) |>
  dplyr::distinct(adm_2, .keep_all = TRUE) |>
  dplyr::select(c('adm_2', 'adm_1')) 

# Administrative Unit 2 (NEW)
adm_2_x_walk_new <- admin_xwalk_new |> 
  group_by(adm_2) |>
  dplyr::distinct(adm_2, .keep_all = TRUE) |>
  dplyr::select(c('adm_2', 'adm_1')) 

# Administrative Unit 1
# Do not need to do this for admin 1

################
# 5. SAVE DATA #
################

# Administrative Unit 3
saveRDS(adm_3_name_vec, './out/adm_3_name_vec.rds')
saveRDS(adm_3_pop_vec, './out/adm_3_pop_vec.rds')
saveRDS(adm_3_x_walk, './out/adm_3_x_walk.rds')

# Administrative Unit 3 (NEW)
saveRDS(adm_3_x_walk_new, './out/adm_3_x_walk_new.rds')

# Administrative Unit 2
saveRDS(adm_2_name_vec, './out/adm_2_name_vec.rds')
saveRDS(adm_2_pop_vec, './out/adm_2_pop_vec.rds')
saveRDS(adm_2_x_walk, './out/adm_2_x_walk.rds')

# Administrative Unit 2 (NEW)
saveRDS(adm_2_name_vec_new, './out/adm_2_name_vec_new.rds')
saveRDS(adm_2_pop_vec_new, './out/adm_2_pop_vec_new.rds')
saveRDS(adm_2_x_walk_new, './out/adm_2_x_walk_new.rds')

# Administrative Unit 1
saveRDS(adm_1_name_vec, './out/adm_1_name_vec.rds')
saveRDS(adm_1_pop_vec, './out/adm_1_pop_vec.rds')

# Administrative Unit 1 (NEW)
saveRDS(adm_1_name_vec_new, './out/adm_1_name_vec_new.rds')
saveRDS(adm_1_pop_vec_new, './out/adm_1_pop_vec_new.rds')

################################################################################
################################################################################

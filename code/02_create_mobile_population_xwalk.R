################################################################################
# File Name: 02_create_mobile_population_xwalk                                 #
#                                                                              #
# Purpose:   Since administrative 3 units are not one to one between the       #
#            mobile phone data and the shape files, a cross walk needs to be   #
#            created to link the two data sets.                                #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load and format mobile phone data                              #
#            3. Load and format admin 3 shape files                            #
#            4. Merge mobile phone and spatial data                            #
#            5. Create cross walk                                              #
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
library(lubridate)
library(geosphere)
library(raster)
library(sf)
library(sp)
library(readxl)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution')








load(file = './tmp/mobility_dat.RData')
mob_dat <- mobility_dat %>% group_by(admin_level_3_origin) %>% 
  distinct(admin_level_3_origin, admin_level_2_origin, admin_level_1_origin, .keep_all = FALSE) %>%
  filter(admin_level_3_origin != '[unknown]') %>% mutate(merge = admin_level_3_origin)

polygon_dat <- polygon_dat %>% 
  dplyr::select(c('ADM3_EN', 'ADM2_EN', 'ADM1_EN')) %>%
  mutate(merge = ADM3_EN)

join <- full_join(mob_dat, polygon_dat, by = c('merge' = 'merge'))
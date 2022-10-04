################################################################################
# File Name: 01c_process_population_data                                       #
#                                                                              #
# Purpose:   Load and format population data used in analyses.                 #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load and format population data                                #
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

######################################
# 2. LOAD AND FORMAT POPULATION DATA #
######################################

# Load population count data
# Downloaded from: https://hub.worldpop.org/geodata/summary?id=49763
population_dat <- raster('./raw/lka_ppp_2020_constrained.tif')

# Convert raster to data frame
population_dat <- as.data.frame(rasterToPoints(population_dat))
population_dat <- population_dat %>% 
  dplyr::rename('lat' = 'y',
                'long' = 'x')

# Save data to load in ArcGIS
write.csv(population_dat, './tmp/population_dat.csv', row.names = FALSE)

# Load administrative 3 map data
# Downloaded from: https://data.humdata.org/dataset/cod-ab-lka
polygon_dat <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                       layer = 'lka_admbnda_adm3_slsd_20220816')
polygon_dat <- as.data.frame(polygon_dat)

# Perform spatial join in ArcGIS
# After loading each data set in ArcGIS, performed a spatial join matching the 
# centroid of each raster pixel to the closest administrative 3 unit within 1 km. 

# Load joined spatial data
population_dat <- read_excel('./tmp/population_dat_join.xlsx')

# Format and rename the joined data
population_dat <- population_dat %>% 
  dplyr::select(c('dist', 'lat', 'long', 'lka_ppp_2020_constrained', 'ADM3_EN', 
                  'ADM2_EN', 'ADM1_EN')) %>%
  dplyr::rename('distance_match' = 'dist',
                'long' = 'lat', # renamed incorrectly earlier
                'lat' = 'long', # renamed incorrectly earlier
                'population_2020_pixel' = 'lka_ppp_2020_constrained',
                'admin_level_3' = 'ADM3_EN',
                'admin_level_2' = 'ADM2_EN',
                'admin_level_1' = 'ADM1_EN') 

# Aggregate to the admin 3 level
admin_3_population_dat <- population_dat %>% 
  dplyr::group_by(admin_level_3) %>%
  mutate(population_2020_polygon = sum(population_2020_pixel)) %>%
  distinct(admin_level_3, admin_level_2, admin_level_1, population_2020_polygon, 
           .keep_all = FALSE)

# Load xwalk to link population admin 3 units to mobile phone admin 3 units
population_mobile_xwalk <- read.csv('./raw/population_mobile_xwalk.csv')

# Merge on the xwalk
admin_3_population_dat <- left_join(admin_3_population_dat, population_mobile_xwalk,
                                    by = c('admin_level_3' = 'admin_level_3_population'))

# Re-aggregate to the admin level 3 in the mobile phone data
# Aggregate to the admin 3 level
admin_3_population_dat <- admin_3_population_dat %>% 
  dplyr::group_by(admin_level_3_mobile) %>%
  mutate(population_2020_adm_3 = sum(population_2020_polygon)) %>%
  distinct(admin_level_3_mobile, admin_level_2, admin_level_1, 
           population_2020_adm_3, .keep_all = FALSE)

# Aggregate to the admin 2 level
admin_2_population_dat <- admin_3_population_dat %>% 
  dplyr::group_by(admin_level_2) %>%
  mutate(population_2020_adm_2 = sum(population_2020_adm_3)) %>%
  distinct(admin_level_2, admin_level_1, 
           population_2020_adm_2, .keep_all = FALSE)

# Aggregate to the admin 1 level
admin_1_population_dat <- admin_2_population_dat %>% 
  dplyr::group_by(admin_level_1) %>%
  mutate(population_2020_adm_1 = sum(population_2020_adm_2)) %>%
  distinct(admin_level_1, population_2020_adm_1, .keep_all = FALSE)

# Save various administrative levels of population data
save(admin_3_population_dat, file = './tmp/admin_3_population_dat.RData')
save(admin_2_population_dat, file = './tmp/admin_2_population_dat.RData')
save(admin_1_population_dat, file = './tmp/admin_1_population_dat.RData')

################################################################################
################################################################################

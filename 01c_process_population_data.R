################################################################################
# File Name: 01c_process_population_data                                       #
#                                                                              #
# Purpose:   Load and format population data used in analyses.                 #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load and format population data                                #
#            3. Load UN data and apply scaling factor                          #
#            4. Aggregate data and merge on cross-walk                         #
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
library(sp)
library(raster)
library(sf)
library(readxl)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

######################################
# 2. LOAD AND FORMAT POPULATION DATA #
######################################

# Load population count data
# Downloaded from: https://hub.worldpop.org/geodata/summary?id=49763
population_dat <- raster('./raw/lka_ppp_2020_UNadj_constrained.tif')

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
# centroid of each raster pixel to the closest administrative 3 unit within 0.5 km. 

# Load joined spatial data
population_dat <- read_excel('./tmp/population_dat_spatial_join.xlsx')

# Format and rename the joined data
population_dat <- population_dat %>% 
  dplyr::select(c('distance', 'lat', 'long', 'lka_ppp_2020_UNadj_constrained', 'ADM3_EN', 
                  'ADM2_EN', 'ADM1_EN')) %>%
  dplyr::rename('distance_match' = 'distance',
                'population_2020_pixel' = 'lka_ppp_2020_UNadj_constrained',
                'adm_3' = 'ADM3_EN',
                'adm_2' = 'ADM2_EN',
                'adm_1' = 'ADM1_EN') 

############################################
# 3. LOAD UN DATA AND APPLY SCALING FACTOR #
############################################

# Load data
# Source: https://population.un.org/wpp/Download/Standard/MostUsed/
un_pop_est <- read_excel('./raw/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx',
                         range = ('A17:M20613'))
# Subset to Sri Lanka
un_pop_est <- un_pop_est[un_pop_est$`Region, subregion, country or area *` == 'Sri Lanka',]

# Calculate scaling factor (2021 July / 2020 Jan)
multiplier <- as.numeric(un_pop_est[un_pop_est$Year == 2021, 13]) / 
  as.numeric(un_pop_est[un_pop_est$Year == 2020, 12])

# Apply scaling factor
population_dat <- population_dat %>%
  mutate(population_2020_pixel_adj = population_2020_pixel * multiplier)

#############################################
# 4. AGGREGATE DATA AND MERGE ON CROSS-WALK #
#############################################

# Aggregate to the admin 3 level
adm_3_population_dat <- population_dat %>% 
  dplyr::group_by(adm_3) %>%
  mutate(population_2020_polygon = sum(population_2020_pixel)) %>%
  distinct(adm_3, adm_2, adm_1, population_2020_polygon, 
           .keep_all = FALSE)

# Load xwalk to link population admin 3 units to mobile phone admin 3 units
mobility_shape_xwalk <- read.csv('./tmp/mobility_shape_xwalk.csv')

# Merge on the xwalk
adm_3_population_dat <- left_join(adm_3_population_dat, mobility_shape_xwalk,
                                    by = c('adm_3' = 'adm_3_shape'))

# Re-aggregate to the admin level 3 in the mobile phone data
# Aggregate to the admin 3 level
adm_3_population_dat <- adm_3_population_dat %>% 
  dplyr::group_by(adm_3_mobility) %>%
  mutate(population_2020_adm_3 = sum(population_2020_polygon)) %>%
  distinct(adm_3_mobility, adm_2, adm_1, 
           population_2020_adm_3, .keep_all = FALSE)

# Aggregate to the admin 2 level
adm_2_population_dat <- adm_3_population_dat %>% 
  dplyr::group_by(adm_2) %>%
  mutate(population_2020_adm_2 = sum(population_2020_adm_3)) %>%
  distinct(adm_2, adm_1, 
           population_2020_adm_2, .keep_all = FALSE)

# Aggregate to the admin 1 level
adm_1_population_dat <- adm_2_population_dat %>% 
  dplyr::group_by(adm_1) %>%
  mutate(population_2020_adm_1 = sum(population_2020_adm_2)) %>%
  distinct(adm_1, population_2020_adm_1, .keep_all = FALSE)

# Save various administrative levels of population data
save(adm_3_population_dat, file = './tmp/adm_3_population_dat.RData')
save(adm_2_population_dat, file = './tmp/adm_2_population_dat.RData')
save(adm_1_population_dat, file = './tmp/adm_1_population_dat.RData')

################################################################################
################################################################################

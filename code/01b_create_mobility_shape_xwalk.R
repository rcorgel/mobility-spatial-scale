################################################################################
# File Name: 01b_create_mobility_shape_xwalk                                   #
#                                                                              #
# Purpose:   Since administrative 3 units are not one to one between the       #
#            mobility data and the shape files, a cross walk needs to be       #
#            created to link the two data sets.                                #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load and format mobility data and shape files                  #
#            3. Merge mobility and spatial data                                #
#            4. Create cross-walk                                              #
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

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution')

###############################################
# 2. LOAD AND FORMAT MOBILITY AND SHAPE FILES #
###############################################

# Load processed mobility data
load(file = './tmp/mobility_dat.RData')

# Select only administrative unit variables
mobility_dat_merge <- mobility_dat %>% 
  group_by(admin_level_3_origin) %>% 
  distinct(admin_level_3_origin, admin_level_2_origin, admin_level_1_origin, .keep_all = FALSE) %>%
  filter(admin_level_3_origin != '[unknown]') %>% # remove unknowns
  mutate(merge = admin_level_3_origin)            # rename to merge

# Load shape files
# Downloaded from: https://data.humdata.org/dataset/cod-ab-lka
polygon_dat <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                       layer = 'lka_admbnda_adm3_slsd_20220816')

# Convert to data frame
polygon_dat <- as.data.frame(polygon_dat) 

# Select only administrative unit variables
polygon_dat_merge <- polygon_dat %>% 
  dplyr::select(c('ADM3_EN', 'ADM2_EN', 'ADM1_EN')) %>%
  mutate(merge = ADM3_EN) # rename to merge

######################################
# 3. MERGE MOBILITY AND SPATIAL DATA #
######################################

# Merge the two data sets together to examine mismatches
# There are 330 admin unit 3's in the mobility data
# There are 339 admin unit 3's in the shape file
merge_dat <- full_join(mobility_dat_merge, polygon_dat_merge, 
                       by = c('merge' = 'merge'))

# Merged data has 342 observations
# 3 admin 3 units in mobility data but not in shape files
# Ambagamuwa, Galle4Gravets, Kothmale

# 12 admin 3 units in shape files but not in mobility data
# Kothmale East, Nildandahinna, Ambagamuwa Korale, Mathurata, Kothmale West,
# Thalawakele, Norwood, Galle 4 Gravets, Wanduramba, Madampagama, Rathgama,
# Kalthota

########################
# 4. CREATE CROSS-WALK #
########################

# Shape file missing rows
# Drop all of these rows because they will be covered in the next section
merge_dat <- merge_dat[merge_dat$merge != 'Ambagamuwa',]
merge_dat <- merge_dat[merge_dat$merge != 'Galle4Gravets',]
merge_dat <- merge_dat[merge_dat$merge != 'Kothmale',]      

# Mobility data missing rows
# Replace misspellings
merge_dat$admin_level_3_origin[merge_dat$merge == 'Ambagamuwa Korale'] <- 'Ambagamuwa'
merge_dat$admin_level_3_origin[merge_dat$merge == 'Galle 4 Gravets'] <- 'Galle4Gravets'

# Single polygon broken into two in the shape file
merge_dat$admin_level_3_origin[merge_dat$merge == 'Kothmale East'] <- 'Kothmale'
merge_dat$admin_level_3_origin[merge_dat$merge == 'Kothmale West'] <- 'Kothmale'
merge_dat$admin_level_3_origin[merge_dat$merge == 'Nildandahinna'] <- 'Walapane'
merge_dat$admin_level_3_origin[merge_dat$merge == 'Mathurata'] <- 'Hanguranketa'
merge_dat$admin_level_3_origin[merge_dat$merge == 'Thalawakele'] <- 'Nuwara Eliya'
merge_dat$admin_level_3_origin[merge_dat$merge == 'Norwood'] <- 'Ambagamuwa'
merge_dat$admin_level_3_origin[merge_dat$merge == 'Wanduramba'] <- 'Baddegama'
merge_dat$admin_level_3_origin[merge_dat$merge == 'Kalthota'] <- 'Balangoda'

# Single polygon broken into three in the shape file
merge_dat$admin_level_3_origin[merge_dat$merge == 'Rathgama'] <- 'Hikkaduwa'
merge_dat$admin_level_3_origin[merge_dat$merge == 'Madampagama'] <- 'Hikkaduwa'

# Select mobility data and shape file admin 3 variables and rename
mobility_shape_xwalk <- merge_dat %>%
  dplyr::select(c('admin_level_3_origin', 'ADM3_EN')) %>%
  dplyr::rename('admin_level_3_mobile' = 'admin_level_3_origin',
                'admin_level_3_shape' = 'ADM3_EN')

# Save crosswalk
write.csv(mobility_shape_xwalk, file = './tmp/mobility_shape_xwalk.csv')

################################################################################
################################################################################

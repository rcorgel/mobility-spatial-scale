################################################################################
# File Name: 01c_create_admin_xwalk                                            #
#                                                                              #
# Purpose:   Create a crosswalk to match administrative divisions at levels    #
#            1, 2, and 3. For both names and codes.                            #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load mobility data and create crosswalk                        #
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
library(assertr)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project')

##############################################
# 2. LOAD MOBILITY DATA AND CREATE CROSSWALK #
##############################################

# Load processed mobility data
phone_mobility_dat <- readRDS('./tmp/phone_mobility_dat.rds')

# Restrict data to administrative names and codes for levels 1, 2, and 3
admin_xwalk <- phone_mobility_dat |>
  group_by(adm_3_origin, adm_2_origin, adm_1_origin) |>
  distinct(adm_3_origin, adm_2_origin, adm_1_origin, 
           adm_3_origin_code, adm_2_origin_code, adm_1_origin_code, 
           .keep_all = FALSE)

# Rename variables to be more generic
admin_xwalk <- admin_xwalk |>
  dplyr::rename('adm_1' = 'adm_1_origin',
                'adm_1_code' = 'adm_1_origin_code',
                'adm_2' = 'adm_2_origin',
                'adm_2_code' = 'adm_2_origin_code',
                'adm_3' = 'adm_3_origin',
                'adm_3_code' = 'adm_3_origin_code')

# Assert admin numbers match up to reality
verify(admin_xwalk, length(unique(adm_1)) == 9)
verify(admin_xwalk, length(unique(adm_1_code)) == 9)
verify(admin_xwalk, length(unique(adm_2)) == 25)
verify(admin_xwalk, length(unique(adm_2_code)) == 25)
verify(admin_xwalk, length(unique(adm_3)) == 330)
verify(admin_xwalk, length(unique(adm_3_code)) == 330)

# Save crosswalk
saveRDS(admin_xwalk, './tmp/admin_xwalk.rds')

################################################################################
################################################################################

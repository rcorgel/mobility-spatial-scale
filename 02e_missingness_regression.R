################################################################################
# File Name: 02e_missingness_regression                                        #
# Purpose:   Find out which variables are significantly related to missing     # 
#            data.                                                             #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load data and create missing variable indicator                #
#            3. Merge on population and distance information                   #
#            4. Perform logistic regression                                    #
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
library(ggpubr)
library(reshape2)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project')

##################################################
# 2. LOAD DATA AND CREATE MISSING DATA INDICATOR #
##################################################

# Load phone mobility data (1 and 2 have no missing data, so this is not necessary)
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')

# Create missing data indicator
# For mobile phone mobility data
adm_3_phone_mobility_long$missing <- ifelse(is.na(adm_3_phone_mobility_long$value), 1, 0)

###################################################
# 3. MERGE ON POPULATION AND DISTANCE INFORMATION #
###################################################

# Load population data
load('./tmp/adm_population_dat.RData')

# Load distance data
load('./tmp/adm_dist_mat.RData')
adm_3_dist_long <- reshape2::melt(adm_3_dist_mat)

# Merge on population information (origin and destination) for each mobility data source
# Mobile phone mobility data
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_3_population_dat,
                                       by = c('origin' = 'adm_3_mobility'))
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_3_population_dat,
                                       by = c('destination' = 'adm_3_mobility'))

# Merge on distance information for each mobility data source
# Mobile phone mobility data
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_3_dist_long,
                                       by = c('origin' = 'origin', 'destination' = 'destination'))

##################################
# 4. PERFORM LOGISTIC REGRESSION #
##################################

# Univariate regression
# Mobile phone mobility data
# Convert population and distance data so coefficients are larger (but not changing relationship)
adm_3_phone_mobility_long$population_2020_adm_3.x <- adm_3_phone_mobility_long$population_2020_adm_3.x/100000
adm_3_phone_mobility_long$population_2020_adm_3.y <- adm_3_phone_mobility_long$population_2020_adm_3.y/100000
adm_3_phone_mobility_long$value.y <- adm_3_phone_mobility_long$value.y/100
# Regress origin population, destination population, and distance on missingness
model_adm_3_phone_1 <- glm(missing ~ population_2020_adm_3.x, family = binomial(link = "logit"), data = adm_3_phone_mobility_long)
summary(model_adm_3_phone_1)
model_adm_3_phone_2 <- glm(missing ~ population_2020_adm_3.y, family = binomial(link = "logit"), data = adm_3_phone_mobility_long)
summary(model_adm_3_phone_2)
model_adm_3_phone_3 <- glm(missing ~ value.y, family = binomial(link = "logit"), data = adm_3_phone_mobility_long)
summary(model_adm_3_phone_3)

################################################################################
################################################################################

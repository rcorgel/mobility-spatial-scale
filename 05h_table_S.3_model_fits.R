################################################################################
# File Name: 05h_table_S.3_model_fits                                          #
#                                                                              #
# Purpose:   Create figure S.3 for the manuscript.                             #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load model fits and display                                    #
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
library(ggridges)
library(scales)
library(reshape2)
library(ggpubr)
library(mobility)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

##################################
# 2. LOAD MODEL FITS AND DISPLAY #
##################################

# Load data
load('./mobility-spatial-scale/simulated data/mobility_model_fits.RData')

# Display model fits for the table
summary(model_1)
summary(model_2)
summary(model_3)

################################################################################
################################################################################

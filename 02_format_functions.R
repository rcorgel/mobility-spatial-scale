################################################################################
# File Name: 02_format_functions                                               #
# Purpose:   Create functions for formatting the mobility data.                #                                                  
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Create formatting functions                                       #
#                                                                              #
# Project: Sri Lanka Spatial Aggregation                                       #
# Authors: Ronan Corgel                                                        #
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
library(lubridate)
library(assertr)
library(writexl)
library(readxl)

# Set the seed
set.seed(12345)

# Set directory 
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

##################################
# 2. CREATE FORMATTING FUNCTIONS #
##################################

format_mobility_data <- function(data, method = c('name', 'code'), 
                                 output = c('matrix', 'long'), na_replace) {
  # Confirm method set correctly
  method <- match.arg(method)
  
  if (method == 'name') {
    # Reshape to wide
    adm_day_avg_mat <- reshape::cast(data, adm_origin ~ adm_destination)    
    # Label rows with adm names
    rownames(adm_day_avg_mat) <- adm_day_avg_mat$adm_origin   
  }
  if (method == 'code') {
    # Reshape to wide
    adm_day_avg_mat <- reshape::cast(data, adm_origin_code ~ adm_destination_code)            
    # Label rows with adm names
    rownames(adm_day_avg_mat) <- adm_day_avg_mat$adm_origin_code
  }
                        
  # Get rid of the first column
  adm_day_avg_mat <- adm_day_avg_mat[, -1]
  
  # Convert to matrix
  adm_day_avg_mat <- as.data.frame(adm_day_avg_mat)
  adm_day_avg_mat <- as.matrix(adm_day_avg_mat)
  names(dimnames(adm_day_avg_mat)) <- c('origin', 'destination')
  
  # Replace NAs with 0
  if (na_replace == TRUE) {
    adm_day_avg_mat[is.na(adm_day_avg_mat)] <- 0
  }
  
  # Convert to %
  adm_day_avg_mat <- adm_day_avg_mat / rowSums(adm_day_avg_mat, na.rm = TRUE)
 
  # Reshape to long
  if (method == 'name') {
    adm_day_avg_mat_long <- reshape2::melt(adm_day_avg_mat, 
                                           id.vars = 'origin_area', 
                                           variable.name = 'destination_area')            
  }
  if (method == 'code') {
    adm_day_avg_mat_long <- reshape2::melt(adm_day_avg_mat, 
                                           id.vars = 'origin_area_code', 
                                           variable.name = 'destination_area_code')            
  }
  
  # Convert numbers to character
  adm_day_avg_mat_long$origin <- as.character(adm_day_avg_mat_long$origin)
  adm_day_avg_mat_long$destination <- as.character(adm_day_avg_mat_long$destination)
  
  # Return matrix or long data
  output <- match.arg(output)
  if (output == 'matrix') {
    return(adm_day_avg_mat)
  }
  if (output == 'long') {
    return(adm_day_avg_mat_long)
  }
}

################################################################################
################################################################################

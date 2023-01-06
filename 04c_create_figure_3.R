################################################################################
# File Name: 03c_create_figure_3                                               #
#                                                                              #
# Purpose:   Create figure 3.                                                  #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure 3 functions                                      #
#            3. Create figure data                                             #
#            4. Create figure 3                                                #
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

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

################################
# 2. CREATE FIGURE 3 FUNCTIONS #
################################

# Plot functions
# Format data
format_data <- function(data) {
  # Reshape to wide
  adm_day_avg_mat <- reshape::cast(data, adm_origin ~ adm_destination)            
  # Label rows with adm names
  rownames(adm_day_avg_mat) <- adm_day_avg_mat$adm_origin                         
  # Get rid of the first column
  adm_day_avg_mat <- adm_day_avg_mat[, -1]
  # Convert to matrix
  adm_day_avg_mat <- as.data.frame(adm_day_avg_mat)
  adm_day_avg_mat <- as.matrix(adm_day_avg_mat)
  names(dimnames(adm_day_avg_mat)) <- c('origin', 'destination')
  # Replace NAs with 0
  adm_day_avg_mat[is.na(adm_day_avg_mat)] <- 0
  # Convert to %
  adm_day_avg_mat <- adm_day_avg_mat / rowSums(adm_day_avg_mat)
  # Reshape to long
  adm_day_avg_mat_long <- reshape2::melt(adm_day_avg_mat, id.vars = 'origin_area', variable.name = 'destination_area')            
  # Convert numbers to character
  adm_day_avg_mat_long$origin <- as.character(adm_day_avg_mat_long$origin)
  adm_day_avg_mat_long$destination <- as.character(adm_day_avg_mat_long$destination)
  return(adm_day_avg_mat_long)
}

#########################
# 3. CREATE FIGURE DATA #
#########################

# Load mobility data
load('./tmp/mobility_dat.RData')

# Administrative Unit 3
# Collapse data to get average daily trips between origin and destination
adm_3_day_avg <- mobility_dat %>%                                
  group_by(adm_3_origin, adm_3_destination) %>%
  mutate(adm_daily_avg = mean(trips_adj, na.rm = TRUE)) %>%
  distinct(adm_3_origin, adm_3_destination, adm_daily_avg, .keep_all = FALSE) %>%
  dplyr::rename('adm_origin' = 'adm_3_origin',
                'adm_destination' = 'adm_3_destination')
# Format data
adm_3_dat <- format_data(data = adm_3_day_avg)
# Filter data
adm_3_dat <- adm_3_dat %>% 
  dplyr::filter(origin == destination) %>%
  dplyr::mutate(level = 'Level 3')

# Administrative Unit 2
# Collapse data to get average daily trips between origin and destination
adm_2_day_avg <- mobility_dat %>%                                
  group_by(adm_2_origin, adm_2_destination) %>%
  mutate(adm_daily_avg = mean(trips_adj, na.rm = TRUE)) %>%
  distinct(adm_2_origin, adm_2_destination, adm_daily_avg, .keep_all = FALSE) %>%
  dplyr::rename('adm_origin' = 'adm_2_origin',
                'adm_destination' = 'adm_2_destination')
# Format data
adm_2_dat <- format_data(data = adm_2_day_avg)
# Filter data
adm_2_dat <- adm_2_dat %>% 
  dplyr::filter(origin == destination) %>%
  dplyr::mutate(level = 'Level 2')

# Collapse data to get average daily trips between origin and destination
adm_1_day_avg <- mobility_dat %>%                                
  group_by(adm_1_origin, adm_1_destination) %>%
  mutate(adm_daily_avg = mean(trips_adj, na.rm = TRUE)) %>%
  distinct(adm_1_origin, adm_1_destination, adm_daily_avg, .keep_all = FALSE) %>%
  dplyr::rename('adm_origin' = 'adm_1_origin',
                'adm_destination' = 'adm_1_destination')
# Format data
adm_1_dat <- format_data(data = adm_1_day_avg)
# Filter data
adm_1_dat <- adm_1_dat %>% 
  dplyr::filter(origin == destination) %>%
  dplyr::mutate(level = 'Level 1')

# Append data together
adm_dat <- rbind(adm_1_dat, adm_2_dat, adm_3_dat)

######################
# 3. CREATE FIGURE 3 #
######################

figure_3 <- ggplot(adm_dat, aes(x = level, y = value, fill = level)) +
  geom_violin(trim = FALSE, color = 'black')+
  geom_boxplot(width=0.1) + theme_minimal() + 
  scale_fill_manual(values=c('#9E9AC8', '#74C476', '#6BAED6')) +
  theme(legend.position = 'none') +
  xlab('Administrative Division Level') + 
  ylab('Proportion') +
  ggtitle('Proportion of Trips within Unit Distribution by Level')

# Save figure 1
ggsave('./figs/figure_3.png', plot = figure_3 , height = 9, width = 10)

################################################################################
################################################################################


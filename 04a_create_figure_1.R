################################################################################
# File Name: 03a_create_figure_1                                               #
#                                                                              #
# Purpose:   Create figure 1.                                                  #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure 1 functions                                      #
#            3. Create sub figures                                             #
#            4. Combine sub figures to create figure 1                         #
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
# 2. CREATE FIGURE 1 FUNCTIONS #
################################

# Plot functions
# Make map
simple_map <- function(data, color) {
  map <- ggplot(data = data) +
    geom_sf(aes(), fill = color, color= 'black', size = 2) +
    theme_void() + ggtitle(' ')
    coord_sf()
  return(map)
}

# Make matrix plot
matrix_plot <- function(data, color) {
  # Reshape to wide
  adm_day_avg_mat <- reshape::cast(data, adm_origin_code ~ adm_destination_code)            
  # Label rows with adm names
  rownames(adm_day_avg_mat) <- adm_day_avg_mat$adm_origin_code                         
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
  adm_day_avg_mat_long <- reshape2::melt(adm_day_avg_mat, id.vars = 'origin_area_code', variable.name = 'destination_area_code')            
  # Make category
  adm_day_avg_mat_long$value_cat <- cut(adm_day_avg_mat_long$value,
                                        breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                        labels = c("< 0.001", "0.001-0.01", "0.01-0.1", "0.1-1.0"))
  # Convert numbers to character
  adm_day_avg_mat_long$origin <- as.character(adm_day_avg_mat_long$origin)
  adm_day_avg_mat_long$destination <- as.character(adm_day_avg_mat_long$destination)
  
  # Plot
  plot <- ggplot(adm_day_avg_mat_long, aes(x = destination, y = origin)) +
    geom_tile(aes(fill = value_cat)) +
    scale_fill_manual(values = brewer.pal(n = nlevels(adm_day_avg_mat_long$value_cat), name = color)) +
    xlab("Destination") +
    ylab("Origin") +
    guides(fill = guide_legend(title = "Trip Proportion")) + 
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          axis.title=element_text(size=16),
          legend.text=element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(size = 16))
  return(plot)
}

#########################
# 3. CREATE SUB FIGURES #
#########################

# Load the shape files
# From: https://data.humdata.org/dataset/cod-ab-mdg
choropleth_1 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm1_slsd_20220816')
choropleth_2 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm2_slsd_20220816')
choropleth_3 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm3_slsd_20220816')

# Load mobility to shape cross walk
# The mobility data combines multiple admin 3 units, changing the total from 339 to 330
mobility_shape_xwalk <- read.csv('./tmp/mobility_shape_xwalk.csv')

# Merge on the cross walk
choropleth_3 <- left_join(choropleth_3, mobility_shape_xwalk, by = c('ADM3_EN' = 'adm_3_shape'))

# Join polygons to create 330 mobility admin 3 units
choropleth_3_mobility <- choropleth_3 %>% 
  group_by(adm_3_mobility) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# Create maps for figure 1
# Administrative level 1
adm_1_map <- simple_map(data = choropleth_1, color = '#9E9AC8')

# Administrative level 2
adm_2_map <- simple_map(data = choropleth_2, color = '#74C476')

# Administrative level 3
adm_3_map <- simple_map(data = choropleth_3_mobility, color = '#6BAED6')

# Create matrix plots for figure 1
# Load mobility data
load('./tmp/mobility_dat.RData')

# Administrative Unit 3
# Collapse data to get average daily trips between origin and destination
adm_3_day_avg <- mobility_dat %>%                                
  group_by(adm_3_origin_code, adm_3_destination_code) %>%
  mutate(adm_daily_avg = mean(trips_adj, na.rm = TRUE)) %>%
  distinct(adm_3_origin_code, adm_3_destination_code, adm_daily_avg, .keep_all = FALSE) %>%
  dplyr::rename('adm_origin_code' = 'adm_3_origin_code',
                'adm_destination_code' = 'adm_3_destination_code')
# Create plot
adm_3_plot <- matrix_plot(data = adm_3_day_avg, color = 'Blues')

# Administrative Unit 2
# Collapse data to get average daily trips between origin and destination
adm_2_day_avg <- mobility_dat %>%                                
  group_by(adm_2_origin_code, adm_2_destination_code) %>%
  mutate(adm_daily_avg = mean(trips_adj, na.rm = TRUE)) %>%
  distinct(adm_2_origin_code, adm_2_destination_code, adm_daily_avg, .keep_all = FALSE) %>%
  dplyr::rename('adm_origin_code' = 'adm_2_origin_code',
                'adm_destination_code' = 'adm_2_destination_code')
# Create plot
adm_2_plot <- matrix_plot(data = adm_2_day_avg, color = 'Greens')

# Administrative Unit 1
# Collapse data to get average daily trips between origin and destination
adm_1_day_avg <- mobility_dat %>%                                
  group_by(adm_1_origin, adm_1_destination) %>%
  mutate(adm_daily_avg = mean(trips_adj, na.rm = TRUE)) %>%
  distinct(adm_1_origin, adm_1_destination, adm_daily_avg, .keep_all = FALSE) %>%
  ungroup()

# Merge on admin 1 codes (mobility data does not have admin 1 codes in it)
# Extract codes from shape file
adm_1_codes <- as.data.frame(choropleth_1) %>% 
  dplyr::mutate(adm_1_code = str_sub(ADM1_PCODE, -1, -1)) %>%
  dplyr::select(c('ADM1_EN', 'adm_1_code'))
# Merge codes
adm_1_day_avg <- left_join(adm_1_day_avg, adm_1_codes, by = c('adm_1_origin' = 'ADM1_EN'))
adm_1_day_avg <- left_join(adm_1_day_avg, adm_1_codes, by = c('adm_1_destination' = 'ADM1_EN'))
# Clean up names and select variables
adm_1_day_avg <- adm_1_day_avg %>%
  dplyr::select(c('adm_1_code.x', 'adm_1_code.y', 'adm_daily_avg')) %>%
  dplyr::rename('adm_origin_code' = 'adm_1_code.x', 
                'adm_destination_code' = 'adm_1_code.y')
# Create plot
adm_1_plot <- matrix_plot(data = adm_1_day_avg, color = 'Purples')

#############################################
# 4. COMBINE SUB FIGURES TO CREATE FIGURE 1 #
#############################################

# Create figure 1, with labels
figure_1 <- plot_grid(adm_1_map, adm_2_map, adm_3_map,
                      adm_1_plot, adm_2_plot, adm_3_plot, 
                      nrow = 2,
                      labels = c('A. Administrative Divisions, Level 1', 
                                 'B. Administrative Divisions, Level 2', 
                                 'C. Administrative Divisions, Level 3', '', '', ''),
                      label_size = 18, hjust = 0)
# Save figure 1
ggsave('./figs/figure_1.png', plot = figure_1 , height = 13, width = 20)

################################################################################
################################################################################


load('./tmp/mobility_dat.RData')



source('./spatial-resolution/02b_metapop_model.R')

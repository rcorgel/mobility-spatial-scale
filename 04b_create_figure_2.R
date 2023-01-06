################################################################################
# File Name: 03b_create_figure_2                                               #
#                                                                              #
# Purpose:   Create figure 2.                                                  #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure 2 functions                                      #
#            3. Create sub figures                                             #
#            4. Combine sub figures to create figure 2                         #
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
# 2. CREATE FIGURE 2 FUNCTIONS #
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
# Restrict to Colombo
adm_3_dat <- adm_3_dat %>% filter(origin == 'Colombo')
# Merge to map
choropleth_3_mobility <- left_join(choropleth_3_mobility, adm_3_dat, by = c('adm_3_mobility' = 'destination'))

# Make map
# Figure out general coordinates for insert map
st_bbox(choropleth_2[2, ])
# Figure out shades of blue to use
brewer.pal(5, 'Blues')
# Create main map
main_adm_3_map <- ggplot(data = choropleth_3_mobility) +
  geom_sf(aes(fill = value), color= 'black', size = 2) +
  theme_void() + ggtitle(' ') +
  scale_fill_gradient(low = '#EFF3FF', high = '#08519C', name = 'Trip Proportion') +
  coord_sf() + theme(legend.position = 'bottom') + 
  geom_rect(
    xmin = 79.8,
    ymin = 6.8,
    xmax = 80.2,
    ymax = 7.2,
    fill = NA, 
    colour = 'black',
    linewidth = 0.5
  )
# Add insert to main map
adm_3_map <- ggdraw(main_adm_3_map) +
  draw_plot({
    main_adm_3_map + 
        coord_sf(
          xlim = c(79.8, 80.2),
          ylim = c(6.8, 7.2),
          expand = FALSE) +
        theme(legend.position = "none")},
    # Set box position
    x = 0.65, 
    y = 0.65,
    # Set box size
    width = 0.3, 
    height = 0.3)

# Make bar chart 
adm_3_bar <- ggplot(data = adm_3_dat, aes(x = reorder(destination, -value), y = value)) +
  geom_bar(stat = 'identity', fill = '#6BAED6') +
  theme_minimal() + xlab('Destination') + ylab('Trip Proportion') + ylim(0, 1) +
  ggtitle(' ') +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_text(size=16),
        strip.background = element_blank(),
        strip.text = element_text(size = 16))

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
# Restrict to Colombo
adm_2_dat <- adm_2_dat %>% filter(origin == 'Colombo')
# Merge to map
choropleth_2 <- left_join(choropleth_2, adm_2_dat, by = c('ADM2_EN' = 'destination'))

# Make map
# Figure out shades of blue to use
brewer.pal(5, 'Greens')
# Create main map
adm_2_map <- ggplot(data = choropleth_2) +
  geom_sf(aes(fill = value), color= 'black', size = 2) +
  theme_void() + ggtitle(' ') +
  scale_fill_gradient(low = '#EDF8E9', high = '#006D2C', name = 'Trip Proportion') +
  coord_sf() + theme(legend.position = 'bottom') 

# Make bar chart 
adm_2_bar <- ggplot(data = adm_2_dat, aes(x = reorder(destination, -value), y = value)) +
  geom_bar(stat = 'identity', fill = '#74C476') +
  theme_minimal() + xlab('Destination') + ylab('Trip Proportion') + ylim(0, 1) +
  ggtitle(' ') +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_text(size=16),
        strip.background = element_blank(),
        strip.text = element_text(size = 16))

# Administrative Unit 1
# Collapse data to get average daily trips between origin and destination
adm_1_day_avg <- mobility_dat %>%                                
  group_by(adm_1_origin, adm_1_destination) %>%
  mutate(adm_daily_avg = mean(trips_adj, na.rm = TRUE)) %>%
  distinct(adm_1_origin, adm_1_destination, adm_daily_avg, .keep_all = FALSE) %>%
  dplyr::rename('adm_origin' = 'adm_1_origin',
                'adm_destination' = 'adm_1_destination')
# Format data
adm_1_dat <- format_data(data = adm_1_day_avg)
# Restrict to Colombo
adm_1_dat <- adm_1_dat %>% filter(origin == 'Western')
# Merge to map
choropleth_1 <- left_join(choropleth_1, adm_1_dat, by = c('ADM1_EN' = 'destination'))

# Make map
# Figure out shades of blue to use
brewer.pal(5, 'Purples')
# Create main map
adm_1_map <- ggplot(data = choropleth_1) +
  geom_sf(aes(fill = value), color= 'black', size = 2) +
  theme_void() + ggtitle(' ') +
  scale_fill_gradient(low = '#F2F0F7', high = '#54278F', name = 'Trip Proportion') +
  coord_sf() + theme(legend.position = 'bottom') 

# Make bar chart 
adm_1_bar <- ggplot(data = adm_1_dat, aes(x = reorder(destination, -value), y = value)) +
  geom_bar(stat = 'identity', fill = '#9E9AC8') +
  theme_minimal() + xlab('Destination') + ylab('Trip Proportion') + ylim(0, 1) +
  ggtitle(' ') +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_text(size=16),
        strip.background = element_blank(),
        strip.text = element_text(size = 16))

#############################################
# 4. COMBINE SUB FIGURES TO CREATE FIGURE 2 #
#############################################

# Create figure 1, with labels
figure_2 <- plot_grid(adm_1_bar, adm_2_bar, adm_3_bar,
                      adm_1_map, adm_2_map, adm_3_map, 
                      nrow = 2,
                      labels = c('A. Western Province Trip Proportions, Level 1', 
                                 'B. Colombo District Trip Proportions, Level 2', 
                                 'C. Colombo Division Trip Proportions, Level 3', 
                                 '', '', ''),
                      label_size = 18, hjust = 0)
# Save figure 1
ggsave('./figs/figure_2.png', plot = figure_2 , height = 13, width = 20)

################################################################################
################################################################################
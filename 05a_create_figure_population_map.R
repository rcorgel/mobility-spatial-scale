################################################################################
# File Name: 05a_create_figure_population_map                                  #
#                                                                              #
# Purpose:   Create a map and histogram for population distribution by         #
#            administrative level.                                             #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure functions                                        #
#            3. Create sub figures                                             #
#            4. Combine sub figures to create figure                           #
#            5. Text callouts                                                  #
#            6. Create poster figure                                           #
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
library(scales)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

##############################
# 2. CREATE FIGURE FUNCTIONS #
##############################

# Plot functions
# Make a simple admin level map
make_simple_map <- function(data, color, coord_data) {
  map <- ggplot(data = data) +
    geom_sf(aes(fill = population), color= 'black', linewidth = 0.20) +
    scale_fill_distiller(palette = color, direction = 1) +
    geom_point(data = coord_data, aes(x = long, y = lat), colour="white", fill = 'black', size=5, alpha = 0.75, shape=21) +
    geom_label(data = coord_data, aes(x = long - 0.14 , y = lat + 0.14 , label = city), fill = 'white', size = 7, fontface = 'bold') +
    theme_void() + ggtitle(' ') + theme(legend.position = 'none',
                                        plot.title = element_text(size=30, hjust = 0.5))
    coord_sf()
  return(map)
}

#########################
# 3. CREATE SUB FIGURES #
#########################

coordinate_cities <- data.frame(
  city = c("Colombo", "Madhu"),
  lat = c(6.927632561772342, 8.85653415340985),
  long = c(79.85843709788357, 80.20433649099449))   

# Load the shape files
# From: https://data.humdata.org/dataset/cod-ab-mdg
choropleth_1 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm1_slsd_20220816')
choropleth_2 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm2_slsd_20220816')
choropleth_3 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm3_slsd_20220816')

# Load population data
load('./tmp/adm_population_dat.RData')

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
# Sample colors
brewer.pal(9, "BuGn")
brewer.pal(9, "Blues")
brewer.pal(9, "Oranges")
brewer.pal(9, "Greys")
brewer.pal(9, "Purples")

# Administrative level 1
choropleth_1 <- left_join(choropleth_1, adm_1_population_dat, by = c('ADM1_EN' = 'adm_1'))
choropleth_1 <- choropleth_1 %>% dplyr::rename('population' = 'population_2020_adm_1')
adm_1_map <- make_simple_map(data = choropleth_1, color = "Purples", coord_data = coordinate_cities)

# Administrative level 2
choropleth_2 <- left_join(choropleth_2, adm_2_population_dat, by = c('ADM2_EN' = 'adm_2'))
choropleth_2 <- choropleth_2 %>% dplyr::rename('population' = 'population_2020_adm_2')
adm_2_map <- make_simple_map(data = choropleth_2, color = "BuGn", coord_data = coordinate_cities)

# Administrative level 3
choropleth_3_mobility <- left_join(choropleth_3_mobility, adm_3_population_dat, 
                                   by = c('adm_3_mobility' = 'adm_3_mobility'))
choropleth_3_mobility <- choropleth_3_mobility %>% dplyr::rename('population' = 'population_2020_adm_3')
adm_3_map <- make_simple_map(data = choropleth_3_mobility, color = "Blues", coord_data = coordinate_cities)

# Create population plots for figure 1
# Administrative level 1
adm_1_plot <- ggplot(adm_1_population_dat, aes(x=population_2020_adm_1)) +
  geom_histogram(bins = 15, aes(), alpha=0.75, fill = '#807DBA', color = '#807DBA',
                 position="identity") + 
  theme_minimal() + xlab('Population') + ylab('Count') +
  scale_x_continuous(limits = c(0, 6500000), labels = label_comma()) +
  ggtitle(' ') +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# Administrative level 2
adm_2_plot <- ggplot(adm_2_population_dat, aes(x=population_2020_adm_2)) +
  geom_histogram(bins = 15, aes(), alpha=0.75, fill = '#41AE76', color = '#41AE76',
                 position="identity") + 
  theme_minimal() + xlab('Population') + ylab('Count') +
  scale_x_continuous(limits = c(-100000, 2700000), labels = label_comma()) +
  ggtitle(' ') +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# Administrative level 3
adm_3_plot <- ggplot(adm_3_population_dat, aes(x=population_2020_adm_3)) +
  geom_histogram(bins = 15, aes(), alpha=0.75, fill = '#4292C6', color = '#4292C6',
                 position="identity") + 
  theme_minimal() + xlab('Population') + ylab('Count') +
  scale_x_continuous(labels = label_comma()) + 
  ggtitle(' ') +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

###########################################
# 4. COMBINE SUB FIGURES TO CREATE FIGURE #
###########################################

# Create figure 1, with labels
figure <- plot_grid(adm_1_map + ggtitle('Administrative Level 1'), 
                    adm_2_map + ggtitle('Administrative Level 2'), 
                    adm_3_map + ggtitle('Administrative Level 3'), 
                    adm_1_plot, adm_2_plot, adm_3_plot, 
                    nrow = 2,
                    rel_heights = c(1.5, 1),
                    labels = c('(a)', '(b)', '(c)', '', '', ''),
                    label_size = 26, hjust = 0)

# Save figure 1
ggsave('./figs/figure_population_map.jpg', plot = figure, height = 18, width = 25)

####################
# 5. TEXT CALLOUTS #
####################

# Number of adm 3 above 250,000
nrow(adm_3_population_dat[adm_3_population_dat$population_2020_adm_3 > 250000, 4])
# Percent of adm 3 below 100,000
nrow(adm_3_population_dat[adm_3_population_dat$population_2020_adm_3 < 100000, 4]) / nrow(adm_3_population_dat)

# Population summaries
mean(adm_3_population_dat$population_2020_adm_3)
mean(adm_2_population_dat$population_2020_adm_2)
mean(adm_1_population_dat$population_2020_adm_1)

min(adm_3_population_dat$population_2020_adm_3)
min(adm_2_population_dat$population_2020_adm_2)
min(adm_1_population_dat$population_2020_adm_1)

max(adm_3_population_dat$population_2020_adm_3)
max(adm_2_population_dat$population_2020_adm_2)
max(adm_1_population_dat$population_2020_adm_1)

###########################
# 6. CREATE POSTER FIGURE #
###########################

# Re-write map function
make_simple_map <- function(data, color, coord_data, breaks, labs) {
  map <- ggplot(data = data) +
    geom_sf(aes(fill = population), color= 'black', linewidth = 0.20) +
    scale_fill_distiller(palette = color, direction = 1, 
                         breaks = breaks,
                         labels = labs) +
    geom_point(data = coord_data, aes(x = long, y = lat), colour="white", fill = 'black', size=5, alpha = 0.75, shape=21) +
    geom_label(data = coord_data, aes(x = long , y = lat + 0.16 , label = city), fill = 'white', size = 12, fontface = 'bold') +
    theme_void() + ggtitle(' ') + theme(legend.position = 'bottom',
                                        legend.title=element_blank(),
                                        plot.title = element_text(size=40, hjust = 0.5)) +
    guides(fill=guide_colorbar(label.theme=element_text(size=34),
                               barwidth=25,
                               barheight=1.5,
                               frame.colour='black',
                               ticks = FALSE)) +
    coord_sf()
  return(map)
}

# Re-create maps
adm_1_map <- make_simple_map(data = choropleth_1, color = "Purples", coord_data = coordinate_cities,
                             breaks = c(932855, 3200000, 6000000), labs = c('100,000', '3,000,000', '6,000,000'))
adm_2_map <- make_simple_map(data = choropleth_2, color = "BuGn", coord_data = coordinate_cities,
                             breaks = c(40092, 1250000, 2400029), labs = c('40,000', '1,250,000', '2,500,000'))
adm_3_map <- make_simple_map(data = choropleth_3_mobility, color = "Blues", coord_data = coordinate_cities,
                             breaks = c(675, 150000, 303422), labs = c('500', '150,000', '300,000'))

# Assemble the figure
figure_poster <- plot_grid(adm_1_map + ggtitle('Population, Admin Level 1') + 
                             theme(plot.title = element_text(size=54, hjust = 0.5)), 
                           adm_2_map + ggtitle('Population, Admin Level 2') + 
                             theme(plot.title = element_text(size=54, hjust = 0.5)),  
                           adm_3_map + ggtitle('Population, Admin Level 3') + 
                             theme(plot.title = element_text(size=54, hjust = 0.5)), 
                           nrow = 1,
                           rel_heights = c(1))

# Save the figure
ggsave('./figs/figure_population_map_poster.jpg', plot = figure_poster, height = 12, width = 30)

################################################################################
################################################################################

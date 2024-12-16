################################################################################
# File Name: figure_1                                                          #
#                                                                              #
# Purpose:   Create figure 1 for the paper.                                    #
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
library(reshape2)
library(ggpubr)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

##############################
# 2. CREATE FIGURE FUNCTIONS #
##############################

# Plot functions
# Make a simple admin level map
make_simple_map <- function(data, color, coord_data) {
  map <- ggplot(data = data) +
    geom_sf(aes(fill = population), color= 'black', linewidth = 0.20) +
    scale_fill_distiller(palette = color, direction = 1, name = ' ', labels = comma) +
    geom_point(data = coord_data, aes(x = long, y = lat), colour="white", fill = 'black', size=5, alpha = 0.75, shape=21) +
    geom_label(data = coord_data, aes(x = long - 0.08 , y = lat + 0.14 , label = city), fill = 'white', size = 8, fontface = 'bold', alpha = 0.85) +
    theme_void() + ggtitle(' ') + theme(legend.position = c(0.80, 0.90),
                                        plot.title = element_text(size = 30, hjust = 0.5),
                                        legend.text = element_text(size = 22)) +
  coord_sf()
  return(map)
}

# Set theme
matrix_theme <- theme(panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.background = element_blank(),
                      panel.border = element_blank(),
                      axis.line = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      axis.title = element_text(size=40),
                      legend.text = element_text(size = 34),
                      legend.title = element_text(size = 12),
                      legend.position = "bottom",
                      legend.box="vertical",
                      legend.margin=margin(),
                      strip.background = element_blank(),
                      legend.spacing.y = unit(0.25, 'cm'),
                      legend.key.size = unit(1, 'cm'),
                      strip.text = element_text(size = 16),
                      plot.title = element_text(size=26, hjust = 0.5))

scatter_theme <- theme(legend.text = element_text(size = 24),
                       legend.title = element_text(size = 20),
                       axis.text = element_text(size=20),
                       axis.title = element_text(size=26),
                       legend.position = "bottom",
                       legend.box="vertical",
                       legend.margin=margin(),
                       strip.background = element_blank(),
                       legend.spacing.y = unit(0.25, 'cm'),
                       legend.key.size = unit(1, 'cm'),
                       strip.text = element_text(size = 16),
                       plot.title = element_text(size=26, hjust = 0.5))

# Make a mobility matrix colored by trip proportion for data with NAs
make_matrix_plot_na <- function(data, color) {
  plot <- ggplot(data, aes(x = as.character(destination), y = as.character(origin), fill = value_cat)) +
    geom_tile(height = 1, width = 1) +
    scale_fill_manual(values = c(brewer.pal(n = 4, name = color), '#FFFFFF'),
                      breaks = c("1", "2", 
                                 "3", "4", "NA"),
                      labels = c("< 0.001", "0.001-0.01", 
                                 "0.01-0.1", "0.1-1.0", "NA")) +
    xlab("Destination") +
    theme_bw() + 
    ylab("Origin") +
    ggtitle(' ') + 
    guides(fill = guide_legend(title = "", byrow = TRUE)) + 
    matrix_theme
  return(plot)
}

# Make a mobility matrix colored by trip proportion for data with no NAs
make_matrix_plot <- function(data, color) {
  plot <- ggplot(data, aes(x = as.character(destination), y = as.character(origin), fill = value_cat)) +
    geom_tile(height = 1, width = 1) +
    scale_fill_manual(values = brewer.pal(n = nlevels(data$value_cat), name = color),
                      breaks = c("< 0.001", "0.001-0.01", 
                                 "0.01-0.1", "0.1-1.0"),
                      labels = c("< 0.001", "0.001-0.01", 
                                 "0.01-0.1", "0.1-1.0")) +
    xlab("Destination") +
    theme_bw() + 
    ylab("Origin") +
    ggtitle(' \n') + 
    guides(fill = guide_legend(title = "", byrow = TRUE)) + 
    matrix_theme
  return(plot)
}

############################
# 3. CREATE MAP SUBFIGURES #
############################

# Add coordinates of highlight cities
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
adm_1_map <- make_simple_map(data = choropleth_1, color = "Blues", coord_data = coordinate_cities)

# Administrative level 2
choropleth_2 <- left_join(choropleth_2, adm_2_population_dat, by = c('ADM2_EN' = 'adm_2'))
choropleth_2 <- choropleth_2 %>% dplyr::rename('population' = 'population_2020_adm_2')
adm_2_map <- make_simple_map(data = choropleth_2, color = "BuGn", coord_data = coordinate_cities)

# Administrative level 3
choropleth_3_mobility <- left_join(choropleth_3_mobility, adm_3_population_dat, 
                                   by = c('adm_3_mobility' = 'adm_3_mobility'))
choropleth_3_mobility <- choropleth_3_mobility %>% dplyr::rename('population' = 'population_2020_adm_3')
adm_3_map <- make_simple_map(data = choropleth_3_mobility, color = "Purples", coord_data = coordinate_cities)

################################
# 3. CREATE SCATTER SUBFIGURES #
################################

# Load simulated trip count data
load('./mobility-spatial-scale/simulated data/mobile_phone_sim_count_dat.RData')

# Reshape the simulated matrices from wide to long
# Administrative Level 1
adm_1_long_pred <- reshape2::melt(phone_mobility_adm_1_pred, 
                                  id.vars = 'origin_area', 
                                  variable.name = 'destination_area')
adm_1_long_pred <- adm_1_long_pred %>%
  dplyr::rename('adm_1_origin' = 'Var1',
         'adm_1_destination' = 'Var2')

# Administrative Level 2
adm_2_long_pred <- reshape2::melt(phone_mobility_adm_2_pred, 
                                  id.vars = 'origin_area', 
                                  variable.name = 'destination_area')
adm_2_long_pred <- adm_2_long_pred %>%
  dplyr::rename('adm_2_origin' = 'Var1',
         'adm_2_destination' = 'Var2')

# Administrative Level 3
adm_3_long_pred <- reshape2::melt(phone_mobility_adm_3_pred, 
                                  id.vars = 'origin_area', 
                                  variable.name = 'destination_area')
adm_3_long_pred <- adm_3_long_pred %>%
  dplyr::rename('adm_3_origin' = 'Var1',
         'adm_3_destination' = 'Var2')

# Load distance data
load('./tmp/adm_dist_mat.RData')

# Load observes trip count data
load('./tmp/adm_phone_mobility_dat.RData')

# Merge simulated and observed trip count data
# Administrative Level 1
adm_1_long_pred <- left_join(adm_1_long_pred, 
          adm_1_phone_mobility_dat[, c('adm_1_origin', 'adm_1_destination', 'trips_avg')],
          by = c('adm_1_origin' = 'adm_1_origin', 
                 'adm_1_destination' = 'adm_1_destination'))
adm_1_long_pred[adm_1_long_pred$adm_1_destination == 'Western',]

adm_1_long_pred <- left_join(adm_1_long_pred, 
                             adm_1_dist,
                             by = c('adm_1_origin' = 'adm_1_origin', 
                                    'adm_1_destination' = 'adm_1_destination'))
# Merge on origin and destination population data
adm_1_long_pred <- left_join(adm_1_long_pred, 
                             adm_1_population_dat,
                             by = c('adm_1_origin' = 'adm_1'))
adm_1_long_pred <- left_join(adm_1_long_pred, 
                             adm_1_population_dat,
                             by = c('adm_1_destination' = 'adm_1'))
adm_1_long_pred <- adm_1_long_pred %>% 
  dplyr::rename('adm_1_origin_pop' = 'population_2020_adm_1.x',
         'adm_1_destination_pop' = 'population_2020_adm_1.y')
# Correlations (observed)
cor(adm_1_long_pred$trips_avg, adm_1_long_pred$distance)
cor(adm_1_long_pred$trips_avg, adm_1_long_pred$adm_1_origin_pop)
cor(adm_1_long_pred$trips_avg, adm_1_long_pred$adm_1_destination_pop)
# Correlations (simulated)
cor(adm_1_long_pred$value, adm_1_long_pred$distance)
cor(adm_1_long_pred$value, adm_1_long_pred$adm_1_origin_pop)
cor(adm_1_long_pred$value, adm_1_long_pred$adm_1_destination_pop)

sim_comp_1 <- ggplot(data = adm_1_long_pred) + 
  geom_point(aes(x = trips_avg, y = value), size = 10, color = '#4292C6', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  xlim(0, 20000000) + theme(legend.position = 'none',
                            axis.text = element_text(size=20),
                            axis.title = element_text(size=26),
                            plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')
cor(adm_1_long_pred$trips_avg, adm_1_long_pred$value)

# Administrative Level 2
adm_2_long_pred <- left_join(adm_2_long_pred, 
                             adm_2_phone_mobility_dat[, c('adm_2_origin', 'adm_2_destination', 'trips_avg')],
                             by = c('adm_2_origin' = 'adm_2_origin', 
                                    'adm_2_destination' = 'adm_2_destination'))
adm_2_long_pred[adm_2_long_pred$adm_2_destination == 'Colombo',]

adm_2_long_pred <- left_join(adm_2_long_pred, 
                             adm_2_dist,
                             by = c('adm_2_origin' = 'adm_2_origin', 
                                    'adm_2_destination' = 'adm_2_destination'))
# Merge on origin and destination population data
adm_2_long_pred <- left_join(adm_2_long_pred, 
                             adm_2_population_dat,
                             by = c('adm_2_origin' = 'adm_2'))
adm_2_long_pred <- left_join(adm_2_long_pred, 
                             adm_2_population_dat,
                             by = c('adm_2_destination' = 'adm_2'))
adm_2_long_pred <- adm_2_long_pred %>% 
  dplyr::rename('adm_2_origin_pop' = 'population_2020_adm_2.x',
         'adm_2_destination_pop' = 'population_2020_adm_2.y')
# Correlations (observed)
cor(adm_2_long_pred$trips_avg, adm_2_long_pred$distance)
cor(adm_2_long_pred$trips_avg, adm_2_long_pred$adm_2_origin_pop)
cor(adm_2_long_pred$trips_avg, adm_2_long_pred$adm_2_destination_pop)
# Correlations (simulated)
cor(adm_2_long_pred$value, adm_2_long_pred$distance)
cor(adm_2_long_pred$value, adm_2_long_pred$adm_2_origin_pop)
cor(adm_2_long_pred$value, adm_2_long_pred$adm_2_destination_pop)

sim_comp_2 <- ggplot(data = adm_2_long_pred) + 
  geom_point(aes(x = trips_avg, y = value), size = 10, color = '#41AE76', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  xlim(0, 7000000) + theme(legend.position = 'none',
                           axis.text = element_text(size=20),
                           axis.title = element_text(size=26),
                           plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')
cor(adm_2_long_pred$trips_avg, adm_2_long_pred$value)

# Administrative Level 3
adm_3_long_pred <- left_join(adm_3_long_pred, 
                             adm_3_phone_mobility_dat[, c('adm_3_origin', 'adm_3_destination', 'trips_avg')],
                             by = c('adm_3_origin' = 'adm_3_origin', 
                                    'adm_3_destination' = 'adm_3_destination'))
adm_3_long_pred[adm_3_long_pred$adm_3_destination == 'Colombo',]

adm_3_long_pred <- left_join(adm_3_long_pred, 
                             adm_3_dist,
                             by = c('adm_3_origin' = 'adm_3_origin', 
                                    'adm_3_destination' = 'adm_3_destination'))
# Merge on origin and destination population data
adm_3_long_pred <- left_join(adm_3_long_pred, 
                             adm_3_population_dat,
                             by = c('adm_3_origin' = 'adm_3_mobility'))
adm_3_long_pred <- left_join(adm_3_long_pred, 
                             adm_3_population_dat,
                             by = c('adm_3_destination' = 'adm_3_mobility'))
adm_3_long_pred <- adm_3_long_pred %>% 
  dplyr::rename('adm_3_origin_pop' = 'population_2020_adm_3.x',
         'adm_3_destination_pop' = 'population_2020_adm_3.y')
# Correlations (observed)
cor(adm_3_long_pred$trips_avg, adm_3_long_pred$distance, use = 'pairwise.complete.obs')
cor(adm_3_long_pred$trips_avg, adm_3_long_pred$adm_3_origin_pop, use = 'pairwise.complete.obs')
cor(adm_3_long_pred$trips_avg, adm_3_long_pred$adm_3_destination_pop, use = 'pairwise.complete.obs')
# Correlations (simulated)
cor(adm_3_long_pred$value, adm_3_long_pred$distance)
cor(adm_3_long_pred$value, adm_3_long_pred$adm_3_origin_pop)
cor(adm_3_long_pred$value, adm_3_long_pred$adm_3_destination_pop)

sim_comp_3 <- ggplot(data = adm_3_long_pred) + 
  geom_point(aes(x = trips_avg, y = value), size = 10, color = '#807DBA', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  ylim(0, 600000) + xlim(0, 600000) + theme(legend.position = 'none',
                                            axis.text = element_text(size=20),
                                            axis.title = element_text(size=26),
                                            plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')
cor(adm_3_long_pred$trips_avg, adm_3_long_pred$value, use = 'pairwise.complete.obs')

# Plot scatter plots of trip count by trip distance for highlight cities
# Administrative Level 3
sim <- adm_3_long_pred[, c('adm_3_origin', 'adm_3_destination', 'value', 'distance')]
sim$Cat <- 'Simulated'
obs <- adm_3_long_pred[, c('adm_3_origin', 'adm_3_destination', 'trips_avg', 'distance')]
obs$Cat <- 'Observed'
obs <- obs %>% dplyr::rename('value' = 'trips_avg')
all <- rbind(sim, obs)

plot_1 <- ggplot(data = all[all$adm_3_destination == 'Madhu',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.25, size = 4) + theme_classic() +
  xlab('Distance (km)') + 
  scale_y_log10("Trip Counts",
                breaks = c(10^-8, 10^-4, 10^0, 10^4, 10^8),
                labels = trans_format("log10", math_format(10^.x)),
                n.breaks = 12,
                limits = c(10^-8, 10^8)) + ggtitle('\n\nMadhu Division (Rural)\n') +
  scale_color_manual('', values = c('#807DBA', 'grey70')) +
  scatter_theme + theme(plot.title = element_text(hjust = 0.5),
                        legend.position = 'none')
cor(all[all$adm_3_destination == 'Madhu',]$distance, all[all$adm_3_destination == 'Madhu',]$value, use = 'pairwise.complete.obs')

plot_2 <- ggplot(data = all[all$adm_3_destination == 'Colombo',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.25, size = 4) + theme_classic() +
  xlab('Distance (km)') + 
  scale_y_log10("Trip Counts",
                breaks = c(10^-8, 10^-4, 10^0, 10^4, 10^8),
                labels = trans_format("log10", math_format(10^.x)),
                n.breaks = 12,
                limits = c(10^-8, 10^8)) + ggtitle('\n\nColombo Division (Urban)\n') +
  scale_color_manual('', values = c('#807DBA', 'grey70')) +
  scatter_theme + theme(plot.title = element_text(hjust = 0.5),
                        legend.position = 'none')
cor(all[all$adm_3_destination == 'Colombo',]$distance, all[all$adm_3_destination == 'Colombo',]$value)

legend_adm_3 <- ggplot(data = all[all$adm_3_destination == 'Colombo',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.85, size = 6) + theme_classic() +
  scale_color_manual('', values = c('#807DBA', 'grey70')) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom') + scatter_theme

# Administrative Level 2
sim <- adm_2_long_pred[, c('adm_2_origin', 'adm_2_destination', 'value', 'distance')]
sim$Cat <- 'Simulated'
obs <- adm_2_long_pred[, c('adm_2_origin', 'adm_2_destination', 'trips_avg', 'distance')]
obs$Cat <- 'Observed'
obs <- obs %>% dplyr::rename('value' = 'trips_avg')
all <- rbind(sim, obs)

plot_3 <- ggplot(data = all[all$adm_2_destination == 'Mannar',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.65, size = 4) + theme_classic() +
  xlab('Distance (km)') + 
  scale_y_log10("Trip Counts",
                breaks = c(10^-8, 10^-4, 10^0, 10^4, 10^8),
                labels = trans_format("log10", math_format(10^.x)),
                n.breaks = 12,
                limits = c(10^-8, 10^8)) + ggtitle('\n\nMannar District (Rural)\n') +
  scale_color_manual('', values = c('#41AE76', 'grey70')) +
  scatter_theme + theme(plot.title = element_text(hjust = 0.5),
                        legend.position = 'none')

plot_4 <- ggplot(data = all[all$adm_2_destination == 'Colombo',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.65, size = 4) + theme_classic() +
  xlab('Distance (km)') + 
  scale_y_log10("Trip Counts",
                breaks = c(10^-8, 10^-4, 10^0, 10^4, 10^8),
                labels = trans_format("log10", math_format(10^.x)),
                n.breaks = 12,
                limits = c(10^-8, 10^8)) + ggtitle('\n\nColombo District (Urban)\n') +
  scale_color_manual('', values = c('#41AE76', 'grey70')) +
  scatter_theme + theme(plot.title = element_text(hjust = 0.5),
                        legend.position = 'none')
cor(all[all$adm_2_destination == 'Colombo',]$distance, all[all$adm_2_destination == 'Colombo',]$value)

legend_adm_2 <- ggplot(data = all[all$adm_2_destination == 'Colombo',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.85, size = 6) + theme_classic() +
  scale_color_manual('', values = c('#41AE76', 'grey70')) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom') + scatter_theme

# Administrative Level 1
sim <- adm_1_long_pred[, c('adm_1_origin', 'adm_1_destination', 'value', 'distance')]
sim$Cat <- 'Simulated'
obs <- adm_1_long_pred[, c('adm_1_origin', 'adm_1_destination', 'trips_avg', 'distance')]
obs$Cat <- 'Observed'
obs <- obs %>% dplyr::rename('value' = 'trips_avg')
all <- rbind(sim, obs)

plot_5 <- ggplot(data = all[all$adm_1_destination == 'Northern',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.65, size = 4) + theme_classic() +
  xlab('Distance (km)') + 
  scale_y_log10("Trip Counts",
                breaks = c(10^-8, 10^-4, 10^0, 10^4, 10^8),
                labels = trans_format("log10", math_format(10^.x)),
                n.breaks = 12,
                limits = c(10^-8, 10^8)) + ggtitle('\n\nNorthern Province (Rural)\n') +
  scale_color_manual('', values = c('#4292C6', 'grey70')) +
  scatter_theme + theme(plot.title = element_text(hjust = 0.5),
                        legend.position = 'none')

plot_6 <- ggplot(data = all[all$adm_1_destination == 'Western',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.65, size = 4) + theme_classic() +
  xlab('Distance (km)') + 
  scale_y_log10("Trip Counts",
                breaks = c(10^-8, 10^-4, 10^0, 10^4, 10^8),
                labels = trans_format("log10", math_format(10^.x)),
                n.breaks = 12,
                limits = c(10^-8, 10^8)) + ggtitle('\n\nWestern Province (Urban)\n') +
  scale_color_manual('', values = c('#4292C6', 'grey70')) +
  scatter_theme + theme(plot.title = element_text(hjust = 0.5),
                        legend.position = 'none')

legend_adm_1 <- ggplot(data = all[all$adm_1_destination == 'Western',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.85, size = 6) + theme_classic() +
  scale_color_manual('', values = c('#4292C6', 'grey70')) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom') + scatter_theme

# Combine individual scatter plots into pairs for plotting
legend_3 <- get_legend(legend_adm_3)
sub_1 <- plot_grid(plot_2, plot_1, legend_3, 
          nrow = 3, rel_heights = c(1, 1, 0.2))

legend_2 <- get_legend(legend_adm_2)
sub_2 <- plot_grid(plot_4, plot_3, legend_2,
          nrow = 3, rel_heights = c(1, 1, 0.2))

legend_1 <- get_legend(legend_adm_1)
sub_3 <- plot_grid(plot_6, plot_5, legend_1,
          nrow = 3, rel_heights = c(1, 1, 0.2))

###############################
# 3. CREATE MATRIX SUBFIGURES #
###############################

#####################
# MOBILE PHONE DATA #
#####################

# Load phone mobility data
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')
load('./tmp/fmt_adm_2_phone_mobility_dat.RData')
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')

# Administrative Unit 3
# Make category variable
adm_3_phone_mobility_long_code$value_cat <- cut(adm_3_phone_mobility_long_code$value,
                                                breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                                labels = c("< 0.001", "0.001-0.01", 
                                                           "0.01-0.1", "0.1-1.0"))
adm_3_phone_mobility_long_code$value_cat <- ifelse(is.na(adm_3_phone_mobility_long_code$value_cat), "NA", adm_3_phone_mobility_long_code$value_cat)

# Create plot
adm_3_phone_plot <- make_matrix_plot_na(data = adm_3_phone_mobility_long_code, color = 'Purples')

# Administrative Unit 2
# Make category variable
adm_2_phone_mobility_long_code$value_cat <- cut(adm_2_phone_mobility_long_code$value,
                                                breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                                labels = c("< 0.001", "0.001-0.01", 
                                                           "0.01-0.1", "0.1-1.0"))
# Create plot
adm_2_phone_plot <- make_matrix_plot(data = adm_2_phone_mobility_long_code, color = 'BuGn')

# Administrative Unit 1
# Make category variable
adm_1_phone_mobility_long_code$value_cat <- cut(adm_1_phone_mobility_long_code$value,
                                                breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                                labels = c("< 0.001", "0.001-0.01", 
                                                           "0.01-0.1", "0.1-1.0"))
# Create plot
adm_1_phone_plot <- make_matrix_plot(data = adm_1_phone_mobility_long_code, color = 'Blues')

###############################
# SIMULATED MOBILE PHONE DATA #
###############################

# Load simulated mobility data
load('./mobility-spatial-scale/simulated data/mobile_phone_sim_prop_dat.RData')

# Load crosswalk with codes
admin_xwalk <- read.csv('./tmp/admin_xwalk.csv')

# Reshape matrices to long format and merge on codes

# Administrative Unit 3
# Reshape to long
adm_3_phone_mobility_sim_dat <- reshape2::melt(adm_3_phone_pred_mobility_mat, 
                                               id.vars = 'adm_3_origin', 
                                               variable.name = 'adm_3_destination')  
# Rename variables
adm_3_phone_mobility_sim_dat <- adm_3_phone_mobility_sim_dat %>% 
  dplyr::rename('adm_3_origin' = 'Var1',
                'adm_3_destination' = 'Var2')
# Join on codes
adm_3_phone_mobility_sim_dat <- left_join(adm_3_phone_mobility_sim_dat, admin_xwalk[, c(1, 4)],
                                          by = c('adm_3_origin' = 'adm_3'))
adm_3_phone_mobility_sim_dat <- left_join(adm_3_phone_mobility_sim_dat, admin_xwalk[, c(1, 4)],
                                          by = c('adm_3_destination' = 'adm_3'))
# Rename codes
adm_3_phone_mobility_sim_dat <- adm_3_phone_mobility_sim_dat %>%
  dplyr::rename('origin' = 'adm_3_code.x',
                'destination' = 'adm_3_code.y')
# Make category variable
adm_3_phone_mobility_sim_dat$value_cat <- cut(adm_3_phone_mobility_sim_dat$value,
                                              breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                              labels = c("< 0.001", "0.001-0.01", 
                                                         "0.01-0.1", "0.1-1.0"))
# Create plot
adm_3_phone_plot_sim <- make_matrix_plot(data = adm_3_phone_mobility_sim_dat, color = 'Purples')

# Merge and Plot Comparison
adm_3_phone_mobility_long_code$origin <- as.numeric(adm_3_phone_mobility_long_code$origin)
adm_3_phone_mobility_long_code$destination <- as.numeric(adm_3_phone_mobility_long_code$destination)
adm_3_phone_mobility_sim_dat <- left_join(adm_3_phone_mobility_sim_dat, 
                                          adm_3_phone_mobility_long_code,
                                          by = c('origin' = 'origin',
                                                 'destination' = 'destination'))
sim_comp_3_prop <- ggplot(data = adm_3_phone_mobility_sim_dat) + 
  geom_point(aes(x = value.y, y = value.x), size = 10, color = '#807DBA', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  ylim(0, 1) + xlim(0, 1) + theme(legend.position = 'none',
                                  axis.text = element_text(size=20),
                                  axis.title = element_text(size=26),
                                  plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')
cor(adm_3_phone_mobility_sim_dat$value.y, adm_3_phone_mobility_sim_dat$value.x, use = 'pairwise.complete.obs')

# Administrative Unit 2
# Subset x walk to admin 2
admin_xwalk_2 <- admin_xwalk %>% group_by(adm_2) %>%
  distinct(adm_2, adm_2_code)
# Reshape to long
adm_2_phone_mobility_sim_dat <- reshape2::melt(adm_2_phone_pred_mobility_mat, 
                                               id.vars = 'adm_2_origin', 
                                               variable.name = 'adm_2_destination')  
# Rename variables
adm_2_phone_mobility_sim_dat <- adm_2_phone_mobility_sim_dat %>% 
  dplyr::rename('adm_2_origin' = 'Var1',
                'adm_2_destination' = 'Var2')
# Join on codes
adm_2_phone_mobility_sim_dat <- left_join(adm_2_phone_mobility_sim_dat, admin_xwalk_2,
                                          by = c('adm_2_origin' = 'adm_2'))
adm_2_phone_mobility_sim_dat <- left_join(adm_2_phone_mobility_sim_dat, admin_xwalk_2,
                                          by = c('adm_2_destination' = 'adm_2'))
# Rename codes
adm_2_phone_mobility_sim_dat <- adm_2_phone_mobility_sim_dat %>%
  dplyr::rename('origin' = 'adm_2_code.x',
                'destination' = 'adm_2_code.y')
# Make category variable
adm_2_phone_mobility_sim_dat$value_cat <- cut(adm_2_phone_mobility_sim_dat$value,
                                              breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                              labels = c("< 0.001", "0.001-0.01", 
                                                         "0.01-0.1", "0.1-1.0"))
# Create plot
adm_2_phone_plot_sim <- make_matrix_plot(data = adm_2_phone_mobility_sim_dat, color = 'BuGn')

# Merge and Plot Comparison
adm_2_phone_mobility_long_code$origin <- as.numeric(adm_2_phone_mobility_long_code$origin)
adm_2_phone_mobility_long_code$destination <- as.numeric(adm_2_phone_mobility_long_code$destination)
adm_2_phone_mobility_sim_dat <- left_join(adm_2_phone_mobility_sim_dat, 
                                          adm_2_phone_mobility_long_code,
                                          by = c('origin' = 'origin',
                                                 'destination' = 'destination'))
sim_comp_2_prop <- ggplot(data = adm_2_phone_mobility_sim_dat) + 
  geom_point(aes(x = value.y, y = value.x), size = 10, color = '#41AE76', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  ylim(0, 1) + xlim(0, 1) + theme(legend.position = 'none',
                                  axis.text = element_text(size=20),
                                  axis.title = element_text(size=26),
                                  plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')
cor(adm_2_phone_mobility_sim_dat$value.y, adm_2_phone_mobility_sim_dat$value.x, use = 'pairwise.complete.obs')

# Administrative Unit 1
# Subset x walk to admin 1
admin_xwalk_1 <- admin_xwalk %>% group_by(adm_1) %>%
  distinct(adm_1, adm_1_code)
# Reshape to long
adm_1_phone_mobility_sim_dat <- reshape2::melt(adm_1_phone_pred_mobility_mat, 
                                               id.vars = 'adm_1_origin', 
                                               variable.name = 'adm_1_destination')   
# Rename variables
adm_1_phone_mobility_sim_dat <- adm_1_phone_mobility_sim_dat %>% 
  dplyr::rename('adm_1_origin' = 'Var1',
                'adm_1_destination' = 'Var2')
# Join on codes
adm_1_phone_mobility_sim_dat <- left_join(adm_1_phone_mobility_sim_dat, admin_xwalk_1,
                                          by = c('adm_1_origin' = 'adm_1'))
adm_1_phone_mobility_sim_dat <- left_join(adm_1_phone_mobility_sim_dat, admin_xwalk_1,
                                          by = c('adm_1_destination' = 'adm_1'))
# Rename codes
adm_1_phone_mobility_sim_dat <- adm_1_phone_mobility_sim_dat %>%
  dplyr::rename('origin' = 'adm_1_code.x',
                'destination' = 'adm_1_code.y')
# Make category variable
adm_1_phone_mobility_sim_dat$value_cat <- cut(adm_1_phone_mobility_sim_dat$value,
                                              breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                              labels = c("< 0.001", "0.001-0.01", 
                                                         "0.01-0.1", "0.1-1.0"))
# Create plot
adm_1_phone_plot_sim <- make_matrix_plot(data = adm_1_phone_mobility_sim_dat, color = 'Blues')

# Merge and Plot Comparison
adm_1_phone_mobility_long_code$origin <- as.numeric(adm_1_phone_mobility_long_code$origin)
adm_1_phone_mobility_long_code$destination <- as.numeric(adm_1_phone_mobility_long_code$destination)
adm_1_phone_mobility_sim_dat <- left_join(adm_1_phone_mobility_sim_dat, 
                                          adm_1_phone_mobility_long_code,
                                          by = c('origin' = 'origin',
                                                 'destination' = 'destination'))
sim_comp_1_prop <- ggplot(data = adm_1_phone_mobility_sim_dat) + 
  geom_point(aes(x = value.y, y = value.x), size = 10, color = '#4292C6', alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + theme_minimal() +
  ylim(0, 1) + xlim(0, 1) + theme(legend.position = 'none',
                                  axis.text = element_text(size=20),
                                  axis.title = element_text(size=26),
                                  plot.title = element_text(size=30, hjust = 0.5)) + xlab('Observed Value') +
  ylab('Simulated Value')
cor(adm_1_phone_mobility_sim_dat$value.y, adm_1_phone_mobility_sim_dat$value.x, use = 'pairwise.complete.obs')

# Save formatted simulated data
adm_3_phone_mobility_sim_dat <- adm_3_phone_mobility_sim_dat %>%
  dplyr::rename('value' = 'value.x',
                'value_cat' = 'value_cat.x')
adm_2_phone_mobility_sim_dat <- adm_2_phone_mobility_sim_dat %>%
  dplyr::rename('value' = 'value.x',
                'value_cat' = 'value_cat.x')
adm_1_phone_mobility_sim_dat <- adm_1_phone_mobility_sim_dat %>%
  dplyr::rename('value' = 'value.x',
                'value_cat' = 'value_cat.x')
save(list = c('adm_3_phone_mobility_sim_dat',
              'adm_2_phone_mobility_sim_dat',
              'adm_1_phone_mobility_sim_dat'), 
     file = './tmp/simulated_phone_mobility_format.RData')

# Create Legends
legend_3 <- get_legend(adm_3_phone_plot)
adm_2_phone_plot_na <- make_matrix_plot_na(data = adm_3_phone_mobility_long_code, color = 'BuGn')
legend_2 <- get_legend(adm_2_phone_plot_na)
adm_1_phone_plot_na <- make_matrix_plot_na(data = adm_3_phone_mobility_long_code, color = 'Blues')
legend_1 <- get_legend(adm_1_phone_plot_na)
save(list = c('legend_1', 'legend_2', 'legend_3'), 
     file = './tmp/matrix_legends.RData')

# Combine matrices into pairs by administrative level
mat_1 <- cowplot::plot_grid(adm_1_phone_plot + theme(legend.position = 'none') + ggtitle('\n\nObserved\n'), 
                            adm_1_phone_plot_sim + theme(legend.position = 'none') + ggtitle('\n\nSimulated\n'), nrow = 1)
title <- ggdraw() + draw_label("Trip Proportion", hjust = 0.5, size = 26, vjust = 0.9)
mat__1 <- plot_grid(mat_1, title, legend_1, 
                    nrow = 3, rel_heights = c(1, 0.05, 0.2))

mat_2 <- cowplot::plot_grid(adm_2_phone_plot + theme(legend.position = 'none') + ggtitle('\n\nObserved\n'), 
                            adm_2_phone_plot_sim + theme(legend.position = 'none') + ggtitle('\n\nSimulated\n'), nrow = 1)
mat__2 <- plot_grid(mat_2, title, legend_2, 
                    nrow = 3, rel_heights = c(1, 0.05, 0.2))

mat_3 <- cowplot::plot_grid(adm_3_phone_plot + theme(legend.position = 'none') + ggtitle('\n\nObserved\n'), 
                            adm_3_phone_plot_sim + theme(legend.position = 'none') + ggtitle('\n\nSimulated\n'), nrow = 1)
mat__3 <- plot_grid(mat_3, title, legend_3, 
                    nrow = 3, rel_heights = c(1, 0.05, 0.2))

#############################
# 4. CREATE FIGURE AND SAVE #
#############################

figure_1 <- plot_grid(adm_1_map + 
                        scale_fill_distiller(palette = "Blues", direction = 1, name = ' ', labels = comma,
                                             breaks = seq(2000000, 6000000, 2000000)), 
                      sub_3,
                      mat__1,
                      adm_2_map, 
                      sub_2,
                      mat__2,
                      adm_3_map, 
                      sub_1,
                      mat__3,
                      nrow = 3,
                      rel_heights = c(1, 1, 1),
                      rel_widths = c(0.6, 0.4, 1),
                      labels = c('(a) Provinces', '(b)', '(c)', '(d) Districts', '(e)', '(f)',
                                 '(g) Divisions', '(h)', '(i)'),
                      label_size = 34, hjust = 0)

ggsave('./figs/figure_1.jpg', plot = figure_1, height = 30, width = 25)


plot_1 <- ggplot(adm_1_phone_mobility_long_code, aes(x = as.character(destination), y = as.character(origin), fill = value_cat)) +
  geom_tile(height = 1, width = 1) +
  scale_fill_manual(values = c('#f2e1d9', '#ddbaa4', '#c89b7e', '#b87b5c', '#FFFFFF'),
                    breaks = c("< 0.001", "0.001-0.01", 
                               "0.01-0.1", "0.1-1.0"),
                    labels = c("< 0.001", "0.001-0.01", 
                               "0.01-0.1", "0.1-1.0")) +
  xlab("Destination") +
  theme_bw() + 
  ylab("Origin") +
  ggtitle(' ') + 
  guides(fill = guide_legend(title = "", byrow = TRUE)) + 
  matrix_theme

plot_2 <- ggplot(adm_2_phone_mobility_long_code, aes(x = as.character(destination), y = as.character(origin), fill = value_cat)) +
  geom_tile(height = 1, width = 1) +
  scale_fill_manual(values = c('#def2dc', '#bce4b7', '#9ad691', '#78c86c'),
                    breaks = c("< 0.001", "0.001-0.01", 
                               "0.01-0.1", "0.1-1.0"),
                    labels = c("< 0.001", "0.001-0.01", 
                               "0.01-0.1", "0.1-1.0")) +
  xlab("Destination") +
  theme_bw() + 
  ylab("Origin") +
  ggtitle(' ') + 
  guides(fill = guide_legend(title = "", byrow = TRUE)) + 
  matrix_theme

plot <- plot_grid(make_matrix_plot(data = adm_1_phone_mobility_long_code, color = 'Greys') + theme(legend.position = 'none'),
          plot_2 + theme(legend.position = 'none'),
          nrow = 2)

ggsave('./figs/pres_plot.jpg', plot = plot, height = 20, width = 10)





plot <- plot_grid(adm_1_map + 
                    scale_fill_distiller(palette = "Blues", direction = 1, name = ' ', labels = comma,
                                         breaks = seq(2000000, 6000000, 2000000)),
                  adm_2_map,
                  adm_3_map,
                  nrow = 1)

ggsave('./figs/pres_plot_0.jpg', plot = plot, height = 10, width = 20)

plot <- plot_grid(adm_1_phone_plot + theme(legend.position = 'none'),
                  adm_2_phone_plot + theme(legend.position = 'none'), 
                  adm_3_phone_plot + theme(legend.position = 'none'),
                  nrow = 1)

ggsave('./figs/pres_plot_2.jpg', plot = plot, height = 10, width = 30)






figure_1 <- plot_grid(adm_1_map + 
                        scale_fill_distiller(palette = "Blues", direction = 1, name = ' ', labels = comma,
                                             breaks = seq(2000000, 6000000, 2000000)), 
                      adm_2_map, 
                      adm_3_map,
                      nrow = 1,
                      rel_heights = c(1),
                      rel_widths = c(1, 1, 1),
                      labels = c('(a) Provinces', '(b) Districts', '(c) Divisions'),
                                 label_size = 40, hjust = 0)
ggsave('./figs/poster_1.jpg', plot = figure_1, height = 12, width = 20)



figure_1 <- plot_grid(adm_1_phone_plot,
                      adm_2_phone_plot,
                      adm_3_phone_plot,
                      nrow = 1,
                      rel_heights = c(1),
                      rel_widths = c(1, 1, 1))
ggsave('./figs/poster_2.jpg', plot = figure_1, height = 13, width = 34)


supp_1.1 <- plot_grid(sim_comp_1 + ggtitle('\n\nProvince'), 
                    sim_comp_2 + ggtitle('\nDistrict'), 
                    sim_comp_3 + ggtitle('\nDivision'), 
                    nrow = 3,
                      labels = c('(a)', '(b)', '(c)'),
                      label_size = 34, hjust = 0)

supp_1.2 <- plot_grid(
                      sim_comp_1_prop + ggtitle('\n\nProvince'),
                      sim_comp_2_prop + ggtitle('\nDistrict'),
                      sim_comp_3_prop + ggtitle('\nDivision'),
                      nrow = 3,
                      labels = c('(d)', '(e)', '(f)'),
                      label_size = 34, hjust = 0)

supp_1 <- plot_grid(
  supp_1.1, supp_1.2,
  nrow = 1,
  labels = c('       Trip Count', 'Trip Proportion'),
  label_size = 34, hjust = -1.45)

ggsave('./figs/supp_1.jpg', plot = supp_1, height = 30, width = 25)

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

sum(adm_1_long_pred$trips_avg)
sum(adm_1_long_pred$value)
max(adm_1_long_pred$trips_avg)
max(adm_1_long_pred$value)
sum(adm_1_long_pred$distance*adm_1_long_pred$trips_avg)/sum(adm_1_long_pred$trips_avg)
sum(adm_1_long_pred$distance*adm_1_long_pred$value)/sum(adm_1_long_pred$value)

sum(adm_2_long_pred$trips_avg)
sum(adm_2_long_pred$value)
max(adm_2_long_pred$trips_avg)
max(adm_2_long_pred$value)
sum(adm_2_long_pred$distance*adm_2_long_pred$trips_avg)/sum(adm_2_long_pred$trips_avg)
sum(adm_2_long_pred$distance*adm_2_long_pred$value)/sum(adm_2_long_pred$value)

sum(adm_3_long_pred$trips_avg, na.rm = TRUE)
sum(adm_3_long_pred$value)
max(adm_3_long_pred$trips_avg, na.rm = TRUE)
max(adm_3_long_pred$value)
sum(adm_3_long_pred$distance*adm_3_long_pred$trips_avg, na.rm = TRUE)/sum(adm_3_long_pred$trips_avg, na.rm = TRUE)
sum(adm_3_long_pred$distance*adm_3_long_pred$value)/sum(adm_3_long_pred$value)


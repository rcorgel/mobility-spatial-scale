################################################################################
# File Name: 05c_figure_2_descriptive                                          #
#                                                                              #
# Purpose:   Create figure 2 for the manuscript.                               #
#                                                                              #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure functions                                        #
#            3. Create map subfigures                                          #
#            4. Create scatter subfigures                                      #
#            5. Create matrix subfigures                                       #
#            6. Combine subfigures                                             #
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
    theme_void() + ggtitle(' ') + theme(legend.position = 'inside', legend.position.inside = c(0.85, 0.90),
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
                      axis.title = element_text(size=28),
                      legend.text = element_text(size = 28),
                      legend.title = element_text(size = 12),
                      legend.position = "bottom",
                      legend.box="vertical",
                      legend.margin=margin(),
                      strip.background = element_blank(),
                      legend.spacing.y = unit(0.25, 'cm'),
                      legend.key.size = unit(1, 'cm'),
                      strip.text = element_text(size = 16),
                      plot.title = element_text(size=28, hjust = 0.5))

scatter_theme <- theme(legend.text = element_text(size = 28),
                       legend.title = element_text(size = 20),
                       axis.text = element_text(size=20),
                       axis.title = element_text(size=28),
                       legend.position = "bottom",
                       legend.box="vertical",
                       legend.margin=margin(),
                       strip.background = element_blank(),
                       legend.spacing.y = unit(0.25, 'cm'),
                       legend.key.size = unit(1, 'cm'),
                       strip.text = element_text(size = 16),
                       plot.title = element_text(size=28, hjust = 0.5))

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

# Add coordinates of highlight cities/units
# Administrative level 3
coordinate_cities_3 <- data.frame(
  city = c("Colombo", "Madhu"),
  lat = c(6.927632561772342, 8.85653415340985),
  long = c(79.85843709788357, 80.20433649099449))   
# Administrative level 2
coordinate_cities_2 <- data.frame(
  city = c("Colombo", "Mannar"),
  lat = c(6.866667, 8.866667),
  long = c(80.016667, 80.066667))   
# Administrative level 1
coordinate_cities_1 <- data.frame(
  city = c("Western", "Northern"),
  lat = c(6.833333, 9.2),
  long = c(80.083333, 80.416667))   

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
mobility_shape_xwalk <- readRDS('./tmp/mobility_shape_xwalk.rds')

# Merge on the cross walk
choropleth_3 <- left_join(choropleth_3, mobility_shape_xwalk, by = c('ADM3_EN' = 'adm_3_shape'))

# Join polygons to create 330 mobility admin 3 units
choropleth_3_mobility <- choropleth_3 |> 
  group_by(adm_3_mobility) |>
  summarise(geometry = sf::st_union(geometry)) |>
  ungroup()

# Administrative level 1
choropleth_1 <- left_join(choropleth_1, adm_1_population_dat, by = c('ADM1_EN' = 'adm_1'))
choropleth_1 <- choropleth_1 |> dplyr::rename('population' = 'population_2020_adm_1')
adm_1_map <- make_simple_map(data = choropleth_1, color = "Blues", coord_data = coordinate_cities_1) +
  geom_label(data = coordinate_cities_1, aes(x = long, y = lat + c(0.62, 0.77) , label = city), 
             fill = 'white', size = 8, fontface = 'bold', alpha = 0.85) +
  geom_sf(data = choropleth_1[c(5, 9),], aes(), fill = '#00000000', color= 'black', linewidth = 1.3)

# Administrative level 2
choropleth_2 <- left_join(choropleth_2, adm_2_population_dat, by = c('ADM2_EN' = 'adm_2'))
choropleth_2 <- choropleth_2 |> dplyr::rename('population' = 'population_2020_adm_2')
adm_2_map <- make_simple_map(data = choropleth_2, color = "BuGn", coord_data = coordinate_cities_2) +
  geom_label(data = coordinate_cities_2, aes(x = long, y = lat  + c(0.24, 0.49), label = city), 
             fill = 'white', size = 8, fontface = 'bold', alpha = 0.85) +
  geom_sf(data = choropleth_2[c(1, 11),], aes(), fill = '#00000000', color= 'black', linewidth = 1.3)

# Administrative level 3
choropleth_3_mobility <- left_join(choropleth_3_mobility, adm_3_population_dat, 
                                   by = c('adm_3_mobility' = 'adm_3_mobility'))
choropleth_3_mobility <- choropleth_3_mobility |> dplyr::rename('population' = 'population_2020_adm_3')
adm_3_map <- make_simple_map(data = choropleth_3_mobility, color = "Purples", coord_data = coordinate_cities_3) +
  geom_point(data = coordinate_cities_3, aes(x = long, y = lat), colour="white", fill = 'black', 
             size=5, alpha = 0.9, shape=21) + 
  geom_label(data = coordinate_cities_3, aes(x = long, y = lat + 0.16 , label = city), 
             fill = 'white', size = 8, fontface = 'bold', alpha = 0.85)

################################
# 4. CREATE SCATTER SUBFIGURES #
################################

# Load simulated trip count data
load('./tmp/adm_sim_mobility_dat.RData')

# Load distance data
load('./tmp/adm_dist_mat.RData')

# Load observed trip count data
load('./tmp/adm_phone_mobility_dat.RData')

# Merge simulated and observed trip count data in addition to distance
# Administrative Level 1
adm_1_long_pred <- left_join(adm_1_sim_mobility_dat, 
                             adm_1_phone_mobility_dat[, c('adm_1_origin', 'adm_1_destination', 'trips_avg')],
                             by = c('adm_1_origin' = 'adm_1_origin', 
                                    'adm_1_destination' = 'adm_1_destination'))
adm_1_long_pred <- left_join(adm_1_long_pred, 
                             adm_1_dist,
                             by = c('adm_1_origin' = 'adm_1_origin', 
                                    'adm_1_destination' = 'adm_1_destination'))
# Administrative Level 2
adm_2_long_pred <- left_join(adm_2_sim_mobility_dat, 
                             adm_2_phone_mobility_dat[, c('adm_2_origin', 'adm_2_destination', 'trips_avg')],
                             by = c('adm_2_origin' = 'adm_2_origin', 
                                    'adm_2_destination' = 'adm_2_destination'))
adm_2_long_pred <- left_join(adm_2_long_pred, 
                             adm_2_dist,
                             by = c('adm_2_origin' = 'adm_2_origin', 
                                    'adm_2_destination' = 'adm_2_destination'))
# Administrative Level 3
adm_3_long_pred <- left_join(adm_3_sim_mobility_dat, 
                             adm_3_phone_mobility_dat[, c('adm_3_origin', 'adm_3_destination', 'trips_avg')],
                             by = c('adm_3_origin' = 'adm_3_origin', 
                                    'adm_3_destination' = 'adm_3_destination'))
adm_3_long_pred <- left_join(adm_3_long_pred, 
                             adm_3_dist,
                             by = c('adm_3_origin' = 'adm_3_origin', 
                                    'adm_3_destination' = 'adm_3_destination'))

# Plot scatter plots of trip count by trip distance for highlight cities
# Administrative Level 3
sim <- adm_3_long_pred[, c('adm_3_origin', 'adm_3_destination', 'value', 'distance')]
sim$Cat <- 'Simulated'
obs <- adm_3_long_pred[, c('adm_3_origin', 'adm_3_destination', 'trips_avg', 'distance')]
obs$Cat <- 'Observed'
obs <- obs |> dplyr::rename('value' = 'trips_avg')
all <- rbind(sim, obs)

plot_1 <- ggplot(data = all[all$adm_3_destination == 'Madhu',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.25, size = 4) + theme_classic() +
  xlab('Distance (km)') + 
  scale_y_log10("Trip Counts",
                breaks = c(10^-8, 10^-4, 10^0, 10^4, 10^8),
                labels = trans_format("log10", math_format(10^.x)),
                n.breaks = 12,
                limits = c(10^-8, 10^8)) + ggtitle('\n\nMadhu Division\n(Rural)') +
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
                limits = c(10^-8, 10^8)) + ggtitle('\n\nColombo Division\n(Urban)') +
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
obs <- obs |> dplyr::rename('value' = 'trips_avg')
all <- rbind(sim, obs)

plot_3 <- ggplot(data = all[all$adm_2_destination == 'Mannar',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.65, size = 4) + theme_classic() +
  xlab('Distance (km)') + 
  scale_y_log10("Trip Counts",
                breaks = c(10^-8, 10^-4, 10^0, 10^4, 10^8),
                labels = trans_format("log10", math_format(10^.x)),
                n.breaks = 12,
                limits = c(10^-8, 10^8)) + ggtitle('\n\nMannar District\n(Rural)') +
  scale_color_manual('', values = c('#41AE76', 'grey70')) +
  scatter_theme + theme(plot.title = element_text(hjust = 0.5),
                        legend.position = 'none')
cor(all[all$adm_2_destination == 'Mannar',]$distance, all[all$adm_2_destination == 'Mannar',]$value)

plot_4 <- ggplot(data = all[all$adm_2_destination == 'Colombo',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.65, size = 4) + theme_classic() +
  xlab('Distance (km)') + 
  scale_y_log10("Trip Counts",
                breaks = c(10^-8, 10^-4, 10^0, 10^4, 10^8),
                labels = trans_format("log10", math_format(10^.x)),
                n.breaks = 12,
                limits = c(10^-8, 10^8)) + ggtitle('\n\nColombo District\n(Urban)') +
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
obs <- obs |> dplyr::rename('value' = 'trips_avg')
all <- rbind(sim, obs)

plot_5 <- ggplot(data = all[all$adm_1_destination == 'Northern',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.65, size = 4) + theme_classic() +
  xlab('Distance (km)') + 
  scale_y_log10("Trip Counts",
                breaks = c(10^-8, 10^-4, 10^0, 10^4, 10^8),
                labels = trans_format("log10", math_format(10^.x)),
                n.breaks = 12,
                limits = c(10^-8, 10^8)) + ggtitle('\n\nNorthern Province\n(Rural)') +
  scale_color_manual('', values = c('#4292C6', 'grey70')) +
  scatter_theme + theme(plot.title = element_text(hjust = 0.5),
                        legend.position = 'none')
cor(all[all$adm_1_destination == 'Northern',]$distance, all[all$adm_1_destination == 'Northern',]$value)

plot_6 <- ggplot(data = all[all$adm_1_destination == 'Western',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.65, size = 4) + theme_classic() +
  xlab('Distance (km)') + 
  scale_y_log10("Trip Counts",
                breaks = c(10^-8, 10^-4, 10^0, 10^4, 10^8),
                labels = trans_format("log10", math_format(10^.x)),
                n.breaks = 12,
                limits = c(10^-8, 10^8)) + ggtitle('\n\nWestern Province\n(Urban)') +
  scale_color_manual('', values = c('#4292C6', 'grey70')) +
  scatter_theme + theme(plot.title = element_text(hjust = 0.5),
                        legend.position = 'none')
cor(all[all$adm_1_destination == 'Western',]$distance, all[all$adm_1_destination == 'Western',]$value)

legend_adm_1 <- ggplot(data = all[all$adm_1_destination == 'Western',]) +
  geom_point(aes(x = distance, y = value, color = Cat), alpha = 0.85, size = 6) + theme_classic() +
  scale_color_manual('', values = c('#4292C6', 'grey70')) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom') + scatter_theme

# Combine individual scatter plots into pairs for plotting
legend_3 <- get_legend(legend_adm_3)
sub_1 <- plot_grid(plot_1, plot_2, legend_3, 
          nrow = 3, rel_heights = c(1, 1, 0.2))

legend_2 <- get_legend(legend_adm_2)
sub_2 <- plot_grid(plot_3, plot_4, legend_2,
          nrow = 3, rel_heights = c(1, 1, 0.2))

legend_1 <- get_legend(legend_adm_1)
sub_3 <- plot_grid(plot_5, plot_6, legend_1,
          nrow = 3, rel_heights = c(1, 1, 0.2))

##################################################
# 6. CREATE MATRIX SUBFIGURES AND S.2 SUBFIGURES #
##################################################

#####################
# MOBILE PHONE DATA #
#####################

# Load phone mobility data
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')
load('./tmp/fmt_adm_2_phone_mobility_dat.RData')
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')

# Load simulated mobility data
load('./tmp/fmt_adm_3_sim_mobility_dat.RData')
load('./tmp/fmt_adm_2_sim_mobility_dat.RData')
load('./tmp/fmt_adm_1_sim_mobility_dat.RData')

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

# Administrative Unit 3
# Make category variable
adm_3_sim_mobility_long_code$value_cat <- cut(adm_3_sim_mobility_long_code$value,
                                              breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                              labels = c("< 0.001", "0.001-0.01", 
                                                         "0.01-0.1", "0.1-1.0"))
# Create plot
adm_3_phone_plot_sim <- make_matrix_plot(data = adm_3_sim_mobility_long_code, color = 'Purples')

# Administrative Unit 2
# Make category variable
adm_2_sim_mobility_long_code$value_cat <- cut(adm_2_sim_mobility_long_code$value,
                                              breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                              labels = c("< 0.001", "0.001-0.01", 
                                                         "0.01-0.1", "0.1-1.0"))
# Create plot
adm_2_phone_plot_sim <- make_matrix_plot(data = adm_2_sim_mobility_long_code, color = 'BuGn')

# Administrative Unit 1
# Make category variable
adm_1_sim_mobility_long_code$value_cat <- cut(adm_1_sim_mobility_long_code$value,
                                              breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                              labels = c("< 0.001", "0.001-0.01", 
                                                         "0.01-0.1", "0.1-1.0"))
# Create plot
adm_1_phone_plot_sim <- make_matrix_plot(data = adm_1_sim_mobility_long_code, color = 'Blues')

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
title <- ggdraw() + draw_label("Trip Proportion", hjust = 0.5, size = 28, vjust = 0.9)
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

#########################
# 7. COMBINE SUBFIGURES #
#########################

# CREATE FIGURE 2
figure_2 <- plot_grid(ggplot() + theme_void(),
                      ggplot() + theme_void(),
                      ggplot() + theme_void(),
                      adm_1_map + 
                        scale_fill_distiller(palette = "Blues", direction = 1, name = ' ', labels = comma,
                                             breaks = seq(2000000, 6000000, 2000000)), 
                      sub_3,
                      mat__1,
                      ggplot() + theme_void(),
                      ggplot() + theme_void(),
                      ggplot() + theme_void(),
                      adm_2_map + 
                        scale_fill_distiller(palette = "BuGn", direction = 1, name = ' ', labels = comma,
                                             breaks = seq(750000, 2250000, 750000)), 
                      sub_2,
                      mat__2,
                      ggplot() + theme_void(),
                      ggplot() + theme_void(),
                      ggplot() + theme_void(),
                      adm_3_map, 
                      sub_1,
                      mat__3,
                      nrow = 6,
                      rel_heights = c(0.06, 1, 0.06, 1, 0.06, 1),
                      rel_widths = c(0.6, 0.4, 1),
                      labels = c('Provinces','', '', '(a)', '(b)', '(c)', 'Districts', '', '', '(d)', '(e)', '(f)',
                                 'Divisions', '', '', '(g)', '(h)', '(i)'),
                      label_size = 34, hjust = 0)

ggsave('./figs/figure_2_descriptive.jpg', plot = figure_2, height = 30, width = 25)

################################################################################
################################################################################

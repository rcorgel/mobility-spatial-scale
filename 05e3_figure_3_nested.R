################################################################################
# File Name: 05e_figure_3_nested                                               #
#                                                                              #
# Purpose:   Create figure 3 for the manuscript.                               #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure functions                                        #
#            4. Create subfigures                                              #
#            5. Create final figure                                            #
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

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

########################
# 3. CREATE SUBFIGURES #
########################

# Load the shape files
# From: https://data.humdata.org/dataset/cod-ab-mdg
choropleth_0 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm0_slsd_20220816')
choropleth_1 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm1_slsd_20220816')
choropleth_2 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm2_slsd_20220816')
choropleth_3 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm3_slsd_20220816')

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

# Load admin cross walk
admin_xwalk <- read_csv('./tmp/admin_xwalk.csv')

# Load mobility data for each data source
adm_3_phone_mobility_long <- readRDS('./out/adm_3_phone_mobility_long.rds')
adm_2_phone_mobility_long <- readRDS('./out/adm_2_phone_mobility_long.rds')
adm_1_phone_mobility_long <- readRDS('./out/adm_1_phone_mobility_long.rds')

# Load population data
adm_3_population_dat <- readRDS('./out/adm_3_population_dat.rds')
adm_2_population_dat <- readRDS('./out/adm_2_population_dat.rds')
adm_1_population_dat <- readRDS('./out/adm_1_population_dat.rds')

#######################
# PHONE MOBILITY DATA #
#######################

######################
# ADMIN 3 to ADMIN 1 #
######################

# Merge on origin
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 3)],
                                       by = c('origin' = 'adm_3'))

# Merge on destination
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 3)], 
                                       by = c('destination' = 'adm_3'))

# Calculate out of province travel at the admin 3 unit
adm_3_adm_1_phone <- adm_3_phone_mobility_long |>
  group_by(origin, adm_1.x, adm_1.y) |>
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_1.x, adm_1.y, adm_3_sum, .keep_all = FALSE) |>
  dplyr::rename('adm_1_origin' = 'adm_1.x',
                'adm_1_destination' = 'adm_1.y')

# Merge on origin from admin 1 and travel probabillity
adm_3_adm_1_phone <- left_join(adm_3_adm_1_phone, 
                               adm_1_phone_mobility_long, 
                               by = c('adm_1_origin' = 'origin',
                                      'adm_1_destination' = 'destination'))

# Calculate 1 - stays to get the leave probability and difference between units
adm_3_adm_1_phone_leave <- adm_3_adm_1_phone |>
  dplyr::filter(adm_1_origin == adm_1_destination) |>
  mutate(adm_3_leave = 1 - adm_3_sum,
         adm_1_leave = 1 - value,
         difference = adm_3_leave - adm_1_leave)
  
# Merge on admin 3 population
adm_3_adm_1_phone_leave <- left_join(adm_3_adm_1_phone_leave, adm_3_population_dat[, c(1, 4)], 
                               by = c('origin' = 'adm_3_mobility'))

################
# PLOT SCATTER #
################

# Create scatter plot by population
plot_3_1_scatter <- ggplot(adm_3_adm_1_phone_leave) +
  geom_point(aes(x = log(population_2020_adm_3), y = difference*100, 
                 fill = difference*100), color = '#565656', alpha = 0.85, size = 8, shape = 21) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = log(population_2020_adm_3), y = difference*100), color = '#565656', 
              method = "loess", se = TRUE, linewidth = 3, alpha = 0.25) + 
  ylim(-0.36*100, 0.60*100) +
  ggtitle('Difference in Province Leave Probability \n(Division - Province)') +
  xlab('Log Population (Division)') + 
  ylab('Leave Probability Difference (%)') +
  theme_minimal() +
  theme(
    axis.title = element_text(size=26),
    axis.text = element_text(size=26),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24),
    legend.key.height = unit(3.3, 'cm'),
    legend.key.width = unit(1.4, 'cm'),
    plot.title = element_text(size=30, hjust = 0.5)) +
  scale_fill_gradient2('Difference (%)\n',
                       low = '#41AE76', mid = "white", high = '#4292C6',
                       midpoint = 0, limits=c(-0.30*100, 0.60*100)) +
  scale_x_continuous(limits = c(6, 13), breaks = c(6, 8, 10, 12))

############
# PLOT MAP #
############

choropleth_3_mobility_1 <- left_join(choropleth_3_mobility, adm_3_adm_1_phone_leave[ c('origin', 'difference')], by = 
                                     c('adm_3_mobility' = 'origin'))
plot_3_1_map <- ggplot() +
  geom_sf(data = choropleth_3_mobility_1, aes(fill = difference), color= 'black', linewidth = 0.25, alpha = 1) +
  geom_sf(data = choropleth_1, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(legend.position = 'none',
                                     plot.title = element_text(size = 30, hjust = 0.5),
                                     legend.text = element_text(size = 24),
                                     legend.title = element_text(size = 24),
                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#41AE76', mid = "white", high = '#4292C6',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.36, 0.60))


######################
# ADMIN 3 to ADMIN 2 #
######################

# Load mobility data for each data source
adm_3_phone_mobility_long <- readRDS('./out/adm_3_phone_mobility_long.rds')
adm_2_phone_mobility_long <- readRDS('./out/adm_2_phone_mobility_long.rds')
adm_1_phone_mobility_long <- readRDS('./out/adm_1_phone_mobility_long.rds')

# Load population data
adm_3_population_dat <- readRDS('./out/adm_3_population_dat.rds')
adm_2_population_dat <- readRDS('./out/adm_2_population_dat.rds')
adm_1_population_dat <- readRDS('./out/adm_1_population_dat.rds')

# Merge on origin
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 2)],
                                       by = c('origin' = 'adm_3'))

# Merge on destination
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 2)], 
                                       by = c('destination' = 'adm_3'))

# Calculate out of province travel at the admin 3 unit
adm_3_adm_2_phone <- adm_3_phone_mobility_long |>
  group_by(origin, adm_2.x, adm_2.y) |>
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_2.x, adm_2.y, adm_3_sum, .keep_all = FALSE) |>
  dplyr::rename('adm_2_origin' = 'adm_2.x',
                'adm_2_destination' = 'adm_2.y')

# Merge on origin from admin 2 and travel probabillity
adm_3_adm_2_phone <- left_join(adm_3_adm_2_phone, 
                               adm_2_phone_mobility_long, 
                               by = c('adm_2_origin' = 'origin',
                                      'adm_2_destination' = 'destination'))

# Calculate 2 - stays to get the leave probability and difference between units
adm_3_adm_2_phone_leave <- adm_3_adm_2_phone |>
  dplyr::filter(adm_2_origin == adm_2_destination) |>
  mutate(adm_3_leave = 1 - adm_3_sum,
         adm_2_leave = 1 - value,
         difference = adm_3_leave - adm_2_leave)

# Merge on admin 3 population
adm_3_adm_2_phone_leave <- left_join(adm_3_adm_2_phone_leave, adm_3_population_dat[, c(1, 4)], 
                                     by = c('origin' = 'adm_3_mobility'))

################
# PLOT SCATTER #
################

# Create scatter plot by population
plot_3_2_scatter <- ggplot(adm_3_adm_2_phone_leave) +
  geom_point(aes(x = log(population_2020_adm_3), y = difference*100, 
                 fill = difference*100), color = '#565656', alpha = 0.85, size = 8, shape = 21) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = log(population_2020_adm_3), y = difference*100), color = '#565656', 
              method = "loess", se = TRUE, linewidth = 3, alpha = 0.25) + 
  ylim(-0.36*100, 0.60*100) +
  ggtitle('Difference in District Leave Probability \n(Division - District)') +
  xlab('Log Population (Division)') + 
  ylab('Leave Probability Difference (%)') +
  theme_minimal() +
  theme(
    axis.title = element_text(size=26),
    axis.text = element_text(size=26),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24),
    legend.key.height = unit(3.3, 'cm'),
    legend.key.width = unit(1.4, 'cm'),
    plot.title = element_text(size=30, hjust = 0.5)) +
  scale_fill_gradient2('Difference (%)\n',
                       low = '#42AE76', mid = "white", high = '#4292C6',
                       midpoint = 0, limits=c(-0.30*100, 0.60*100)) +
  scale_x_continuous(limits = c(6, 13), breaks = c(6, 8, 10, 12))

############
# PLOT MAP #
############

choropleth_3_mobility_2 <- left_join(choropleth_3_mobility, adm_3_adm_2_phone_leave[ c('origin', 'difference')], by = 
                                     c('adm_3_mobility' = 'origin'))
plot_3_2_map <- ggplot() +
  geom_sf(data = choropleth_3_mobility_2, aes(fill = difference), color= 'black', linewidth = 0.25, alpha = 2) +
  geom_sf(data = choropleth_2, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(legend.position = 'none',
                                     plot.title = element_text(size = 30, hjust = 0.5),
                                     legend.text = element_text(size = 24),
                                     legend.title = element_text(size = 24),
                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#42AE76', mid = "white", high = '#4292C6',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.36, 0.60))
 
######################
# ADMIN 2 to ADMIN 1 #
######################

# Change admin cross walk to admin 2 level
admin_xwalk_adm_2 <- admin_xwalk |>
  group_by(adm_2, adm_1) |>
  distinct(adm_2, adm_1, .keep_all = FALSE)

# Load mobility data for each data source
adm_3_phone_mobility_long <- readRDS('./out/adm_3_phone_mobility_long.rds')
adm_2_phone_mobility_long <- readRDS('./out/adm_2_phone_mobility_long.rds')
adm_1_phone_mobility_long <- readRDS('./out/adm_1_phone_mobility_long.rds')

# Load population data
adm_3_population_dat <- readRDS('./out/adm_3_population_dat.rds')
adm_2_population_dat <- readRDS('./out/adm_2_population_dat.rds')
adm_1_population_dat <- readRDS('./out/adm_1_population_dat.rds')

# Merge on origin
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, admin_xwalk_adm_2[, c(1, 2)],
                                       by = c('origin' = 'adm_2'))

# Merge on destination
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, admin_xwalk_adm_2[, c(1, 2)], 
                                       by = c('destination' = 'adm_2'))

# Calculate out of province travel at the admin 2 unit
adm_2_adm_1_phone <- adm_2_phone_mobility_long |>
  group_by(origin, adm_1.x, adm_1.y) |>
  mutate(adm_2_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_1.x, adm_1.y, adm_2_sum, .keep_all = FALSE) |>
  dplyr::rename('adm_1_origin' = 'adm_1.x',
                'adm_1_destination' = 'adm_1.y')

# Merge on origin from admin 1 and travel probability
adm_2_adm_1_phone <- left_join(adm_2_adm_1_phone, 
                               adm_1_phone_mobility_long, 
                               by = c('adm_1_origin' = 'origin',
                                      'adm_1_destination' = 'destination'))

# Calculate 2 - stays to get the leave probability and difference between units
adm_2_adm_1_phone_leave <- adm_2_adm_1_phone |>
  dplyr::filter(adm_1_origin == adm_1_destination) |>
  mutate(adm_2_leave = 1 - adm_2_sum,
         adm_1_leave = 1 - value,
         difference = adm_2_leave - adm_1_leave)

# Merge on admin 2 population
adm_2_adm_1_phone_leave <- left_join(adm_2_adm_1_phone_leave, adm_2_population_dat[, c(1, 3)], 
                                     by = c('origin' = 'adm_2'))

################
# PLOT SCATTER #
################

# Create scatter plot by population
plot_2_1_scatter <- ggplot(adm_2_adm_1_phone_leave) +
  geom_point(aes(x = log(population_2020_adm_2), y = difference*100, 
                 fill = difference*100), color = '#565656', alpha = 0.85, size = 8, shape = 21) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 1) +
  geom_smooth(aes(x = log(population_2020_adm_2), y = difference*100), color = '#565656', 
              method = "loess", se = TRUE, linewidth = 3, alpha = 0.25) + 
  ylim(-0.36*100, 0.60*100) +
  ggtitle('Difference in Province Leave Probability \n(District - Province)') +
  xlab('Log Population (District)') + 
  ylab('Leave Probability Difference (%)') +
  theme_minimal() +
  theme(
    axis.title = element_text(size=26),
    axis.text = element_text(size=26),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24),
    legend.key.height = unit(3.3, 'cm'),
    legend.key.width = unit(1.4, 'cm'),
    plot.title = element_text(size=30, hjust = 0.5)) +
  scale_fill_gradient2('Difference (%)\n',
                       low = '#42AE76', mid = "white", high = '#4292C6',
                       midpoint = 0, limits=c(-0.30*100, 0.60*100)) +
  scale_x_continuous(limits = c(10, 15), breaks = c(10, 12, 14))

############
# PLOT MAP #
############

choropleth_2_1 <- left_join(choropleth_2, adm_2_adm_1_phone_leave[ c('origin', 'difference')], by = 
                                       c('ADM2_EN' = 'origin'))
plot_2_1_map <- ggplot() +
  geom_sf(data = choropleth_2_1, aes(fill = difference), color= 'black', linewidth = 0.25, alpha = 2) +
  geom_sf(data = choropleth_1, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(legend.position = 'none',
                                     plot.title = element_text(size = 30, hjust = 0.5),
                                     legend.text = element_text(size = 24),
                                     legend.title = element_text(size = 24),
                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#42AE76', mid = "white", high = '#4292C6',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.36, 0.60))

# TEXT CALL OUTS

mean(adm_3_adm_1_phone_leave$difference)
min(adm_3_adm_1_phone_leave$difference)
max(adm_3_adm_1_phone_leave$difference)


mean(adm_3_adm_2_phone_leave$difference)
min(adm_3_adm_2_phone_leave$difference)
max(adm_3_adm_2_phone_leave$difference)

mean(adm_2_adm_1_phone_leave$difference)
min(adm_2_adm_1_phone_leave$difference)
max(adm_2_adm_1_phone_leave$difference)



##################
# SIMULATED DATA #
##################

######################
# ADMIN 3 to ADMIN 1 #
######################

# Load mobility data for each data source
adm_3_phone_mobility_long <- readRDS('./out/adm_3_sim_mobility_long.rds')
adm_2_phone_mobility_long <- readRDS('./out/adm_2_sim_mobility_long.rds')
adm_1_phone_mobility_long <- readRDS('./out/adm_1_sim_mobility_long.rds')

# Merge on origin
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 3)],
                                       by = c('origin' = 'adm_3'))

# Merge on destination
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 3)], 
                                       by = c('destination' = 'adm_3'))

# Calculate out of province travel at the admin 3 unit
adm_3_adm_1_phone <- adm_3_phone_mobility_long |>
  group_by(origin, adm_1.x, adm_1.y) |>
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_1.x, adm_1.y, adm_3_sum, .keep_all = FALSE) |>
  dplyr::rename('adm_1_origin' = 'adm_1.x',
                'adm_1_destination' = 'adm_1.y')

# Merge on origin from admin 1 and travel probabillity
adm_3_adm_1_phone <- left_join(adm_3_adm_1_phone, 
                               adm_1_phone_mobility_long, 
                               by = c('adm_1_origin' = 'origin',
                                      'adm_1_destination' = 'destination'))

# Calculate 1 - stays to get the leave probability and difference between units
adm_3_adm_1_phone_leave <- adm_3_adm_1_phone |>
  dplyr::filter(adm_1_origin == adm_1_destination) |>
  mutate(adm_3_leave = 1 - adm_3_sum,
         adm_1_leave = 1 - value,
         difference = adm_3_leave - adm_1_leave)

# Merge on admin 3 population
adm_3_adm_1_phone_leave <- left_join(adm_3_adm_1_phone_leave, adm_3_population_dat[, c(1, 4)], 
                                     by = c('origin' = 'adm_3_mobility'))

################
# PLOT SCATTER #
################

# Create scatter plot by population
plot_3_1_scatter_sim <- ggplot(adm_3_adm_1_phone_leave) +
  geom_point(aes(x = log(population_2020_adm_3), y = difference*100, 
                 fill = difference*100), color = '#565656', alpha = 0.85, size = 8, shape = 21) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = log(population_2020_adm_3), y = difference*100), color = '#565656', 
              method = "loess", se = TRUE, linewidth = 3, alpha = 0.25) + 
  ylim(-0.36*100, 0.60*100) +
  ggtitle('Difference in Province Leave Probability \n(Division - Province)') +
  xlab('Log Population (Division)') + 
  ylab('Leave Probability Difference (%)') +
  theme_minimal() +
  theme(
    axis.title = element_text(size=26),
    axis.text = element_text(size=26),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24),
    legend.key.height = unit(3.3, 'cm'),
    legend.key.width = unit(1.4, 'cm'),
    plot.title = element_text(size=30, hjust = 0.5)) +
  scale_fill_gradient2('Difference (%)\n',
                       low = '#41AE76', mid = "white", high = '#4292C6',
                       midpoint = 0, limits=c(-0.30*100, 0.60*100)) +
  scale_x_continuous(limits = c(6, 13), breaks = c(6, 8, 10, 12))

############
# PLOT MAP #
############

choropleth_3_mobility_1 <- left_join(choropleth_3_mobility, adm_3_adm_1_phone_leave[ c('origin', 'difference')], by = 
                                       c('adm_3_mobility' = 'origin'))
plot_3_1_map_sim <- ggplot() +
  geom_sf(data = choropleth_3_mobility_1, aes(fill = difference), color= 'black', linewidth = 0.25, alpha = 1) +
  geom_sf(data = choropleth_1, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(legend.position = 'none',
                                     plot.title = element_text(size = 30, hjust = 0.5),
                                     legend.text = element_text(size = 24),
                                     legend.title = element_text(size = 24),
                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#41AE76', mid = "white", high = '#4292C6',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.36, 0.60))


######################
# ADMIN 3 to ADMIN 2 #
######################

# Load mobility data for each data source
adm_3_phone_mobility_long <- readRDS('./out/adm_3_sim_mobility_long.rds')
adm_2_phone_mobility_long <- readRDS('./out/adm_2_sim_mobility_long.rds')
adm_1_phone_mobility_long <- readRDS('./out/adm_1_sim_mobility_long.rds')

# Load population data
adm_3_population_dat <- readRDS('./out/adm_3_population_dat.rds')
adm_2_population_dat <- readRDS('./out/adm_2_population_dat.rds')
adm_1_population_dat <- readRDS('./out/adm_1_population_dat.rds')

# Merge on origin
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 2)],
                                       by = c('origin' = 'adm_3'))

# Merge on destination
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 2)], 
                                       by = c('destination' = 'adm_3'))

# Calculate out of province travel at the admin 3 unit
adm_3_adm_2_phone <- adm_3_phone_mobility_long |>
  group_by(origin, adm_2.x, adm_2.y) |>
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_2.x, adm_2.y, adm_3_sum, .keep_all = FALSE) |>
  dplyr::rename('adm_2_origin' = 'adm_2.x',
                'adm_2_destination' = 'adm_2.y')

# Merge on origin from admin 2 and travel probabillity
adm_3_adm_2_phone <- left_join(adm_3_adm_2_phone, 
                               adm_2_phone_mobility_long, 
                               by = c('adm_2_origin' = 'origin',
                                      'adm_2_destination' = 'destination'))

# Calculate 2 - stays to get the leave probability and difference between units
adm_3_adm_2_phone_leave <- adm_3_adm_2_phone |>
  dplyr::filter(adm_2_origin == adm_2_destination) |>
  mutate(adm_3_leave = 1 - adm_3_sum,
         adm_2_leave = 1 - value,
         difference = adm_3_leave - adm_2_leave)

# Merge on admin 3 population
adm_3_adm_2_phone_leave <- left_join(adm_3_adm_2_phone_leave, adm_3_population_dat[, c(1, 4)], 
                                     by = c('origin' = 'adm_3_mobility'))

################
# PLOT SCATTER #
################

# Create scatter plot by population
plot_3_2_scatter_sim <- ggplot(adm_3_adm_2_phone_leave) +
  geom_point(aes(x = log(population_2020_adm_3), y = difference*100, 
                 fill = difference*100), color = '#565656', alpha = 0.85, size = 8, shape = 21) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = log(population_2020_adm_3), y = difference*100), color = '#565656', 
              method = "loess", se = TRUE, linewidth = 3, alpha = 0.25) + 
  ylim(-0.36*100, 0.60*100) +
  ggtitle('Difference in District Leave Probability \n(Division - District)') +
  xlab('Log Population (Division)') + 
  ylab('Leave Probability Difference (%)') +
  theme_minimal() +
  theme(
    axis.title = element_text(size=26),
    axis.text = element_text(size=26),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24),
    legend.key.height = unit(3.3, 'cm'),
    legend.key.width = unit(1.4, 'cm'),
    plot.title = element_text(size=30, hjust = 0.5)) +
  scale_fill_gradient2('Difference (%)\n',
                       low = '#42AE76', mid = "white", high = '#4292C6',
                       midpoint = 0, limits=c(-0.36*100, 0.60*100)) +
  scale_x_continuous(limits = c(6, 13), breaks = c(6, 8, 10, 12))

############
# PLOT MAP #
############

choropleth_3_mobility_2 <- left_join(choropleth_3_mobility, adm_3_adm_2_phone_leave[ c('origin', 'difference')], by = 
                                       c('adm_3_mobility' = 'origin'))
plot_3_2_map_sim <- ggplot() +
  geom_sf(data = choropleth_3_mobility_2, aes(fill = difference), color= 'black', linewidth = 0.25, alpha = 2) +
  geom_sf(data = choropleth_2, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(legend.position = 'none',
                                     plot.title = element_text(size = 30, hjust = 0.5),
                                     legend.text = element_text(size = 24),
                                     legend.title = element_text(size = 24),
                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#42AE76', mid = "white", high = '#4292C6',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.36, 0.60))

######################
# ADMIN 2 to ADMIN 1 #
######################

# Change admin cross walk to admin 2 level
admin_xwalk_adm_2 <- admin_xwalk |>
  group_by(adm_2, adm_1) |>
  distinct(adm_2, adm_1, .keep_all = FALSE)

# Load mobility data for each data source
adm_3_phone_mobility_long <- readRDS('./out/adm_3_sim_mobility_long.rds')
adm_2_phone_mobility_long <- readRDS('./out/adm_2_sim_mobility_long.rds')
adm_1_phone_mobility_long <- readRDS('./out/adm_1_sim_mobility_long.rds')

# Load population data
adm_3_population_dat <- readRDS('./out/adm_3_population_dat.rds')
adm_2_population_dat <- readRDS('./out/adm_2_population_dat.rds')
adm_1_population_dat <- readRDS('./out/adm_1_population_dat.rds')

# Merge on origin
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, admin_xwalk_adm_2[, c(1, 2)],
                                       by = c('origin' = 'adm_2'))

# Merge on destination
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, admin_xwalk_adm_2[, c(1, 2)], 
                                       by = c('destination' = 'adm_2'))

# Calculate out of province travel at the admin 2 unit
adm_2_adm_1_phone <- adm_2_phone_mobility_long |>
  group_by(origin, adm_1.x, adm_1.y) |>
  mutate(adm_2_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_1.x, adm_1.y, adm_2_sum, .keep_all = FALSE) |>
  dplyr::rename('adm_1_origin' = 'adm_1.x',
                'adm_1_destination' = 'adm_1.y')

# Merge on origin from admin 1 and travel probability
adm_2_adm_1_phone <- left_join(adm_2_adm_1_phone, 
                               adm_1_phone_mobility_long, 
                               by = c('adm_1_origin' = 'origin',
                                      'adm_1_destination' = 'destination'))

# Calculate 2 - stays to get the leave probability and difference between units
adm_2_adm_1_phone_leave <- adm_2_adm_1_phone |>
  dplyr::filter(adm_1_origin == adm_1_destination) |>
  mutate(adm_2_leave = 1 - adm_2_sum,
         adm_1_leave = 1 - value,
         difference = adm_2_leave - adm_1_leave)

# Merge on admin 2 population
adm_2_adm_1_phone_leave <- left_join(adm_2_adm_1_phone_leave, adm_2_population_dat[, c(1, 3)], 
                                     by = c('origin' = 'adm_2'))

################
# PLOT SCATTER #
################

# Create scatter plot by population
plot_2_1_scatter_sim <- ggplot(adm_2_adm_1_phone_leave) +
  geom_point(aes(x = log(population_2020_adm_2), y = difference*100, 
                 fill = difference*100), color = '#565656', alpha = 0.85, size = 8, shape = 21) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 1) +
  geom_smooth(aes(x = log(population_2020_adm_2), y = difference*100), color = '#565656', 
              method = "loess", se = TRUE, linewidth = 3, alpha = 0.25) + 
  ylim(-0.36*100, 0.60*100) +
  ggtitle('Difference in Province Leave Probability \n(District - Province)') +
  xlab('Log Population (District)') + 
  ylab('Leave Probability Difference (%)') +
  theme_minimal() +
  theme(
    axis.title = element_text(size=26),
    axis.text = element_text(size=26),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24),
    legend.key.height = unit(3.3, 'cm'),
    legend.key.width = unit(1.4, 'cm'),
    plot.title = element_text(size=30, hjust = 0.5)) +
  scale_fill_gradient2('Difference (%)\n',
                       low = '#42AE76', mid = "white", high = '#4292C6',
                       midpoint = 0, limits=c(-0.36*100, 0.60*100)) +
  scale_x_continuous(limits = c(10, 15), breaks = c(10, 12, 14))

############
# PLOT MAP #
############

choropleth_2_2 <- left_join(choropleth_2, adm_2_adm_1_phone_leave[ c('origin', 'difference')], by = 
                            c('ADM2_EN' = 'origin'))
plot_2_1_map_sim <- ggplot() +
  geom_sf(data = choropleth_2_2, aes(fill = difference), color= 'black', linewidth = 0.25, alpha = 2) +
  geom_sf(data = choropleth_1, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(legend.position = 'none',
                                     plot.title = element_text(size = 30, hjust = 0.5),
                                     legend.text = element_text(size = 24),
                                     legend.title = element_text(size = 24),
                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#42AE76', mid = "white", high = '#4292C6',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.36, 0.60))





##########################
# 5. CREATE FINAL FIGURE #
##########################

col_1 <- cowplot::plot_grid(plot_3_2_scatter,
                            ggplot() + theme_void(), 
                            plot_3_2_map,
                            nrow = 1, labels = c('(a)', '', ''),
                            label_size = 34, hjust = 0,
                            rel_widths = c(1.8, 0.2, 1))


col_2 <- cowplot::plot_grid(plot_3_1_scatter,
                            ggplot() + theme_void(), 
                            plot_3_1_map,
                            nrow = 1, labels = c('(b)', '', ''),
                            label_size = 34, hjust = 0,
                            rel_widths = c(1.8, 0.2, 1))

col_3 <- cowplot::plot_grid(plot_2_1_scatter,
                            ggplot() + theme_void(), 
                            plot_2_1_map,
                            nrow = 1, labels = c('(c)', '', ''),
                            label_size = 34, hjust = 0,
                            rel_widths = c(1.8, 0.2, 1))


col_4 <- cowplot::plot_grid(plot_3_2_scatter_sim,
                            ggplot() + theme_void(), 
                            plot_3_2_map_sim,
                            nrow = 1, labels = c('(d)', '', ''),
                            label_size = 34, hjust = 0,
                            rel_widths = c(1.8, 0.2, 1))


col_5 <- cowplot::plot_grid(plot_3_1_scatter_sim,
                            ggplot() + theme_void(), 
                            plot_3_1_map_sim,
                            nrow = 1, labels = c('(e)', '', ''),
                            label_size = 34, hjust = 0,
                            rel_widths = c(1.8, 0.2, 1))

col_6 <- cowplot::plot_grid(plot_2_1_scatter_sim,
                            ggplot() + theme_void(), 
                            plot_2_1_map_sim,
                            nrow = 1, labels = c('(f)', '', ''),
                            label_size = 34, hjust = 0,
                            rel_widths = c(1.8, 0.2, 1))

figure_3 <- cowplot::plot_grid(ggplot() + theme_void(), ggplot() + theme_void(), 
                               col_1, col_4, 
                               ggplot() + theme_void(), ggplot() + theme_void(),
                               col_2, col_5, 
                               ggplot() + theme_void(), ggplot() + theme_void(),
                               col_3, col_6,
                               nrow = 6, rel_heights = c(0.1, 1, 0.1, 1, 0.1, 1),
                               labels = c('Observed Data: Division - District', 'Simulated Data: Division - District', '', '',
                                          'Observed Data: Division - Province', 'Simulated Data: Division - Province', '', '',
                                          'Observed Data: District - Province', 'Simulated Data: District - Province', '', ''),
                               label_size = 30, hjust = 0)                            
# Save plot
ggsave('./figs/figure_3_supp_new.jpg', plot = figure_3, height = 25, width = 35)


# TEXT CALL OUTS

mean(adm_3_adm_1_phone_leave$difference)
min(adm_3_adm_1_phone_leave$difference)
max(adm_3_adm_1_phone_leave$difference)


mean(adm_3_adm_2_phone_leave$difference)
min(adm_3_adm_2_phone_leave$difference)
max(adm_3_adm_2_phone_leave$difference)

mean(adm_2_adm_1_phone_leave$difference)
min(adm_2_adm_1_phone_leave$difference)
max(adm_2_adm_1_phone_leave$difference)



################################################################################
################################################################################

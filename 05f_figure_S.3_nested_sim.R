################################################################################
# File Name: 05f_figure_S.3_nested_sim                                         #
#                                                                              #
# Purpose:   Create figure S.3 for the manuscript.                             #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure functions                                        #
#            3. Create external subfigures                                     #
#            4. Create final figure                                            #
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

##############################
# 2. CREATE FIGURE FUNCTIONS #
##############################

# Plot functions
# Make a simple admin level map
make_simple_map <- function(data, coord_data) {
  map <- ggplot(data = data) +
    geom_sf(aes(), color= 'black', fill = 'white', linewidth = 0.20) +
    geom_point(data = coord_data, aes(x = long, y = lat), colour="white", fill = 'black', size=2.15, alpha = 1, shape=21) +
    geom_text(data = coord_data, aes(x = long - 0.10 , y = lat + 0.04 , label = city), size = 3.5, fontface = 'bold') +
    theme_void() + ggtitle(' ') + theme(legend.position = 'none',
                                        plot.title = element_text(size = 30, hjust = 0.5),
                                        legend.text = element_text(size = 24),
                                        legend.title = element_text(size = 24)) +
    coord_sf()
  return(map)
}

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

#################################
# 3. CREATE EXTERNAL SUBFIGURES #
#################################

# Load mobility data for each data source
load('./tmp/fmt_adm_3_sim_mobility_dat.RData')
load('./tmp/fmt_adm_2_sim_mobility_dat.RData')
load('./tmp/fmt_adm_1_sim_mobility_dat.RData')

# Load population data
load('./tmp/adm_population_dat.RData')

# Load admin crosswalk
admin_xwalk <- read_csv('./tmp/admin_xwalk.csv')

######################
# ADMIN 3 to ADMIN 2 #
######################

###############################
# SIMULATED MOBILE PHONE DATA #
###############################

# Merge on destination
adm_3_sim_mobility_long <- left_join(adm_3_sim_mobility_long, admin_xwalk[, c(1, 2)], 
                                     by = c('destination' = 'adm_3'))
# Merge on admin 2 origin
adm_3_sim_mobility_long <- left_join(adm_3_sim_mobility_long, admin_xwalk[, c(1, 2)],
                                     by = c('origin' = 'adm_3'))

# Calculate out of district travel at the admin 3 unit
adm_3_adm_2_sim <- adm_3_sim_mobility_long |>
  # Filter to trips leaving the district
  filter(adm_2.x != adm_2.y) |>
  group_by(origin) |>
  # Calculate the external district travel proportions
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_2.y, adm_3_sum, .keep_all = FALSE) |>
  dplyr::rename('adm_2_origin' = 'adm_2.y')

# Merge on origin from admin 2 and internal travel proportion
adm_3_adm_2_sim <- left_join(adm_3_adm_2_sim, 
                             adm_2_sim_mobility_long[
                               adm_2_sim_mobility_long$origin == 
                                 adm_2_sim_mobility_long$destination, ], 
                             by = c('adm_2_origin' = 'origin'))

# Calculate 1 - stays to convert to an external travel proportion
adm_3_adm_2_sim$adm_2_value <- 1 - adm_3_adm_2_sim$value

# Calculate difference between nested units
adm_3_adm_2_sim$diff <- adm_3_adm_2_sim$adm_2_value - adm_3_adm_2_sim$adm_3_sum

# Merge on admin 3 population
adm_3_adm_2_sim <- left_join(adm_3_adm_2_sim, adm_3_population_dat[, c(1, 4)], 
                             by = c('origin' = 'adm_3_mobility'))

################
# PLOT SCATTER #
################

# Create scatter plot by population
plot_3_2_scatter <- ggplot(adm_3_adm_2_sim) +
  geom_point(aes(x = log(population_2020_adm_3), y = diff, 
                 fill = diff), color = '#565656', alpha = 0.5, size = 5, shape = 21) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = log(population_2020_adm_3), y = diff), color = '#565656', 
              method = "lm", se = FALSE, linewidth = 3.5, alpha = 9) + 
  ylim(-0.75, 0.28) +
  xlab('Log Population (Division)') + 
  ylab('External District Trip\nProportion Difference') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 24),
        legend.key.height = unit(1.5, 'cm'),
        plot.title = element_text(size=30, hjust = 0.5)) +
  scale_fill_gradient2('Difference\n',
                       low = '#41AE76', mid = "white", high = '#807DBA',
                       midpoint = 0, limits=c(-0.75, 0.28))

############
# PLOT MAP #
############

choropleth_3_mobility <- left_join(choropleth_3_mobility, adm_3_adm_2_sim[ c('origin', 'diff')], by = 
                                     c('adm_3_mobility' = 'origin'))
plot_3_2_map <- ggplot() +
  geom_sf(data = choropleth_3_mobility, aes(fill = diff), color= 'black', linewidth = 0.25, alpha = 1) +
  geom_sf(data = choropleth_2, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(legend.position = 'none',
                                     plot.title = element_text(size = 30, hjust = 0.5),
                                     legend.text = element_text(size = 20),
                                     legend.title = element_text(size = 24),
                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#41AE76', mid = "white", high = '#807DBA',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.75, 0.28))

######################
# ADMIN 3 to ADMIN 1 #
######################

###############################
# SIMULATED MOBILE PHONE DATA #
###############################

# Merge on destination
adm_3_sim_mobility_long <- left_join(adm_3_sim_mobility_long, admin_xwalk[, c(1, 3)], 
                                     by = c('destination' = 'adm_3'))
# Merge on origin
adm_3_sim_mobility_long <- left_join(adm_3_sim_mobility_long, admin_xwalk[, c(1, 3)],
                                     by = c('origin' = 'adm_3'))

# Calculate out of province travel at the admin 3 unit
adm_3_adm_1_sim <- adm_3_sim_mobility_long |>
  filter(adm_1.x != adm_1.y) |>
  group_by(origin) |>
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_1.y, adm_3_sum, .keep_all = FALSE) |>
  dplyr::rename('adm_1_origin' = 'adm_1.y')

# Merge on origin from admin 1 and internal travel proportion
adm_3_adm_1_sim <- left_join(adm_3_adm_1_sim, 
                             adm_1_sim_mobility_long[
                               adm_1_sim_mobility_long$origin == 
                                 adm_1_sim_mobility_long$destination, ], 
                             by = c('adm_1_origin' = 'origin'))

# Calculate 1 - stays to get external proportion
adm_3_adm_1_sim$adm_1_value <- 1 - adm_3_adm_1_sim$value

# Calculate difference between nested units
adm_3_adm_1_sim$diff <- adm_3_adm_1_sim$adm_1_value - adm_3_adm_1_sim$adm_3_sum

# Load population data
# Merge on admin 3 population
adm_3_adm_1_sim <- left_join(adm_3_adm_1_sim, adm_3_population_dat[, c(1, 4)], 
                             by = c('origin' = 'adm_3_mobility'))

################
# PLOT SCATTER #
################

# Create scatter plot by population
plot_3_1_scatter <- ggplot(adm_3_adm_1_sim) +
  geom_point(aes(x = log(population_2020_adm_3), y = diff, 
                 fill = diff), color = '#565656', alpha = 0.5, size = 5, shape = 21) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = log(population_2020_adm_3), y = diff), color = '#565656', 
              method = "lm", se = FALSE, linewidth = 3.5, alpha = 9) + 
  ylim(-0.75, 0.28) +
  xlab('Log Population (Division)') + 
  ylab('External Province Trip\nProportion Difference') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 24),
        legend.key.height = unit(1.5, 'cm'),
        plot.title = element_text(size=30, hjust = 0.5)) +
  scale_fill_gradient2('Difference\n',
                       low = '#41AE76', mid = "white", high = '#807DBA',
                       midpoint = 0, limits=c(-0.75, 0.28))
############
# PLOT MAP #
############

choropleth_3_mobility <- left_join(choropleth_3_mobility, adm_3_adm_1_sim[ c('origin', 'diff')], by = 
                                     c('adm_3_mobility' = 'origin'))
plot_3_1_map <- ggplot() +
  geom_sf(data = choropleth_3_mobility, aes(fill = diff.y), color= 'black', linewidth = 0.25, alpha = 1) +
  geom_sf(data = choropleth_1, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(legend.position = 'none',
                                     plot.title = element_text(size = 30, hjust = 0.5),
                                     legend.text = element_text(size = 20),
                                     legend.title = element_text(size = 24),
                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#41AE76', mid = "white", high = '#807DBA',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.75, 0.28))

######################
# ADMIN 2 to ADMIN 1 #
######################

###############################
# SIMULATED MOBILE PHONE DATA #
###############################

# Change admin cross walk to admin 2 level
admin_xwalk_adm_2 <- admin_xwalk |>
  group_by(adm_2, adm_1) |>
  distinct(adm_2, adm_1, .keep_all = FALSE)

# Merge on destination
adm_2_sim_mobility_long <- left_join(adm_2_sim_mobility_long, admin_xwalk_adm_2, 
                                     by = c('destination' = 'adm_2'))
# Merge on origin
adm_2_sim_mobility_long <- left_join(adm_2_sim_mobility_long, admin_xwalk_adm_2,
                                     by = c('origin' = 'adm_2'))

# Calculate out of province travel at the admin 2 unit
adm_2_adm_1_sim <- adm_2_sim_mobility_long |>
  filter(adm_1.x != adm_1.y) |>
  group_by(origin) |>
  mutate(adm_2_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_1.y, adm_2_sum, .keep_all = FALSE) |>
  dplyr::rename('adm_1_origin' = 'adm_1.y')

# Merge on origin from admin 1 to get internal proportion
adm_2_adm_1_sim <- left_join(adm_2_adm_1_sim, 
                             adm_1_sim_mobility_long[
                               adm_1_sim_mobility_long$origin == 
                                 adm_1_sim_mobility_long$destination, ], 
                             by = c('adm_1_origin' = 'origin'))

# Calculate 1 - stays to get external proportion
adm_2_adm_1_sim$adm_1_value <- 1 - adm_2_adm_1_sim$value

# Calculate difference between nested units
adm_2_adm_1_sim$diff <- adm_2_adm_1_sim$adm_1_value - adm_2_adm_1_sim$adm_2_sum

# Load population data
# Merge on admin 2 population
adm_2_adm_1_sim <- left_join(adm_2_adm_1_sim, adm_2_population_dat[, c(1, 3)], 
                             by = c('origin' = 'adm_2'))

################
# PLOT SCATTER #
################

# Create scatter plot by population
plot_2_1_scatter <- ggplot(adm_2_adm_1_sim) +
  geom_point(aes(x = log(population_2020_adm_2), y = diff, 
                 fill = diff), color = '#565656', alpha = 0.5, size = 5, shape = 21) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = log(population_2020_adm_2), y = diff), color = '#565656', 
              method = "lm", se = FALSE, linewidth = 3.5, alpha = 9) + 
  ylim(-0.75, 0.28) +
  xlab('Log Population (District)') + 
  ylab('External Province Trip\nProportion Difference') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 24),
        legend.key.height = unit(1.5, 'cm'),
        plot.title = element_text(size=30, hjust = 0.5)) +
  scale_fill_gradient2('Difference\n',
                       low = '#41AE76', mid = "white", high = '#807DBA',
                       midpoint = 0, limits=c(-0.75, 0.28))

############
# PLOT MAP #
############

choropleth_2 <- left_join(choropleth_2, adm_2_adm_1_sim[ c('origin', 'diff')], by = 
                            c('ADM2_EN' = 'origin'))
plot_2_1_map <- ggplot() +
  geom_sf(data = choropleth_2, aes(fill = diff), color= 'black', linewidth = 0.25, alpha = 1) +
  geom_sf(data = choropleth_1, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(legend.position = 'none',
                                     plot.title = element_text(size = 30, hjust = 0.5),
                                     legend.title = element_text(size = 24),
                                     legend.text = element_text(size = 20),
                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#41AE76', mid = "white", high = '#807DBA',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.75, 0.28))

##########################
# 4. CREATE FINAL FIGURE #
##########################

col_2 <- cowplot::plot_grid(ggplot() + theme_void(), plot_2_1_scatter,
                            ggplot() + theme_void(), plot_3_1_scatter,
                            ggplot() + theme_void(), plot_3_2_scatter,
                            nrow = 6, labels = c('Province - District', '(a)', 
                                                 'Province - Division', '(b)', 
                                                 'District - Division', '(c)'),
                            label_size = 34, hjust = 0,
                            rel_heights = c(0.06, 1, 0.06, 1, 0.06, 1))

col_3 <- cowplot::plot_grid(ggplot() + theme_void(), plot_2_1_map,
                            ggplot() + theme_void(), plot_3_1_map,
                            ggplot() + theme_void(), plot_3_2_map, 
                            nrow = 6, labels = c('', '', 
                                                 '', '', 
                                                 '', ''),
                            label_size = 34, hjust = 0,
                            rel_heights = c(0.06, 1, 0.06, 1, 0.06, 1))

figure_3 <- cowplot::plot_grid(
                               col_2,
                               ggplot() + theme_void(),
                               col_3,
                               nrow = 1, rel_widths = c(1, 0.3, 0.8),
                               labels = c('', '', ''),
                               label_size = 26, hjust = 0)                            

ggsave('./figs/figure_S.3_nested_sim.jpg', plot = figure_3, height = 25, width = 18)

################################################################################
################################################################################

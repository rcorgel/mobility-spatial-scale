################################################################################
# File Name: 05e_figure_3_nested_sim                                           #
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
adm_3_phone_mobility_long <- readRDS('./out/adm_3_sim_mobility_long.rds')
adm_2_phone_mobility_long <- readRDS('./out/adm_2_sim_mobility_long.rds')
adm_1_phone_mobility_long <- readRDS('./out/adm_1_sim_mobility_long.rds')
adm_3_sim_mobility_long <- readRDS('./out/adm_3_sim_mobility_long.rds')
adm_2_sim_mobility_long <- readRDS('./out/adm_2_sim_mobility_long.rds')
adm_1_sim_mobility_long <- readRDS('./out/adm_1_sim_mobility_long.rds')

# Load population data
adm_3_population_dat <- readRDS('./out/adm_3_population_dat.rds')
adm_2_population_dat <- readRDS('./out/adm_2_population_dat.rds')
adm_1_population_dat <- readRDS('./out/adm_1_population_dat.rds')

#################
# UNEVEN TRAVEL #
#################

# Merge on destination
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 3)], 
                                       by = c('destination' = 'adm_3'))
# Merge on origin
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 3)],
                                       by = c('origin' = 'adm_3'))

# Probability of province travel at the division level
adm_3_example_odd <- adm_3_phone_mobility_long |> filter(origin == 'Sevanagala') |>
  group_by(adm_1.x, adm_1.y) |>
  mutate(value_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_1.x, value_sum) |>
  dplyr::rename('destination' = 'adm_1.x') |>
  ungroup() |>
  select(-c(adm_1.y))

# Probability of province travel at the province level
adm_1_example_odd  <- adm_1_phone_mobility_long |> filter(origin == 'Uva') |>
  group_by(origin, destination) |>
  mutate(value_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, destination, value_sum)

# Combine
network_odd <- rbind(adm_3_example_odd, adm_1_example_odd)

# Calculate the outgoing proportion
network_odd_prop <- network_odd |>
  dplyr::filter(destination != 'Uva') |>
  group_by(origin) |>
  mutate(proportion = value_sum / sum(value_sum)) |>
  distinct(origin, destination, proportion)

# Add outgoing propotion to map
choropleth_1_3_prop_odd <- left_join(choropleth_1, network_odd_prop[network_odd_prop$origin == "Sevanagala",], by = 
                                       c('ADM1_EN' = 'destination'))
# Create map
plot_1_3_map_prop_odd <- ggplot() +
  geom_sf(data = choropleth_1_3_prop_odd, aes(fill = proportion), color= 'black', linewidth = 0.25, alpha = 1) +
  theme_void() + ggtitle('\nOutgoing Travel\nSevanagala Division') + theme(legend.position = 'right',
                                                                           plot.title = element_text(size = 30, hjust = 0.5),
                                                                           legend.text = element_text(size = 24),
                                                                           legend.title = element_text(size = 28),
                                                                           panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() +
  scale_colour_gradient('Proportion',
                        low = 'white', high = '#41AE76',aesthetics = 'fill', limits=c(0, 0.65),
                        breaks = c(0, 0.30, 0.60),
                        labels = function(x) sprintf("%.2f", x))

# Add outgoing proportion to map
choropleth_1_1_prop_odd <- left_join(choropleth_1, network_odd_prop[network_odd_prop$origin == "Uva",], by = 
                                       c('ADM1_EN' = 'destination'))
# Create map
plot_1_1_map_prop_odd <- ggplot() +
  geom_sf(data = choropleth_1_1_prop_odd, aes(fill = proportion), color= 'black', linewidth = 0.25, alpha = 1) +
  theme_void() + ggtitle('\nOutgoing Travel\nUva Province') + theme(legend.position = 'right',
                                                                    plot.title = element_text(size = 30, hjust = 0.5),
                                                                    legend.text = element_text(size = 24),
                                                                    legend.title = element_text(size = 28),
                                                                    panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient('Proportion',
                        low = 'white', high = '#4292C6',aesthetics = 'fill', limits=c(0, 0.65),
                        breaks = c(0, 0.30, 0.60),
                        labels = function(x) sprintf("%.2f", x))

# Create bar plot
net_odd <- ggplot(data=network_odd, aes(x=destination, y=value_sum * 100, fill = origin)) + 
  geom_bar(stat="identity", position="dodge", width = 0.5, color = 'black', alpha = 0.8) + theme_minimal() +
  scale_fill_manual('Origin Unit', values = c('#41AE76', '#4292C6')) + 
  ylab('Travel Probability (%)') + xlab("Destination Province") + ggtitle('\nTravel from Sevanagala Division vs. Uva Province') +
  theme(legend.position = 'right', plot.title = element_text(size = 30, hjust = 0.5),
        axis.text.y = element_text(color = 'black', size = 26), 
        axis.text.x = element_text(size = 28, angle = 30, hjust = 1, vjust = 1, color = 'black'),
        axis.title = element_text(size = 28),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 24),
        legend.key.size = unit(1.2, 'cm')) +
  scale_y_continuous(limits = c(0, 100),                
                     breaks = c(0, 25, 50, 75, 100)) 

###############
# EVEN TRAVEL #
###############

# Probability of province travel at the division level
adm_3_example_even <- adm_3_phone_mobility_long |> filter(origin == 'Colombo') |>
  group_by(adm_1.x, adm_1.y) |>
  mutate(value_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, adm_1.x, value_sum) |>
  dplyr::rename(destination = adm_1.x) |>
  ungroup() |>
  select(-c(adm_1.y))

# Probability of province travel at the province level
adm_1_example_even <- adm_1_phone_mobility_long |> filter(origin == 'Western') |>
  group_by(origin, destination) |>
  mutate(value_sum = sum(value, na.rm = TRUE)) |>
  distinct(origin, destination, value_sum)

# Combine
network_even <- rbind(adm_3_example_even, adm_1_example_even)

# Calculate the outgoing proportion
network_even_prop <- network_even |>
  dplyr::filter(destination != 'Western') |>
  group_by(origin) |>
  mutate(proportion = value_sum / sum(value_sum)) |>
  distinct(origin, destination, proportion)

# Add outgoing propotion to map
choropleth_1_3_prop_even <- left_join(choropleth_1, network_even_prop[network_even_prop$origin == "Colombo",], by = 
                                        c('ADM1_EN' = 'destination'))
# Map
plot_1_3_map_prop_even <- ggplot() +
  geom_sf(data = choropleth_1_3_prop_even, aes(fill = proportion), color= 'black', linewidth = 0.25, alpha = 1) +
  theme_void() + ggtitle('\nOutgoing Travel\nColombo Division') + theme(legend.position = 'right',
                                                                        plot.title = element_text(size = 30, hjust = 0.5),
                                                                        legend.text = element_text(size = 24),
                                                                        legend.title = element_text(size = 28),
                                                                        panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient('Proportion',
                        low = 'white', high = '#41AE76',aesthetics = 'fill', limits=c(0, 0.65),
                        breaks = c(0, 0.30, 0.60))

# Add outgoing propotion to map
choropleth_1_1_prop_even <- left_join(choropleth_1, network_even_prop[network_even_prop$origin == "Western",], by = 
                                        c('ADM1_EN' = 'destination'))
# Map
plot_1_1_map_prop_even <- ggplot() +
  geom_sf(data = choropleth_1_1_prop_even, aes(fill = proportion), color= 'black', linewidth = 0.25, alpha = 1) +
  theme_void() + ggtitle('\nOutgoing Travel\nWestern Province') + theme(legend.position = 'right',
                                                                        plot.title = element_text(size = 30, hjust = 0.5),
                                                                        legend.text = element_text(size = 24),
                                                                        legend.title = element_text(size = 28),
                                                                        panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient('Proportion',
                        low = 'white', high = '#4292C6',aesthetics = 'fill', limits=c(0, 0.65),
                        breaks = c(0, 0.30, 0.60))

# Create bar plot
net_even <- ggplot(data=network_even, aes(x=destination, y=value_sum * 100, fill = origin)) + 
  geom_bar(stat="identity", position="dodge", width = 0.5, color = 'black', alpha = 0.8) + theme_minimal() +
  scale_fill_manual('Origin Unit', values = c( '#41AE76', '#4292C6')) + 
  ylab('Travel Probability (%)') + xlab("Destination Province") + ggtitle('\nTravel from Colombo Division vs. Western Province') +
  theme(legend.position = 'right', plot.title = element_text(size = 30, hjust = 0.5),
        axis.text.y = element_text(color = 'black', size = 26), 
        axis.text.x = element_text(size = 28, angle = 30, hjust = 1, vjust = 1, color = 'black'),
        axis.title = element_text(size = 28),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 24),
        legend.key.size = unit(1.2, 'cm'))  +
  scale_y_continuous(limits = c(0, 100),                
                     breaks = c(0, 25, 50, 75, 100)) 

######################
# ADMIN 3 to ADMIN 1 #
######################

# Load mobility data for each data source
adm_3_phone_mobility_long <- readRDS('./out/adm_3_sim_mobility_long.rds')
adm_1_phone_mobility_long <- readRDS('./out/adm_1_sim_mobility_long.rds')

# Load population data
adm_3_population_dat <- readRDS('./out/adm_3_population_dat.rds')
adm_1_population_dat <- readRDS('./out/adm_1_population_dat.rds')

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
              method = "loess", se = TRUE, linewidth = 4, alpha = 0.25) + 
  ylim(-0.30*100, 0.60*100) +
  ggtitle('\nDifference in Province Leave Probability\n(Division - Province)') +
  xlab('Log Population (Division)') + 
  ylab('Leave Probability Difference (%)') +
  theme_minimal() +
  theme(
    axis.title = element_text(size=28),
    axis.text = element_text(size=26),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 28),
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

choropleth_3_mobility <- left_join(choropleth_3_mobility, adm_3_adm_1_phone_leave[ c('origin', 'difference')], by = 
                                     c('adm_3_mobility' = 'origin'))
plot_3_1_map <- ggplot() +
  geom_sf(data = choropleth_3_mobility, aes(fill = difference), color= 'black', linewidth = 0.25, alpha = 1) +
  geom_sf(data = choropleth_1, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(legend.position = 'none',
                                     plot.title = element_text(size = 30, hjust = 0.5),
                                     legend.text = element_text(size = 24),
                                     legend.title = element_text(size = 24),
                                     panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#41AE76', mid = "white", high = '#4292C6',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.30, 0.60))

##########################
# 4. CREATE FINAL FIGURE #
##########################

col_1 <- cowplot::plot_grid(net_even, plot_1_3_map_prop_even, plot_1_1_map_prop_even,
                            nrow = 1, labels = c('(a)', '(b)', ''),
                            label_size = 34, hjust = 0,
                            rel_widths = c(1.5, 0.75, 0.75))

col_2 <- cowplot::plot_grid(net_odd, plot_1_3_map_prop_odd, plot_1_1_map_prop_odd,
                            nrow = 1, labels = c('(c)', '(d)', ''),
                            label_size = 34, hjust = 0,
                            rel_widths = c(1.5, 0.75, 0.75))

col_3 <- cowplot::plot_grid(plot_3_1_scatter,
                            ggplot() + theme_void(), 
                            plot_3_1_map,
                            nrow = 1, labels = c('(e)', '', ''),
                            label_size = 34, hjust = 0,
                            rel_widths = c(1.8, 0.2, 1))

figure_3 <- cowplot::plot_grid(col_1, ggplot() + theme_void(),
                               col_2, ggplot() + theme_void(),
                               col_3,
                               nrow = 5, rel_heights = c(0.9, 0.05, 0.9, 0.05, 1.2),
                               labels = c('', '', ''),
                               label_size = 26, hjust = 0)                            
# Save plot
ggsave('./figs/figure_3_nested_new_sim.jpg', plot = figure_3, height = 25, width = 25)

################################################################################
################################################################################

# At the province level, the probability of remaining in the Western province was 93%. 
network_even[network_even$origin == 'Western' & network_even$destination == 'Western',]$value_sum

# Meanwhile, the probability of remaining in the Western province when originating 
# the Colombo division (nested within the Western province) was also 93%
network_even[network_even$origin == 'Colombo' & network_even$destination == 'Western',]$value_sum

# In the Sevanagala division nested within the Uva province, there was a much lower probability 
# of remaining within the Uva province than for someone in Uva province on average (45% vs. 96%)
network_odd[network_odd$origin == 'Sevanagala' & network_odd$destination == 'Uva',]$value_sum
network_odd[network_odd$origin == 'Uva' & network_odd$destination == 'Uva',]$value_sum

# After comparing this difference across all nested province-division pairs, we found that 
# divisions on average were more likely to have a higher travel probability (6%, ranging from -16% to 64%)
mean(adm_3_adm_1_phone_leave$difference)
min(adm_3_adm_1_phone_leave$difference)
max(adm_3_adm_1_phone_leave$difference)
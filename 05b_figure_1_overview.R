################################################################################
# File Name: 05b_figure_1_overview                                             #
#                                                                              #
# Purpose:   Create figure 1 for the manuscript.                               #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create individual level map                                    #
#            3. Create subnational level map                                   #
#            4. Create national level map                                      #
#            5. Create epidemic curves at different scales                     #
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
library(osmdata)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load metapopulation model functions
source('./mobility-spatial-scale/04_metapop_model.R')

##################################
# 2. CREATE INVIDIVUAL LEVEL MAP #
##################################

# Set the center of the map
kandy <- matrix(data = c(80.62463, 80.64857, 7.28427, 7.30483),
                    nrow = 2,
                    byrow = TRUE)
# Update column and row names
colnames(kandy) <- c("min", "max")
rownames(kandy) <- c("x", "y")
# Print the matrix to confirm
kandy

# Obtain map layers
# Major roads
kandy_major <- getbb("Kandy") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = c("highway")) %>%
  osmdata_sf()

# Rail
kandy_rail <- getbb("Kandy") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = c("railway")) %>%
  osmdata_sf()

# Buildings
kandy_place <- getbb("Kandy") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = c("building")) %>%
  osmdata_sf()

# Landscape
kandy_natural <- getbb("Kandy") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = "natural") %>%
  osmdata_sf()

# Combine all layers
street_plot <- ggplot() +
  geom_sf(data = kandy_major$osm_lines,
          color = "black",
          size = 0.15) + 
  geom_sf(data = kandy_rail$osm_lines,
          color = "black",
          size = 0.15) + 
  geom_sf(data = kandy_natural$osm_polygons,
          fill = "#DEF4FC",
          color = "black",
          size = 0.05) + 
  geom_sf(data = kandy_place$osm_polygons,
          fill = "white",
          color = "black",
          size = 0.02) + 
  coord_sf(xlim = c(80.63, 80.64), ylim = c(7.29, 7.30)) + theme_void() + 
  theme(plot.background = element_rect(fill = "#ABDDA4")) 

# Save
ggsave('./figs/individual_level.jpg', plot = street_plot, height = 16, width = 16)

###################################
# 3. CREATE SUBNATIONAL LEVEL MAP #
###################################

# Create map function
# Make a simple admin level map
make_simple_map <- function(data, coord_data) {
  map <- ggplot(data = data) +
    geom_sf(aes(), color= 'black', fill = 'white', linewidth = 0.20) +
    geom_point(data = coord_data, aes(x = long, y = lat), colour="white", 
               fill = 'black', size=2.15, alpha = 1, shape=21) +
    geom_text(data = coord_data, aes(x = long - 0.10 , y = lat + 0.04 , label = city), 
              size = 3.5, fontface = 'bold') +
    theme_void() + ggtitle(' ') + theme(legend.position = 'none',
                                        plot.title = element_text(size = 30, hjust = 0.5),
                                        legend.text = element_text(size = 24),
                                        legend.title = element_text(size = 24)) +
    coord_sf()
  return(map)
}

# Add coordinates of highlight cities
coordinate_cities <- data.frame(
  city = c("Colombo", "Madhu"),
  lat = c(6.927632561772342, 8.85653415340985),
  long = c(79.85843709788357, 80.20433649099449))   

# Load the map shape files
# From: https://data.humdata.org/dataset/cod-ab-mdg
choropleth_0 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm0_slsd_20220816')
choropleth_1 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm1_slsd_20220816')
choropleth_2 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm2_slsd_20220816')
choropleth_3 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm3_slsd_20220816')
choropleth_4 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm4_slsd_20220816')

# Load mobility to shape cross walk
# The mobility data combines multiple admin 3 units, changing the total from 339 to 330
mobility_shape_xwalk <- readRDS('./tmp/mobility_shape_xwalk.rds')

# Merge on the cross walk
choropleth_3 <- left_join(choropleth_3, mobility_shape_xwalk, 
                          by = c('ADM3_EN' = 'adm_3_shape'))

# Join polygons to create 330 mobility admin 3 units
choropleth_3_mobility <- choropleth_3 %>% 
  group_by(adm_3_mobility) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# Assign values for subnational map colors
choropleth_2$color <- 'A'
choropleth_2$color[4] <- 'B'
choropleth_2$color[5] <- 'C'
choropleth_2$color[6] <- 'D'

# Add map layers for major roads and rail
sl_major <- getbb("Sri Lanka") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = c("highway")) %>%
  osmdata_sf()

sl_rail <- getbb("Sri Lanka") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = c("railway")) %>%
  osmdata_sf()

# Create map
map <- ggplot() +
  geom_sf(data = choropleth_0, aes(group = ADM0_EN), fill = '#DDBAA4', 
          color= 'black', linewidth = 1.3, alpha = 0.85) +
  geom_sf(data = choropleth_2[c(4, 5, 6),], aes(fill = color, group = ADM2_EN), 
          linewidth = 1.3, color= 'black') +
  geom_sf(data = sl_major$osm_lines,
          color = "black",
          size = 0.005,
          alpha = 0.1) + 
  theme_void() + theme(legend.position = "none") +
  scale_fill_manual(values = c("#ABDDA4", "#A4C7DD","#D6A4DD")) + 
  coord_sf(ylim = c(6.78, 7.98), xlim = c(80.12, 81.32), expand = TRUE)
 
# Save
ggsave('./figs/subnational_level.jpg', plot = map, height = 16, width = 13)

################################
# 4. CREATE NATIONAL LEVEL MAP #
################################

# Create full map
map <- ggplot() +
  geom_sf(data = choropleth_0, aes(group = ADM0_EN), fill = '#DDBAA4', 
          color= 'black', linewidth = 1.0, alpha = 0.85) +
  geom_sf(data = sl_major$osm_lines,
          color = "black",
          size = 0.005,
          alpha = 0.1) + 
  theme_void() + theme(legend.position = "none") +
  scale_fill_manual(values = c("#ABDDA4", "#A4C7DD","#D6A4DD")) 

# Save
ggsave('./figs/national_level.jpg', plot = map, height = 16, width = 13)

# Create full map w/ subnational level
map <- ggplot() +
  geom_sf(data = choropleth_0, aes(group = ADM0_EN), fill = '#DDBAA4', 
          color= 'black', linewidth = 1.3, alpha = 0.85) +
  geom_sf(data = choropleth_2[c(4, 5, 6),], aes(fill = color, group = ADM2_EN), 
          linewidth = 1.3, color= 'black') +
  geom_sf(data = sl_major$osm_lines,
          color = "black",
          size = 0.005,
          alpha = 0.1) + 
  theme_void() + theme(legend.position = "none") +
  scale_fill_manual(values = c("#ABDDA4", "#A4C7DD","#D6A4DD")) 

# Save
ggsave('./figs/subnational_level_full.jpg', plot = map, height = 16, width = 13)

#################################################
# 4. CREATE EPIDEMIC CURVES AT DIFFERENT SCALES #
#################################################

# First, load metapopulation model data at Admin 2 level
load('./tmp/adm_2_metapop_dat.RData')

# Simulate an epidemic for display purposes only
adm_2_obs_col <- run_seir_model_multi(n = 50, density_dep = FALSE, method = 'append',
                                      R_0 = 1.85, gamma = 1/7, sigma = 1/2, 
                                      prop_s = 0.90, adm_name_vec = adm_2_name_vec, 
                                      adm_level = '2', pop_vec = adm_2_pop_vec, 
                                      intro_adm = 'Colombo', intro_num = 1,
                                      adm_x_walk = adm_2_x_walk, 
                                      travel_mat = adm_2_phone_mobility_mat, 
                                      max_time = 365, time_step = 1, mobility = TRUE)
# Average at the Admin 2 level
adm_2_at_2_obs_col <- adm_2_obs_col %>%
  group_by(time, adm_2) %>%
  mutate(avg_incid_I_adm_2 = mean(incid_I)) %>%
  distinct(time, adm_2, avg_incid_I_adm_2) %>% 
  ungroup() 

# Sum to the national level
adm_2_at_0_obs_col <- adm_2_at_2_obs_col %>%
  group_by(time) %>%
  mutate(avg_incid_I_adm_2 = sum(avg_incid_I_adm_2)) %>%
  distinct(time, avg_incid_I_adm_2) %>% 
  ungroup() 

# NATIONAL LEVEL
# Graph an epidemic curve at the national level
line <- ggplot() +
  geom_line(data = adm_2_at_0_obs_col, aes(x = time, y = avg_incid_I_adm_2), 
            color = '#DDBAA4', linewidth = 3, alpha = 1) + 
  theme(panel.border = element_blank(), axis.line = element_line(linewidth = 1.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        axis.text = element_blank(), axis.title=element_text(size=26)) + 
  xlab("Time") + ylab("Cases")

# Save
ggsave('./figs/national_level_curve.jpg', plot = line, height = 4, width = 6)

# INDIVIDUAL LEVEL
# Graph an empty curve to add individual level dots to
line <- ggplot() +
  geom_line(data = adm_2_at_0_obs_col, aes(x = time, y = avg_incid_I_adm_2), 
            color = '#FFFFFF00', linewidth = 3, alpha = 1) + 
  theme(panel.border = element_blank(), axis.line = element_line(linewidth = 1.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        axis.text = element_blank(), axis.title=element_text(size=26)) + 
  xlab("Time") + ylab("Cases")

# Save
ggsave('./figs/individual_level_curve.jpg', plot = line, height = 4, width = 6)

# SUBNATIONAL LEVEL
# Filter the epidemic simulation to 3 administrative units at Admin level 2
adm_2_at_2_obs_col_filt <- adm_2_at_2_obs_col |> filter(adm_2 == 'Matale' | 
                                                          adm_2 == 'Kandy' | 
                                                          adm_2 == 'Nuwara Eliya')
# Manually shift the curves for display purposes only
adm_2_at_2_obs_col_filt[adm_2_at_2_obs_col_filt$adm_2 == 'Kandy',]$time <- 
  adm_2_at_2_obs_col_filt[adm_2_at_2_obs_col_filt$adm_2 == 'Kandy',]$time - rep(65, 365)
adm_2_at_2_obs_col_filt[adm_2_at_2_obs_col_filt$adm_2 == 'Nuwara Eliya',]$time <- 
  adm_2_at_2_obs_col_filt[adm_2_at_2_obs_col_filt$adm_2 == 'Nuwara Eliya',]$time + rep(40, 365)
# Graph an epidemic curve at the subnational level
line <- ggplot() +
  geom_line(data = adm_2_at_2_obs_col_filt, aes(x = time, y = avg_incid_I_adm_2, color = adm_2), 
            linewidth = 3, alpha = 1) + 
  theme(panel.border = element_blank(), axis.line = element_line(linewidth = 1.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        axis.text = element_blank(), axis.title=element_text(size=26),
        legend.position = 'none') + ylim(0, 12000) +
  xlab("Time") + ylab("Cases") + scale_color_manual(values = c("#ABDDA4", "#A4C7DD","#D6A4DD"))

# Save
ggsave('./figs/subnational_level_curve.jpg', plot = line, height = 4, width = 6)

################################################################################
################################################################################

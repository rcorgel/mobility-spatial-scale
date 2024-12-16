################################################################################
# File Name: 04j_create_figures_appendix                                       #
#                                                                              #
# Purpose:   Create two figures for the appendix. One describing the mobile    #
#            phone data adjustment and the other describing country-wide       #
#            travel in Sri Lanka.                                              #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create mobile phone adjustment figure                          #
#            3. Create country-wide travel figure                              #
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
library(dplyr)
library(ggpubr)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

############################################
# 2. CREATE MOBILE PHONE ADJUSTMENT FIGURE #
############################################

# Load mobility data
load('./tmp/phone_mobility_dat.RData')

# Create adjusted and unadjusted data
un_adj <- phone_mobility_dat %>% group_by(date) %>%
  mutate(tot_trips = sum(trips)) %>%
  distinct(date, tot_trips, .keep_all = FALSE)

adj <- phone_mobility_dat %>% group_by(date) %>%
  mutate(tot_trips = sum(trips_adj)) %>%
  distinct(date, tot_trips, .keep_all = FALSE)

# Calculate adjusted and unadjusted trip proportions
trip_prop <- phone_mobility_dat %>% group_by(date, adm_2_origin, adm_2_destination) %>%
  mutate(adm_2_trips = sum(trips)) %>%
  distinct(date, adm_2_origin, adm_2_destination, adm_2_trips, adm_2_origin_code, adm_2_destination_code, .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(date, adm_2_origin) %>%
  mutate(day_sum = sum(adm_2_trips)) %>%
  ungroup() %>%
  mutate(trip_prop = adm_2_trips/day_sum) %>%
  mutate(route = paste(adm_2_origin_code, adm_2_destination_code))

trip_prop_adj <- phone_mobility_dat %>% group_by(date, adm_2_origin, adm_2_destination) %>%
  mutate(adm_2_trips = sum(trips_adj)) %>%
  distinct(date, adm_2_origin, adm_2_destination, adm_2_trips, adm_2_origin_code, adm_2_destination_code, .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(date, adm_2_origin) %>%
  mutate(day_sum = sum(adm_2_trips)) %>%
  ungroup() %>%
  mutate(trip_prop = adm_2_trips/day_sum) %>%
  mutate(route = paste(adm_2_origin_code, adm_2_destination_code))

# Create four sub-plots
plot_1 <- ggplot(un_adj) +
  geom_line(aes(x = date, y = tot_trips), color = '#4292C6', linewidth = 1.5) +
  theme_minimal() + xlab("Date") + ylab("Number of Trips") + ylim(0, 50000000) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank())

plot_2 <- ggplot(adj) +
  geom_line(aes(x = date, y = tot_trips), color = '#4292C6', linewidth = 1.5) +
  theme_minimal() + xlab("Date") + ylab("Number of Trips") + ylim(0, 50000000) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank())

plot_3 <- ggplot(trip_prop) +
  geom_line(aes(x = date, y = trip_prop, group = route), color = '#41AE76', alpha = 0.1, linewidth = 1) +
  geom_smooth(aes(x = date, y = trip_prop), method = 'loess', linewidth = 1.5, color = 'black', se = FALSE, linetype = "dashed") +
  theme_minimal() + xlab("Date") + ylab("Trip Proportion") + ylim(0, 1) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=30, hjust = 0.5),
        legend.position = 'none')

plot_4 <- ggplot(trip_prop_adj) +
  geom_line(aes(x = date, y = trip_prop, group = route), color = '#41AE76', alpha = 0.1, linewidth = 1) +
  geom_smooth(aes(x = date, y = trip_prop), method = 'loess', linewidth = 1.5, color = 'black', se = FALSE, linetype = "dashed") +
  theme_minimal() + xlab("Date") + ylab("Trip Proportion") + ylim(0, 1) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

# Create full figure and save
fig <- plot_grid(plot_1 + ggtitle('Pre-Adjustment Total Trip Volume'), 
                 plot_2 + ggtitle('Post-Adjustment Total Trip Volume'), 
                 plot_3 + ggtitle('Pre-Adjustment Admin 2 Trip Proportions'), 
                 plot_4 + ggtitle('Post-Adjustment Admin 2 Trip Proportions'), 
                 nrow = 2, labels = c('(a)', '(b)', '(c)', '(d)'),
                 label_size = 26, hjust = 0)
ggsave('./figs/figure_mobile_phone_adj.jpg', plot = fig, height = 20, width = 25)


########################################
# 3. CREATE COUNTRY-WIDE TRAVEL FIGURE #
########################################

# Make a static map
# Load the shape file
# From: https://data.humdata.org/dataset/cod-ab-mdg
choropleth <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                      layer = 'lka_admbnda_adm1_slsd_20220816')

ggplot() +
  geom_sf(data = choropleth, aes(), color="black", fill = '#FFFFFF', linewidth=0.5) +
  coord_sf() + theme_void()

# Load FB data and create a map
load('./tmp/fb_mobility_dat.RData')

# Create link data
fb_mobility_dat_agg <- fb_mobility_dat %>% group_by(Date, start_lat, start_lon, end_lat, end_lon) %>%
  dplyr::filter(!is.na(n_baseline)) %>%
  dplyr::filter(!is.na(start_lat)) %>%
  dplyr::filter(!is.na(start_lon)) %>%
  dplyr::filter(!is.na(end_lat)) %>%
  dplyr::filter(!is.na(end_lon)) %>%
  dplyr::mutate(trips = sum(n_baseline, na.rm = TRUE)) %>%
  distinct(Date, start_lat, start_lon, end_lat, end_lon, trips, 
           .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(start_lat, start_lon, end_lat, end_lon, trips) %>%
  dplyr::mutate(trips_avg = mean(trips, na.rm = TRUE)) %>%
  dplyr::filter(!is.na(trips_avg)) %>%
  distinct(start_lat, start_lon, end_lat, end_lon, trips_avg,
           .keep_all = FALSE) %>%
  ungroup()

# Create map
xquiet <- scale_x_continuous("", breaks=NULL)
yquiet <- scale_y_continuous("", breaks=NULL)
quiet <- list(xquiet, yquiet)
map_fb <- ggplot() +
  geom_sf(data = choropleth, aes(), color = "black", fill = 'white', linewidth = 0.15, alpha = 0.8) +
  geom_segment(data = fb_mobility_dat_agg,
               aes(x = start_lon, y = start_lat,
                   xend = end_lon, yend = end_lat, 
                   alpha = trips_avg), col = '#F16913') +
  scale_alpha_continuous(range = c(0.07, 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        legend.position = 'none',
        plot.title = element_text(size=30, hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size=16)) + 
  quiet +
  coord_sf()

# Load mobile phone data and create a map
load('./tmp/phone_mobility_dat.RData')

# Create link data
adm_3_phone_mobility_dat <- phone_mobility_dat %>%                                
  group_by(adm_3_origin, adm_3_destination) %>%
  mutate(trips_avg = mean(trips_adj, na.rm = TRUE)) %>%
  distinct(adm_3_origin, adm_3_destination, trips_avg, 
           adm_3_origin_code, adm_3_destination_code, 
           adm_2_origin, adm_2_destination, 
           adm_2_origin_code, adm_2_destination_code, 
           adm_1_origin, adm_1_destination, 
           adm_1_origin_code, adm_1_destination_code, 
           origin_long, origin_lat, 
           destination_long, destination_lat,
           .keep_all = FALSE) %>%
  ungroup()

# Create map and only exame popular routes
quantile(adm_3_phone_mobility_dat$trips_avg, probs = seq(0, 1, 0.1))
map_phone <- ggplot() +
  geom_sf(data = choropleth, aes(), color = "black", fill = 'white', linewidth = 0.15, alpha = 0.8) +
  geom_segment(data = adm_3_phone_mobility_dat[adm_3_phone_mobility_dat$trips_avg > 1.555612e+02, ],
               aes(x = origin_long, y = origin_lat,
                   xend = destination_long, yend = destination_lat, 
                   alpha = trips_avg), col = '#4292C6') +
  scale_alpha_continuous(range = c(0.07, 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        legend.position = 'none',
        plot.title = element_text(size=30, hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size=16)) + 
  quiet +
  coord_sf()

# Load travel survey data and create a map
load('./tmp/survey_dat_analysis.RData')

map_survey <- ggplot() +
  geom_sf(data = choropleth, aes(), color = "black", fill = 'white', linewidth = 0.15, alpha = 0.8) +
  geom_segment(data = survey_dat,
               aes(x = lon_res, y = lat_res,
                   xend = lon_loc, yend = lat_loc, 
                   alpha = 0.5), col = '#41AE76') +
  scale_alpha_continuous(range = c(0.07, 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        legend.position = 'none',
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size=16),
        plot.title = element_text(size=30, hjust = 0.5)) + 
  quiet +
  coord_sf()

# Compile figure
fig <- plot_grid(map_phone + ggtitle('Mobile Phone Data'), 
                 map_fb + ggtitle('Facebook Mobility Data'),
                 map_survey + ggtitle('Travel Survey Data'),
                 nrow = 1,
                 labels = c('(a)', 
                            '(b)',
                            '(c)'
                 ),
                 label_size = 26, hjust = 0)
ggsave('./figs/figure_mobility_maps.jpg', plot = fig, height = 16, width = 25)

################################################################################
################################################################################

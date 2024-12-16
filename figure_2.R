################################################################################
# File Name: figure_1                                                          #
#                                                                              #
# Purpose:   Create figure 1 for the paper.                                    #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure functions                                        #
#            3. Create map schematic subfigures                                #
#            4. Create external subfigures                                     #
#            5. Create internal subfigures                                     #
#            6. Create final figure                                            #
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

######################################
# 3. CREATE MAP SCHEMATIC SUBFIGURES #
######################################

# Add coordinates of highlight cities
coordinate_cities <- data.frame(
  city = c("Colombo", "Madhu"),
  lat = c(6.927632561772342, 8.85653415340985),
  long = c(79.85843709788357, 80.20433649099449))   

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
mobility_shape_xwalk <- read.csv('./tmp/mobility_shape_xwalk.csv')

# Merge on the cross walk
choropleth_3 <- left_join(choropleth_3, mobility_shape_xwalk, by = c('ADM3_EN' = 'adm_3_shape'))

# Join polygons to create 330 mobility admin 3 units
choropleth_3_mobility <- choropleth_3 %>% 
  group_by(adm_3_mobility) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# Create maps for figure 2
make_simple_map(data = choropleth_1, coord_data = coordinate_cities) + 
  coord_sf(ylim = c(8.4, 9.8), xlim = c(79.6, 81.3), expand = TRUE)

make_simple_map(data = choropleth_2, coord_data = coordinate_cities) + 
  coord_sf(ylim = c(8.4, 9.8), xlim = c(79.6, 81.3), expand = TRUE)

# Main map
map <- ggplot() +
  geom_sf(data = choropleth_1, aes(fill = ADM1_EN, group = ADM1_EN), color= 'black', linewidth = 1.5, alpha = 0.85) +
  geom_sf(data = choropleth_2, aes(group = ADM2_EN), color= 'black', fill = '#FFFFFF00', linewidth = 0.50) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('Internal and External Trips,\nProvinces and Districts') + theme(legend.position = 'right',
                                      plot.title = element_text(size = 30, hjust = 0.5),
                                      panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                      legend.title = element_text(size = 24),
                                      legend.text = element_text(size = 24),
                                      legend.key.size = unit(1.2, 'cm')) + 
  #coord_sf(ylim = c(8.4, 9.8), xlim = c(79.65, 81.5), expand = TRUE) +
  labs(fill = 'Provinces') + scale_fill_manual(values = c("#E6F598", "#ABDDA4", "#66C2A5",
                                                          "#5E4FA2", "#3288BD",  "#D53E4F", "#F46D43", "#FDAE61", "#9E0142"))

brewer.pal(11, 'Spectral')

# Reference map
map_ref <- ggplot() +
  geom_sf(data = choropleth_1, aes(fill = ADM1_EN), color= 'black', linewidth = 1, alpha = 0.85) +
  geom_sf(data = choropleth_2, aes(), color= 'black', fill = '#FFFFFF00', linewidth = 0.50) +
  geom_rect(
    aes(ymin = 8.3,
    ymax = 9.9,
    xmin = 79.4,
    xmax = 81.6),
    color = 'black',
    fill = NA, 
    linewidth = 1
  ) + theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(values = c("#E6F598", "#ABDDA4", "#66C2A5",
                               "#5E4FA2", "#3288BD",  "#D53E4F", "#F46D43", "#FDAE61", "#9E0142"))

# Combined maps
map_1 <- ggdraw() + 
  draw_plot(map) +
  draw_plot(map_ref,
            height = 0.3,
            width = 0.2,
            x = 0.75,
            y = 0.65)

# Save map
map <- ggplot() +
  geom_sf(data = choropleth_3_mobility, aes(fill = adm_3_mobility, group = adm_3_mobility), color= 'black', linewidth = 1.5, alpha = 1) +
  theme_void() + theme(legend.position = 'none')
ggsave('./figs/map_3.jpg', plot = map, height = 20, width = 25)

# Second stage of maps

choropleth_1$highlight <- c('0', '0', '0', '0', '1', '0', '0', '0', '0')
choropleth_2$highlight <- c('0', '0', '0', '0', '0', '0', '0', '0', '0', '2', '1', 
                            '2', '2', '2', '0', '0', '0', '0', '0', '0', '0', '0', 
                            '0', '0', '0')
choropleth_3_mobility$highlight <- '0'
choropleth_3_mobility$highlight[156] <- '1'

# Merge on x walk
x_walk <- read_csv('./tmp/admin_xwalk.csv')
choropleth_3_mobility <- left_join(choropleth_3_mobility, x_walk, 
                                   by = c('adm_3_mobility' = 'adm_3'))

# Create admin 1 map
map_2 <- ggplot() +
  geom_sf(data = choropleth_1[5, ], aes(), color= 'black', fill = "#4292C6", linewidth = 1.5, alpha = 1) +
  geom_sf(data = choropleth_2[c(11), ], aes(fill = highlight), color= 'black', linewidth = 0.50, alpha = 1) +
  geom_point(data = coordinate_cities[2,], aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities[2,], aes(x = long - 0.14 , y = lat + 0.12 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('Northern Province') + theme(legend.position = 'none',
                                      plot.title = element_text(size = 30, hjust = 0.5),
                                      legend.text = element_text(size = 20),
                                      panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf()

ggsave('./figs/figure_2_map_1.jpg', plot = map_2, height = 25, width = 20)

# Create admin 2 map
map_1_2 <- ggplot() +
  geom_sf(data = choropleth_1[5, ], aes(), fill = 'white', color= 'black', linewidth = 1.5) +
  geom_sf(data = choropleth_2[c(11), ], aes(fill = highlight), color= 'black', linewidth = 1.5, alpha = 1) +
  theme_void() + theme(legend.position = 'none',
                                      plot.title = element_text(size = 30, hjust = 0.5),
                                      legend.text = element_text(size = 20),
                                      panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() +
  scale_fill_manual(values = c("#41AE76", 'white')) + ggtitle('Mannar District')

ggsave('./figs/figure_2_map_2.jpg', plot = map_1_2, height = 20, width = 25)

map_2_3 <- ggplot() +
  geom_sf(data = choropleth_1[5, ], aes(), fill = 'white', color= 'black', linewidth = 1.5) +
  geom_sf(data = choropleth_2[c(11), ], aes(), fill = 'white', color= 'black', linewidth = 1.50) +
  geom_sf(data = choropleth_3_mobility[choropleth_3_mobility$adm_3_mobility == 'Madhu', ], aes(fill = highlight), color= 'black', linewidth = 1.5, alpha = 1) +
  theme_void() + theme(legend.position = 'none',
                                                                              plot.title = element_text(size = 30, hjust = 0.5),
                                                                              legend.text = element_text(size = 20),
                                                                              panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() +
  scale_fill_manual(values = c( "#807DBA")) + ggtitle('Madhu Division')

ggsave('./figs/figure_2_map_1.jpg', plot = map_2_3, height = 20, width = 25)

map_1_3 <- ggplot() +
  geom_sf(data = choropleth_1[5, ], aes(), fill = '#4292C6', color= 'black', linewidth = 1.5) +
  geom_sf(data = choropleth_3_mobility[choropleth_3_mobility$adm_3_mobility == 'Madhu', ], aes(fill = highlight), color= 'black', linewidth = 1.5, alpha = 1) +
  geom_point(data = coordinate_cities[2,], aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities[2,], aes(x = long - 0.14 , y = lat + 0.12 , label = city), size = 8, fontface = 'bold') +
  theme_void() + theme(legend.position = 'none',
                       plot.title = element_text(size = 30, hjust = 0.5),
                       legend.text = element_text(size = 20),
                       panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() +
  scale_fill_manual(values = c( "#807DBA")) + ggtitle('Northern Province \nand Madhu Division')

ggsave('./figs/figure_2_map_3.jpg', plot = map_1_3, height = 20, width = 25)


#################################
# 4. CREATE EXTERNAL SUBFIGURES #
#################################

# Load mobility data for each data source
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')
load('./tmp/fmt_adm_2_phone_mobility_dat.RData')
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')
load('./tmp/simulated_phone_mobility_format.RData')

# Load admin crosswalk
admin_xwalk <- read_csv('./tmp/admin_xwalk.csv')

# Callout
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk, 
                                       by = c('destination' = 'adm_3'))
adm_3_madhu <- adm_3_phone_mobility_long[adm_3_phone_mobility_long$origin == 'Madhu',]
adm_3_madhu$type <- ifelse(adm_3_madhu$adm_2 == "Mannar", 'Internal', 'External')
madhu_3 <- adm_3_madhu %>% group_by(type) %>%
  mutate(type_sum = sum(value, na.rm = TRUE)) %>%
  distinct(type, type_sum)
madhu_3$order <- c(2, 1)
madhu_3$type_sum[2] <- 1 - madhu_3$type_sum[1]
comp_1_3 <- ggplot(data=madhu_3, aes(x=order, y=type_sum, fill = type)) + 
  geom_bar(stat="identity", position="identity", width = 0.5, color = 'black', alpha = 1)+ labs(x="", y="") + theme_classic() +
  scale_fill_manual('', values = c('white', '#807DBA'),
                      breaks = c('Internal', 'External')) + ylab('Trip Proportion') +
  ylim(0, 0.8) + theme(legend.position = 'bottom',
                     axis.text = element_blank(), 
                     axis.title.y = element_text(size = 24),
                     axis.ticks = element_blank(),
                     legend.title = element_text(size = 24),
                     legend.text = element_text(size = 24),
                     legend.key.size = unit(1.2, 'cm')) +
  geom_text(aes(label=round(type_sum, digits = 2)), size = 8, 
            position=position_dodge(width=0.9), vjust=-0.25)

adm_3_madhu$type <- ifelse(adm_3_madhu$adm_1 == "Northern", 'District', 'Division')
madhu_3 <- adm_3_madhu %>% group_by(type) %>%
  mutate(type_sum = sum(value, na.rm = TRUE)) %>%
  distinct(type, type_sum)
madhu_3$order <- c(2, 1)
madhu_3$type_sum[2] <- 0.22524324
madhu_3$type_sum[1] <- 0.66209230
comp_2_3 <- ggplot(data=madhu_3, aes(x=order, y=type_sum, fill = type)) + 
  geom_bar(stat="identity", position="identity", width = 0.5, color = 'black', alpha = 1)+ labs(x="", y="") + theme_classic() +
  scale_fill_manual('', values = c('#41AE76', '#807DBA'),
                    breaks = c('District', 'Division')) + ylab('External District Trip Proportion') +
  ylim(0, 0.7) + theme(legend.position = 'bottom',
                       axis.text = element_blank(), 
                       axis.title.y = element_text(size = 24),
                       axis.ticks = element_blank(),
                       legend.title = element_text(size = 24),
                       legend.text = element_text(size = 24),
                       legend.key.size = unit(1.2, 'cm')) +
  geom_text(aes(label=round(type_sum, digits = 2)), size = 8, 
            position=position_dodge(width=0.9), vjust=-0.25)
  
admin_xwalk_2 <- admin_xwalk %>% group_by(adm_2) %>%
  distinct(adm_2, adm_1, .keep_all = FALSE)
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, admin_xwalk_2, 
                                       by = c('destination' = 'adm_2'))
adm_2_madhu <- adm_2_phone_mobility_long[adm_2_phone_mobility_long$origin == 'Mannar',]
adm_2_madhu$type <- ifelse(adm_2_madhu$destination == "Mannar", 'Internal', 'External')
madhu_2 <- adm_2_madhu %>% group_by(type) %>%
  mutate(type_sum = sum(value, na.rm = TRUE)) %>%
  distinct(type, type_sum)
madhu_2$order <- c(2, 1)
madhu_2$type_sum[2] <- 1 - madhu_2$type_sum[1]
comp_1_2 <- ggplot(data=madhu_2, aes(x=order, y=type_sum, fill = type)) + 
  geom_bar(stat="identity", position="identity", width = 0.5, color = 'black', alpha = 1)+ labs(x="", y="") + theme_classic() +
  scale_fill_manual('', values = c('white', '#41AE76'),
                      breaks = c('Internal', 'External')) + ylab('Trip Proportion') +
  ylim(0, 0.8) + theme(legend.position = 'bottom',
                     axis.text = element_blank(), 
                     axis.title.y = element_text(size = 24),
                     axis.ticks = element_blank(),
                     legend.title = element_text(size = 24),
                     legend.text = element_text(size = 24),
                     legend.key.size = unit(1.2, 'cm')) +
  geom_text(aes(label=round(type_sum, digits = 2)), size = 8, 
            position=position_dodge(width=0.9), vjust=-0.25)

# Combine Figs
sub_1 <- cowplot::plot_grid(map_1_2, ggplot() + theme_void(),
                            comp_1_2 + ggtitle('\n\n'), ggplot() + theme_void(),
                            nrow = 1, rel_widths = c(1, 0.1, 1, 0.2))
sub_2 <- cowplot::plot_grid(map_1_3, ggplot() + theme_void(),
                            comp_1_3 + ggtitle('\n\n'), ggplot() + theme_void(),
                            nrow = 1, rel_widths = c(1, 0.1, 1, 0.2))
sub_3 <- cowplot::plot_grid(map_2_3, ggplot() + theme_void(),
                            comp_2_3 + ggtitle('\n\n'), ggplot() + theme_void(),
                            nrow = 1, rel_widths = c(1, 0.1, 1, 0.2))



sub <- cowplot::plot_grid(
                          map_2_3, comp_1_3,
                          nrow = 1)

ggsave('./figs/pres_fig_nest_2.jpg', plot = sub, height = 5, width = 12)


# Load mobility data for each data source
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')
load('./tmp/fmt_adm_2_phone_mobility_dat.RData')
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')
load('./tmp/simulated_phone_mobility_format.RData')

######################
# ADMIN 3 to ADMIN 2 #
######################

#####################
# MOBILE PHONE DATA #
#####################

# Merge on destination
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 2)], 
                                       by = c('destination' = 'adm_3'))
# Merge on admin 2 origin
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 2)],
                                       by = c('origin' = 'adm_3'))


adm_3_phone_mobility_long$cluster <- ifelse(adm_3_phone_mobility_long$origin == 
                                              adm_3_phone_mobility_long$destination, 'Stay', 'In District')
adm_3_phone_mobility_long$cluster <- ifelse(adm_3_phone_mobility_long$adm_2.x != 
                                              adm_3_phone_mobility_long$adm_2.y, 'Out District', adm_3_phone_mobility_long$cluster)

cluster_analysis <- adm_3_phone_mobility_long %>% group_by(origin, cluster) %>%
  mutate(
    value_sum = sum(value, na.rm = TRUE),
         value_sum = ifelse(is.na(value_sum), 0, value_sum)) %>%
  distinct(origin, cluster, value_sum, .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(origin) %>% mutate(sum = sum(value_sum))

cluster_in <- cluster_analysis %>% filter(cluster == 'In District') %>%
  dplyr::rename('in' = 'value_sum')
cluster_out <- cluster_analysis %>% filter(cluster == 'Out District') %>%
  dplyr::rename('out' = 'value_sum')
cluster_merge_3_2 <- left_join(cluster_in, cluster_out, by = c('origin' = 'origin'))
cluster_merge_3_2$diff <- cluster_merge_3_2$`in` - cluster_merge_3_2$out
mean(cluster_merge_3_2$diff )
hist(cluster_merge_3_2$diff, breaks = 40)

# Calculate out of district travel at the admin 3 unit
adm_3_adm_2_phone <- adm_3_phone_mobility_long %>%
  filter(adm_2.x != adm_2.y) %>%
  group_by(origin) %>%
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) %>%
  distinct(origin, adm_2.y, adm_3_sum, .keep_all = FALSE) %>%
  dplyr::rename('adm_2_origin' = 'adm_2.y')

# Merge on origin from admin 2
adm_3_adm_2_phone <- left_join(adm_3_adm_2_phone, 
                               adm_2_phone_mobility_long[
                                 adm_2_phone_mobility_long$origin == 
                                   adm_2_phone_mobility_long$destination, ], 
                               by = c('adm_2_origin' = 'origin'))
# Calculate 1 - stays
adm_3_adm_2_phone$adm_2_value <- 1 - adm_3_adm_2_phone$value
# Calculate difference between nested units
adm_3_adm_2_phone$diff <- adm_3_adm_2_phone$adm_2_value - adm_3_adm_2_phone$adm_3_sum

# Load population data
load('./tmp/adm_population_dat.RData')
# Merge on admin 3 population
adm_3_adm_2_phone <- left_join(adm_3_adm_2_phone, adm_3_population_dat[, c(1, 4)], 
                               by = c('origin' = 'adm_3_mobility'))

###############################
# SIMULATED MOBILE PHONE DATA #
###############################

# Merge on destination
adm_3_phone_mobility_sim_dat <- left_join(adm_3_phone_mobility_sim_dat, admin_xwalk[, c(1, 2)], 
                                          by = c('adm_3_destination' = 'adm_3'))
# Merge on admin 2 origin
adm_3_phone_mobility_sim_dat <- left_join(adm_3_phone_mobility_sim_dat, admin_xwalk[, c(1, 2)],
                                          by = c('adm_3_origin' = 'adm_3'))

# Calculate out of district travel at the admin 3 unit
adm_3_adm_2_phone_sim <- adm_3_phone_mobility_sim_dat %>%
  filter(adm_2.x != adm_2.y) %>%
  group_by(adm_3_origin) %>%
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) %>%
  distinct(adm_3_origin, adm_2.y, adm_3_sum, .keep_all = FALSE) %>%
  dplyr::rename('adm_2_origin' = 'adm_2.y')

# Merge on origin from admin 2
adm_3_adm_2_phone_sim <- left_join(adm_3_adm_2_phone_sim, 
                                   adm_2_phone_mobility_sim_dat[
                                     adm_2_phone_mobility_sim_dat$adm_2_origin == 
                                       adm_2_phone_mobility_sim_dat$adm_2_destination, ], 
                                   by = c('adm_2_origin' = 'adm_2_origin'))
# Calculate 1 - stays
adm_3_adm_2_phone_sim$adm_2_value <- 1 - adm_3_adm_2_phone_sim$value
# Calculate difference between nested units
adm_3_adm_2_phone_sim$diff <- adm_3_adm_2_phone_sim$adm_2_value - adm_3_adm_2_phone_sim$adm_3_sum

# Merge on admin 3 population
adm_3_adm_2_phone_sim <- left_join(adm_3_adm_2_phone_sim, adm_3_population_dat[, c(1, 4)], 
                                   by = c('adm_3_origin' = 'adm_3_mobility'))

# Combine observed and simulated
adm_3_adm_2_phone$Cat <- 'Observed'
adm_3_adm_2_phone_sim$Cat <- 'Simulated'
adm_3_adm_2_phone_sim <- adm_3_adm_2_phone_sim %>% 
  select(-c(origin, destination, value_cat)) %>%
  dplyr::rename('origin' = 'adm_3_origin', 
         'destination' = 'adm_2_destination')
adm_3_adm_2 <- rbind(adm_3_adm_2_phone, adm_3_adm_2_phone_sim)

# Create scatter plot by population
plot_3_2_scatter <- ggplot(adm_3_adm_2_phone, aes(log(population_2020_adm_3), diff, color = Cat)) +
  geom_point(aes(color = Cat, group = Cat), color = '#565656', alpha = 0.1, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(color = Cat, group = Cat), color = '#565656', method = "lm", se = FALSE, linewidth = 3.5, alpha = 0.9) + 
  ylim(-0.6, 0.5) +
  xlab('Log Population (Division)') + 
  ylab('External District Trip\nProportion Difference') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size=30, hjust = 0.5)) 
  #scale_color_manual(values = c("#FDAE61", 'grey70'))

choropleth_3_mobility <- left_join(choropleth_3_mobility, adm_3_adm_2_phone[ c('origin', 'diff')], by = 
                                     c('adm_3_mobility' = 'origin'))
choropleth_3_mobility$diff <- rep(0, 330)
test_1 <- ggplot() +
  geom_sf(data = choropleth_3_mobility, aes(fill = diff), color= 'black', linewidth = 0.25, alpha = 1) +
  geom_sf(data = choropleth_2, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(
                                                    plot.title = element_text(size = 30, hjust = 0.5),
                                                    legend.text = element_text(size = 20),
                                                    legend.title = element_text(size = 24),
                                                    panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
    low = '#807DBA', mid = "white", high = '#41AE76',
    midpoint = 0, aesthetics = 'fill', limits=c(-0.65, 0.65)
  )

ggsave('./figs/pres_map.jpg', plot = test_1, height = 7, width = 8)


######################
# ADMIN 3 to ADMIN 1 #
######################

#####################
# MOBILE PHONE DATA #
#####################

# Merge on destination
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 3)], 
                                       by = c('destination' = 'adm_3'))
# Merge on origin
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 3)],
                                       by = c('origin' = 'adm_3'))

adm_3_phone_mobility_long$cluster <- ifelse(adm_3_phone_mobility_long$origin == 
                                              adm_3_phone_mobility_long$destination, 'Stay', 'In District')
adm_3_phone_mobility_long$cluster <- ifelse(adm_3_phone_mobility_long$adm_1.x != 
                                              adm_3_phone_mobility_long$adm_1.y, 'Out District', adm_3_phone_mobility_long$cluster)

cluster_analysis <- adm_3_phone_mobility_long %>% group_by(origin, cluster) %>%
  mutate(
    value_sum = sum(value, na.rm = TRUE),
    value_sum = ifelse(is.na(value_sum), 0, value_sum)) %>%
  distinct(origin, cluster, value_sum, .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(origin) %>% mutate(sum = sum(value_sum))

cluster_in <- cluster_analysis %>% filter(cluster == 'In District') %>%
  dplyr::rename('in' = 'value_sum')
cluster_out <- cluster_analysis %>% filter(cluster == 'Out District') %>%
  dplyr::rename('out' = 'value_sum')
cluster_merge_3_1 <- left_join(cluster_in, cluster_out, by = c('origin' = 'origin'))
cluster_merge_3_1$diff <- cluster_merge_3_1$`in` - cluster_merge_3_1$out
mean(cluster_merge_3_1$diff )
hist(cluster_merge_3_1$diff, breaks = 40)

# Calculate out of district travel at the admin 3 unit
adm_3_adm_1_phone <- adm_3_phone_mobility_long %>%
  filter(adm_1.x != adm_1.y) %>%
  group_by(origin) %>%
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) %>%
  distinct(origin, adm_1.y, adm_3_sum, .keep_all = FALSE) %>%
  dplyr::rename('adm_1_origin' = 'adm_1.y')

# Merge on origin from admin 1
adm_3_adm_1_phone <- left_join(adm_3_adm_1_phone, 
                               adm_1_phone_mobility_long[
                                 adm_1_phone_mobility_long$origin == 
                                   adm_1_phone_mobility_long$destination, ], 
                               by = c('adm_1_origin' = 'origin'))
# Calculate 1 - stays
adm_3_adm_1_phone$adm_1_value <- 1 - adm_3_adm_1_phone$value
# Calculate difference between nested units
adm_3_adm_1_phone$diff <- adm_3_adm_1_phone$adm_1_value - adm_3_adm_1_phone$adm_3_sum

# Load population data
# Merge on admin 3 population
adm_3_adm_1_phone <- left_join(adm_3_adm_1_phone, adm_3_population_dat[, c(1, 4)], 
                               by = c('origin' = 'adm_3_mobility'))

###############################
# SIMULATED MOBILE PHONE DATA #
###############################

# Merge on destination
adm_3_phone_mobility_sim_dat <- left_join(adm_3_phone_mobility_sim_dat, admin_xwalk[, c(1, 3)], 
                                          by = c('adm_3_destination' = 'adm_3'))
# Merge on origin
adm_3_phone_mobility_sim_dat <- left_join(adm_3_phone_mobility_sim_dat, admin_xwalk[, c(1, 3)],
                                          by = c('adm_3_origin' = 'adm_3'))
# Calculate out of district travel at the admin 3 unit
adm_3_adm_1_phone_sim <- adm_3_phone_mobility_sim_dat %>%
  filter(adm_1.x != adm_1.y) %>%
  group_by(adm_3_origin) %>%
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) %>%
  distinct(origin, adm_1.y, adm_3_sum, .keep_all = FALSE) %>%
  dplyr::rename('adm_1_origin' = 'adm_1.y')

# Merge on origin from admin 1
adm_3_adm_1_phone_sim <- left_join(adm_3_adm_1_phone_sim, 
                                   adm_1_phone_mobility_sim_dat[
                                     adm_1_phone_mobility_sim_dat$origin == 
                                       adm_1_phone_mobility_sim_dat$destination, ], 
                                   by = c('adm_1_origin' = 'adm_1_origin'))
# Calculate 1 - stays
adm_3_adm_1_phone_sim$adm_1_value <- 1 - adm_3_adm_1_phone_sim$value
# Calculate difference between nested units
adm_3_adm_1_phone_sim$diff <- adm_3_adm_1_phone_sim$adm_1_value - adm_3_adm_1_phone_sim$adm_3_sum

# Load population data
# Merge on admin 3 population
adm_3_adm_1_phone_sim <- left_join(adm_3_adm_1_phone_sim, adm_3_population_dat[, c(1, 4)], 
                                   by = c('adm_3_origin' = 'adm_3_mobility'))

# Combine observed and simulated
adm_3_adm_1_phone$Cat <- 'Observed'
adm_3_adm_1_phone_sim$Cat <- 'Simulated'
adm_3_adm_1_phone_sim <- adm_3_adm_1_phone_sim %>%
  select(-c(origin.y, origin.x, destination, value_cat)) %>%
  dplyr::rename('origin' = 'adm_3_origin', 
         'destination' = 'adm_1_destination')
adm_3_adm_1 <- rbind(adm_3_adm_1_phone, adm_3_adm_1_phone_sim)

# Create scatter plot by population
plot_3_1_scatter <- ggplot(adm_3_adm_1_phone, aes(log(population_2020_adm_3), diff, color = Cat)) +
  geom_point(aes(color = Cat, group = Cat), color = '#565656', alpha = 0.1, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(color = Cat, group = Cat), color = '#565656', method = "lm", se = FALSE, linewidth = 3.5, alpha = 9) + 
  ylim(-0.6, 0.5) +
  xlab('Log Population (Division)') + 
  ylab('External Province Trip\nProportion Difference') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size=30, hjust = 0.5))
  #scale_color_manual(values = c("#FDAE61", 'grey70'))

choropleth_3_mobility <- left_join(choropleth_3_mobility, adm_3_adm_1_phone[ c('origin', 'diff')], by = 
                                     c('adm_3_mobility' = 'origin'))
test_2 <- ggplot() +
  geom_sf(data = choropleth_3_mobility, aes(fill = diff.y), color= 'black', linewidth = 0.25, alpha = 1) +
  geom_sf(data = choropleth_1, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(
    plot.title = element_text(size = 30, hjust = 0.5),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 24),
    panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
    low = '#807DBA', mid = "white", high = '#4292C6',
    midpoint = 0, aesthetics = 'fill', limits=c(-0.65, 0.65)
  )


######################
# ADMIN 2 to ADMIN 1 #
######################

#####################
# MOBILE PHONE DATA #
#####################

# Change admin cross walk to admin 2 level
admin_xwalk_adm_2 <- admin_xwalk %>%
  group_by(adm_2, adm_1) %>%
  distinct(adm_2, adm_1, .keep_all = FALSE)

# Merge on destination
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, admin_xwalk_adm_2, 
                                       by = c('destination' = 'adm_2'))
# Merge on origin
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, admin_xwalk_adm_2,
                                       by = c('origin' = 'adm_2'))

adm_2_phone_mobility_long$cluster <- ifelse(adm_2_phone_mobility_long$origin == 
                                              adm_2_phone_mobility_long$destination, 'Stay', 'In District')
adm_2_phone_mobility_long$cluster <- ifelse(adm_2_phone_mobility_long$adm_1.x != 
                                              adm_2_phone_mobility_long$adm_1.y, 'Out District', adm_2_phone_mobility_long$cluster)

cluster_analysis <- adm_2_phone_mobility_long %>% group_by(origin, cluster) %>%
  mutate(
    value_sum = sum(value, na.rm = TRUE),
    value_sum = ifelse(is.na(value_sum), 0, value_sum)) %>%
  distinct(origin, cluster, value_sum, .keep_all = FALSE) %>%
  ungroup() %>%
  group_by(origin) %>% mutate(sum = sum(value_sum))

cluster_in <- cluster_analysis %>% filter(cluster == 'In District') %>%
  dplyr::rename('in' = 'value_sum')
cluster_out <- cluster_analysis %>% filter(cluster == 'Out District') %>%
  dplyr::rename('out' = 'value_sum')
cluster_merge_2_1 <- left_join(cluster_in, cluster_out, by = c('origin' = 'origin'))
cluster_merge_2_1$diff <- cluster_merge_2_1$`in` - cluster_merge_2_1$out
mean(cluster_merge_2_1$diff )
hist(cluster_merge_2_1$diff, breaks = 40)

# Calculate out of district travel at the admin 1 unit
adm_2_adm_1_phone <- adm_2_phone_mobility_long %>%
  filter(adm_1.x != adm_1.y) %>%
  group_by(origin) %>%
  mutate(adm_2_sum = sum(value, na.rm = TRUE)) %>%
  distinct(origin, adm_1.y, adm_2_sum, .keep_all = FALSE) %>%
  dplyr::rename('adm_1_origin' = 'adm_1.y')

# Merge on origin from admin 1
adm_2_adm_1_phone <- left_join(adm_2_adm_1_phone, 
                               adm_1_phone_mobility_long[
                                 adm_1_phone_mobility_long$origin == 
                                   adm_1_phone_mobility_long$destination, ], 
                               by = c('adm_1_origin' = 'origin'))
# Calculate 1 - stays
adm_2_adm_1_phone$adm_1_value <- 1 - adm_2_adm_1_phone$value
# Calculate difference between nested units
adm_2_adm_1_phone$diff <- adm_2_adm_1_phone$adm_1_value - adm_2_adm_1_phone$adm_2_sum

# Load population data
# Merge on admin 3 population
adm_2_adm_1_phone <- left_join(adm_2_adm_1_phone, adm_2_population_dat[, c(1, 3)], 
                               by = c('origin' = 'adm_2'))

###############################
# SIMULATED MOBILE PHONE DATA #
###############################

# Merge on destination
adm_2_phone_mobility_sim_dat <- left_join(adm_2_phone_mobility_sim_dat, admin_xwalk_adm_2, 
                                          by = c('adm_2_destination' = 'adm_2'))
# Merge on origin
adm_2_phone_mobility_sim_dat <- left_join(adm_2_phone_mobility_sim_dat, admin_xwalk_adm_2,
                                          by = c('adm_2_origin' = 'adm_2'))
# Calculate out of district travel at the admin 3 unit
adm_2_adm_1_phone_sim <- adm_2_phone_mobility_sim_dat %>%
  filter(adm_1.x != adm_1.y) %>%
  group_by(adm_2_origin) %>%
  mutate(adm_2_sum = sum(value, na.rm = TRUE)) %>%
  distinct(origin, adm_1.y, adm_2_sum, .keep_all = FALSE) %>%
  dplyr::rename('adm_1_origin' = 'adm_1.y')

# Merge on origin from admin 1
adm_2_adm_1_phone_sim <- left_join(adm_2_adm_1_phone_sim, 
                                   adm_1_phone_mobility_sim_dat[
                                     adm_1_phone_mobility_sim_dat$origin == 
                                       adm_1_phone_mobility_sim_dat$destination, ], 
                                   by = c('adm_1_origin' = 'adm_1_origin'))
# Calculate 1 - stays
adm_2_adm_1_phone_sim$adm_1_value <- 1 - adm_2_adm_1_phone_sim$value
# Calculate difference between nested units
adm_2_adm_1_phone_sim$diff <- adm_2_adm_1_phone_sim$adm_1_value - adm_2_adm_1_phone_sim$adm_2_sum

# Load population data
# Merge on admin 2 population
adm_2_adm_1_phone_sim <- left_join(adm_2_adm_1_phone_sim, adm_2_population_dat[, c(1, 3)], 
                               by = c('adm_2_origin' = 'adm_2'))

# Combine observed and simulated
adm_2_adm_1_phone$Cat <- 'Observed'
adm_2_adm_1_phone_sim$Cat <- 'Simulated'
adm_2_adm_1_phone_sim <- adm_2_adm_1_phone_sim %>%
  select(-c(origin.y, origin.x, destination, value_cat)) %>%
  dplyr::rename('origin' = 'adm_2_origin', 
         'destination' = 'adm_1_destination')
adm_2_adm_1 <- rbind(adm_2_adm_1_phone, adm_2_adm_1_phone_sim)

# Create scatter plot by population
plot_2_1_scatter <- ggplot(adm_2_adm_1_phone, aes(log(population_2020_adm_2), diff, color = Cat)) +
  geom_point(aes(color = Cat, group = Cat), color = '#565656', alpha = 0.1, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(color = Cat, group = Cat), color = '#565656', method = "lm", se = FALSE, linewidth = 3.5, alpha = 1) + 
  ylim(-0.6, 0.5) +
  xlab('Log Population (District)') + 
  ylab('External Province Trip\nProportion Difference') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size=30, hjust = 0.5)) 
  #scale_color_manual(values = c("#FDAE61", 'grey70'))

choropleth_2 <- left_join(choropleth_2, adm_2_adm_1_phone[ c('origin', 'diff')], by = 
                                     c('ADM2_EN' = 'origin'))
test_3 <- ggplot() +
  geom_sf(data = choropleth_2, aes(fill = diff), color= 'black', linewidth = 0.25, alpha = 1) +
  geom_sf(data = choropleth_1, aes(), fill = '#FFFFFF00', color= 'black', linewidth = 1) +
  theme_void() + ggtitle('') + theme(
    plot.title = element_text(size = 30, hjust = 0.5),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 20),
    panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white')) + 
  coord_sf() + 
  scale_colour_gradient2('Difference',
                         low = '#41AE76', mid = "white", high = '#4292C6',
                         midpoint = 0, aesthetics = 'fill', limits=c(-0.65, 0.65)
  )

# CREATE FIGURES
# Create Differences in Out of Unit Travel between Administrative Levels by 
# Population for Mobile Phone Data figure
legend <- get_legend(plot_2_1_scatter + theme(legend.position = 'bottom',
                                              legend.text = element_text(size = 26),
                                              legend.key.size = unit(1, 'cm')) + 
                       scale_color_manual(name = '', values = c("#FDAE61", 'grey70')))
external_plots <- plot_grid(plot_2_1_scatter, plot_3_1_scatter, plot_3_2_scatter,
                    nrow = 3,
                    labels = c('', 
                               '',
                               ''),
                    label_size = 26, hjust = 0)

external_plot <- plot_grid(external_plots, legend,
                            nrow = 2, rel_heights = c(1, 0.05), 
                            labels = c('', 
                                       ''),
                            label_size = 26, hjust = 0)

# Save figure
ggsave('./figs/figure_nested_trip_prop_pop.jpg', plot = external_plot, height = 10, width = 25)

#################################
# 5. CREATE INTERNAL SUBFIGURES #
#################################

# Plot functions
# Data summary function, mean +/- one sd
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y = m,ymin = ymin,ymax = ymax))
}

# Create violin plots of internal trip proportions by location
make_violin_plot <- function(data, color) {
  plot <- ggplot(data, aes(x = level, y = value, fill = level)) +
    geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, scale="width", width = 0.6) +
    theme_minimal() + ylim(0, 1) +
    scale_fill_manual(values=c(color, 'grey70')) +
    theme(legend.position = 'none') +
    xlab('') + 
    ylab('\nInternal Trip Proportion') +
    ggtitle(' ') +
    theme(axis.title = element_text(size=26),
          axis.text = element_text(size=22),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=30, hjust = 0.5))
  return(plot)
} 

make_scatter_plot <- function(data) {
  plot <- ggplot(data) +
    geom_point(aes(x = log(population), y = value, color = level), alpha = 0.25, size = 4) +
    geom_smooth(method = lm, aes(x = log(population), y = value, color = level), 
                alpha = 0.8, se = FALSE, linewidth = 2.5) +
    ylim(0, 1) + xlim(6, 16.5) +
    scale_color_manual('Scale', values=c('#41AE76', '#807DBA', '#4292C6')) +
    xlab('Log Population') + 
    ylab('Internal Trip Proportion') +
    ggtitle(' ') +
    theme_minimal() + 
    theme(axis.title = element_text(size=26),
          axis.text = element_text(size=20),
          panel.grid.major.x = element_blank(),
          legend.text = element_text(size = 22),
          legend.title = element_text(size = 22),
          panel.grid.minor = element_blank(),
          legend.position = 'bottom',
          plot.title = element_text(size=30, hjust = 0.5))
  return(plot)
}

#####################
# MOBILE PHONE DATA #
#####################

# Load phone mobility data
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')
load('./tmp/fmt_adm_2_phone_mobility_dat.RData')
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')

# Load population data
load('./tmp/adm_population_dat.RData')

# Calculate internal trip proportions
# Administrative Level 3
adm_3_phone_dat <- adm_3_phone_mobility_long %>% 
  dplyr::filter(origin == destination) %>%
  dplyr::mutate(level = '3')
adm_3_phone_dat <- left_join(adm_3_phone_dat, adm_3_population_dat[, c(1, 4)], 
                             by = c('origin' = 'adm_3_mobility'))
adm_3_phone_dat <- adm_3_phone_dat %>% dplyr::rename('population' = 'population_2020_adm_3')

# Administrative Level 2
adm_2_phone_dat <- adm_2_phone_mobility_long %>% 
  dplyr::filter(origin == destination) %>%
  dplyr::mutate(level = '2')
adm_2_phone_dat <- left_join(adm_2_phone_dat, adm_2_population_dat[, c(1, 3)], 
                             by = c('origin' = 'adm_2'))
adm_2_phone_dat <- adm_2_phone_dat %>% dplyr::rename('population' = 'population_2020_adm_2')

# Administrative Level 1
adm_1_phone_dat <- adm_1_phone_mobility_long %>% 
  dplyr::filter(origin == destination) %>%
  dplyr::mutate(level = '1')
adm_1_phone_dat <- left_join(adm_1_phone_dat, adm_1_population_dat[, c(1, 2)], 
                             by = c('origin' = 'adm_1'))
adm_1_phone_dat <- adm_1_phone_dat %>% dplyr::rename('population' = 'population_2020_adm_1')

###############################
# SIMULATED MOBILE PHONE DATA #
###############################

# Load phone mobility data
load('./tmp/simulated_phone_mobility_format.RData')

# Calculate internal trip proportions
# Administrative Level 3
adm_3_phone_sim_dat <- adm_3_phone_mobility_sim_dat %>% 
  dplyr::filter(origin == destination) %>%
  dplyr::mutate(level = '3')
adm_3_phone_sim_dat <- left_join(adm_3_phone_sim_dat, adm_3_population_dat[, c(1, 4)], 
                                 by = c('adm_3_origin' = 'adm_3_mobility'))
adm_3_phone_sim_dat <- adm_3_phone_sim_dat %>% dplyr::rename('population' = 'population_2020_adm_3',
                                                             'origin_code' = 'origin',
                                                             'destiation_code' = 'destination',
                                                             'origin' = 'adm_3_origin',
                                                             'destination' = 'adm_3_destination')

# Administrative Level 2
adm_2_phone_sim_dat <- adm_2_phone_mobility_sim_dat %>% 
  dplyr::filter(origin == destination) %>%
  dplyr::mutate(level = '2')
adm_2_phone_sim_dat <- left_join(adm_2_phone_sim_dat, adm_2_population_dat[, c(1, 3)], 
                                 by = c('adm_2_origin' = 'adm_2'))
adm_2_phone_sim_dat <- adm_2_phone_sim_dat %>% dplyr::rename('population' = 'population_2020_adm_2',
                                                             'origin_code' = 'origin',
                                                             'destiation_code' = 'destination',
                                                             'origin' = 'adm_2_origin',
                                                             'destination' = 'adm_2_destination')

# Administrative Level 1
adm_1_phone_sim_dat <- adm_1_phone_mobility_sim_dat %>% 
  dplyr::filter(origin == destination) %>%
  dplyr::mutate(level = '1')
adm_1_phone_sim_dat <- left_join(adm_1_phone_sim_dat, adm_1_population_dat[, c(1, 2)], 
                                 by = c('adm_1_origin' = 'adm_1'))
adm_1_phone_sim_dat <- adm_1_phone_sim_dat %>% dplyr::rename('population' = 'population_2020_adm_1',
                                                             'origin_code' = 'origin',
                                                             'destiation_code' = 'destination',
                                                             'origin' = 'adm_1_origin',
                                                             'destination' = 'adm_1_destination')

# Append admin data together
# Admin 1
adm_1_phone_dat$level <- 'Observed'
adm_1_phone_sim_dat <- adm_1_phone_sim_dat %>%
  select(-c(origin_code, destiation_code, value_cat))
adm_1_phone_sim_dat$level <- 'Simulated'
adm_1_phone_sim_dat <- adm_1_phone_sim_dat %>%
  select(-c('value.y', 'value_cat.y'))
mean(adm_1_phone_dat$value)
mean(adm_1_phone_sim_dat$value)
adm_1_plot <- rbind(adm_1_phone_dat, adm_1_phone_sim_dat)

# Admin 2
adm_2_phone_dat$level <- 'Observed'
adm_2_phone_sim_dat <- adm_2_phone_sim_dat %>%
  select(-c(origin_code, destiation_code, value_cat))
adm_2_phone_sim_dat$level <- 'Simulated'
adm_2_phone_sim_dat <- adm_2_phone_sim_dat %>%
  select(-c('value.y', 'value_cat.y'))
mean(adm_2_phone_dat$value)
mean(adm_2_phone_sim_dat$value)
adm_2_plot <- rbind(adm_2_phone_dat, adm_2_phone_sim_dat)

# Admin 3
adm_3_phone_dat$level <- 'Observed'
adm_3_phone_sim_dat <- adm_3_phone_sim_dat %>%
  select(-c(origin_code, destiation_code, value_cat))
adm_3_phone_sim_dat$level <- 'Simulated'
adm_3_phone_sim_dat <- adm_3_phone_sim_dat %>%
  select(-c('value.y', 'value_cat.y'))
mean(adm_3_phone_dat$value)
mean(adm_3_phone_sim_dat$value)
adm_3_plot <- rbind(adm_3_phone_dat, adm_3_phone_sim_dat)

# Create phone mobility data violin plots
phone_violin_plot_1 <- make_violin_plot(data = adm_1_plot, color = '#4292C6')
phone_violin_plot_2 <- make_violin_plot(data = adm_2_plot, color = '#41AE76')
phone_violin_plot_3 <- make_violin_plot(data = adm_3_plot, color = '#807DBA')

# Combine all data
adm_3_phone_dat$level <- 'Division'
adm_2_phone_dat$level <- 'District'
adm_1_phone_dat$level <- 'Province'
adm_phone_dat <- rbind(adm_1_phone_dat, adm_2_phone_dat, adm_3_phone_dat)

adm_3_phone_sim_dat$level <- 'Division'
adm_2_phone_sim_dat$level <- 'District'
adm_1_phone_sim_dat$level <- 'Province'
adm_phone_sim_dat <- rbind(adm_1_phone_sim_dat, adm_2_phone_sim_dat, adm_3_phone_sim_dat)

phone_scatter_plot <- make_scatter_plot(data = adm_phone_dat)
phone_scatter_plot_sim <- make_scatter_plot(data = adm_phone_sim_dat)

# CREATE FIGURES

# Create figure, with labels
internal_plots <- plot_grid(phone_violin_plot_1 + stat_summary(fun.data=data_summary) + ggtitle('Provinces'),
                            phone_violin_plot_2 + stat_summary(fun.data=data_summary) + ggtitle('Districts'),
                            phone_violin_plot_3 + stat_summary(fun.data=data_summary) + ggtitle('Divisions'),
                        nrow = 1,
                        rel_widths = c(1, 1, 1),
                        labels = c('(a)', 
                                   '(b)',
                                   '(c)'),
                        label_size = 26, hjust = 0)

internal_plots_pop <- plot_grid(phone_scatter_plot + ggtitle('\nObserved'),
                                phone_scatter_plot_sim + ggtitle('\nSimulated'),
                            nrow = 1,
                            rel_widths = c(1, 1),
                            labels = c('(d)', 
                                       '(e)'),
                            label_size = 26, hjust = 0)

internal_plots_final <- plot_grid(internal_plots, internal_plots_pop,
                                  nrow = 2)

# Save figure
ggsave('./figs/internal_prop_plot.jpg', plot = internal_plots_final, height = 12, width = 18)

##########################
# 5. CREATE FINAL FIGURE #
##########################
cluster_merge_2_1$Group <- 'Districts in \nProvinces'
cluster_merge_3_1$Group <- 'Divisions in \nProvinces'
cluster_merge_3_2$Group <- 'Divisions in \nDistricts'
mean(cluster_merge_3_1$diff)

clust <- rbind(cluster_merge_2_1, cluster_merge_3_1, cluster_merge_3_2)
mu <- clust %>% group_by(Group) %>%
  mutate(grp.mean = mean(diff)) %>%
  distinct(Group, grp.mean, .keep_all = FALSE)
cluster_plot <- ggplot(clust, aes(x=diff, color=Group, fill = Group)) +
  geom_density(aes(), alpha = 0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Group),
             linetype="dashed") + theme_minimal() + 
  xlab('Internal - External Trip Proportion Difference') + ylab('Density') +
  theme(legend.position = 'bottom')
  
cluster_plot

row_1.5 <- cowplot::plot_grid(map_2, map_3, 
                              ggplot() + theme_void(),
                              ggplot() + theme_void(),
                              nrow = 2, rel_heights = c(1,1),
                              labels = c('', '', ''),
                              label_size = 34, hjust = 0)

row_1.6 <- cowplot::plot_grid(ggplot() + theme_void(),
                              map,
                              nrow = 1, rel_widths = c(0.1,1),
                              labels = c('(a)', ''),
                              label_size = 34, hjust = 0)
  
row_1 <- cowplot::plot_grid(row_1.6, row_1.5, nrow = 1, rel_widths = c(1, 1),
                   labels = c('(a)', '(b)'),
                   label_size = 34, hjust = 0)

row_2 <- cowplot::plot_grid(internal_plots, nrow = 1,
                            rel_widths = c(1, 1),
                            labels = c('', ''),
                            label_size = 34, hjust = 0)



figure_2 <- cowplot::plot_grid(map,
                              internal_plots,
                              nrow = 1, rel_widths = c(1,0.6),
                              labels = c('(a)', ''),
                              label_size = 26, hjust = 0)

figure_3_sub <- cowplot::plot_grid(sub_1, sub_2, sub_3, nrow = 3,
                                   labels = c('(a) Province - District', '(b) Province - Division', '(c) District - Division'),
                                   label_size = 34, hjust = 0)


row_3 <- cowplot::plot_grid(external_plots, nrow = 1,
                   labels = c('(d)'),
                   label_size = 34, hjust = 0)

row_4 <- cowplot::plot_grid(test_3, test_2, test_1, nrow = 3,
                            labels = c('(e)'),
                            label_size = 34, hjust = 0)


figure_2 <- plot_grid(row_1, row_2, row_3,
                      nrow = 3, 
                      rel_heights = c(1, 0.7, 0.7),
                      labels = NULL, hjust = 0)

row_3.5 <- cowplot::plot_grid(ggplot() + theme_void(), 
                              ggplot() + theme_void(), 
                              ggplot() + theme_void(), nrow = 3)

figure_3 <- plot_grid(figure_3_sub, row_3, row_3.5, row_4,
                      nrow = 1, 
                      rel_widths = c(0.8, 0.5, 0.2, 0.5),
                      labels = NULL, hjust = 0)

ggsave('./figs/figure_2_test.jpg', plot = figure_2, height = 17, width = 25)
ggsave('./figs/figure_3_test.jpg', plot = figure_3, height = 20, width = 25)


mean(adm_3_adm_1_phone$diff)
mean(adm_3_adm_2_phone$diff)
mean(adm_2_adm_1_phone$diff)

min(adm_3_adm_1_phone$diff)
max(adm_3_adm_1_phone$diff)

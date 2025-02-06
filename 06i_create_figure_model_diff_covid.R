################################################################################
# File Name: 04i_create_figure_model_diff_covid                                #
#                                                                              #
# Purpose:   Create figures that describe differences between models with      #
#            various spatial levels of mobility data, comparing with COVID-19. #
#                                                                              #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load figure data and format                                    # 
#            3. Create spatial invasion comparison figure                      #
#            4. Create spatial invasion figure                                 #
#            5. Create epidemic magnitude figure                               #
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
library(ggridges)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

##################################
# 2. LOAD FIGURE DATA AND FORMAT #
##################################

# Load covid results
load('./tmp/covid_model_results.RData')

# Load observed covid introductions
load('./tmp/covid_introductions.RData')

# Format COVID-19 observed introductions
# Level 2 at Lavel 2
covid_dat_district_intro_5_2_2 <- covid_dat_district_intro_5 %>%
  dplyr::select(c(district, intro_time)) %>%
  dplyr::rename('adm' = 'district',
         'time' = 'intro_time') %>%
  mutate(Category = 'Level 2',
         Group = 'COVID-19')

# Level 2 at Lavel 1
covid_dat_province_intro_25_2_1 <- covid_dat_province_intro_25 %>%
  dplyr::select(c(adm_1, intro_time)) %>%
  dplyr::rename('time' = 'intro_time',
         'adm' = 'adm_1') %>%
  mutate(Category = 'Level 2 at Level 1',
         Group = 'COVID-19')

# Level 1 at Lavel 1
covid_dat_province_intro_25_1_1 <- covid_dat_province_intro_25_2_1
covid_dat_province_intro_25_1_1$Category <- 'Level 1'

# Format model COVID-19 results
# Level 2 at Level 2
mobile_phone_2_2 <- adm_2_avg_2_avg_list[[1]] %>%
  dplyr::select(c(adm_2, time_mean)) %>%
  dplyr::rename('time' = 'time_mean',
         'adm' = 'adm_2') %>%
  mutate(Category = 'Level 2',
         Group = 'Mobile Phone')

sim_2_2 <- adm_2_avg_2_avg_list[[2]] %>%
  dplyr::select(c(adm_2, time_mean)) %>%
  dplyr::rename('time' = 'time_mean',
                'adm' = 'adm_2') %>%
  mutate(Category = 'Level 2',
         Group = 'Simulated Mobile Phone')

none_2_2 <- adm_2_avg_2_avg_list[[3]] %>%
  dplyr::select(c(adm_2, time_mean)) %>%
  dplyr::rename('time' = 'time_mean',
         'adm' = 'adm_2') %>%
  mutate(Category = 'Level 2',
         Group = 'No Mobility')

# Level 2 at Level 1
mobile_phone_2_1 <- adm_2_avg_1_avg_list[[1]] %>%
  dplyr::select(c(adm_1, time_mean)) %>%
  dplyr::rename('time' = 'time_mean',
         'adm' = 'adm_1') %>%
  mutate(Category = 'Level 2 at Level 1',
         Group = 'Mobile Phone')

sim_2_1 <- adm_2_avg_1_avg_list[[2]] %>%
  dplyr::select(c(adm_1, time_mean)) %>%
  dplyr::rename('time' = 'time_mean',
                'adm' = 'adm_1') %>%
  mutate(Category = 'Level 2 at Level 1',
         Group = 'Simulated Mobile Phone')

none_2_1 <- adm_2_avg_1_avg_list[[3]] %>%
  dplyr::select(c(adm_1, time_mean)) %>%
  dplyr::rename('time' = 'time_mean',
         'adm' = 'adm_1') %>%
  mutate(Category = 'Level 2 at Level 1',
         Group = 'No Mobility')

# Level 1 at Level 1
mobile_phone_1_1 <- adm_1_avg_1_avg_list[[1]] %>%
  dplyr::select(c(adm_1, time_mean)) %>%
  dplyr::rename('time' = 'time_mean',
         'adm' = 'adm_1') %>%
  mutate(Category = 'Level 1',
         Group = 'Mobile Phone')

sim_1_1 <- adm_1_avg_1_avg_list[[2]] %>%
  dplyr::select(c(adm_1, time_mean)) %>%
  dplyr::rename('time' = 'time_mean',
                'adm' = 'adm_1') %>%
  mutate(Category = 'Level 1',
         Group = 'Simulated Mobile Phone')

none_1_1 <- adm_1_avg_1_avg_list[[3]] %>%
  dplyr::select(c(adm_1, time_mean)) %>%
  dplyr::rename('time' = 'time_mean',
         'adm' = 'adm_1') %>%
  mutate(Category = 'Level 1',
         Group = 'No Mobility')

# Create comparison dataset for results at level 2
comparison_dat_2 <- rbind(mobile_phone_2_2, sim_2_2, none_2_2)
# Merge on observed COVID-19 information
comparison_dat_2 <- left_join(comparison_dat_2, covid_dat_district_intro_5_2_2[, c(1, 2)],
                              by = c('adm' = 'adm'))
# Calculate spatial invasion differences
comparison_dat_2$time_diff <- comparison_dat_2$time.x - comparison_dat_2$time.y

# Create comparison dataset for results at level 1
comparison_dat_1 <- rbind(mobile_phone_2_1, mobile_phone_1_1, 
                          sim_2_1, sim_1_1,
                          none_2_1, none_1_1)
# Merge on observed COVID-19 information
comparison_dat_1 <- left_join(comparison_dat_1, covid_dat_province_intro_25_2_1[, c(1, 2)],
                              by = c('adm' = 'adm'))
comparison_dat_1$time_diff <- comparison_dat_1$time.x - comparison_dat_1$time.y

# Add just level 1 data to comparison dataset
comparison_dat <- rbind(comparison_dat_1)

################################################
# 3. CREATE SPATIAL INVASION COMPARISON FIGURE #
################################################

# Load population information
load('./tmp/adm_population_dat.RData')
# Join population data to COVID-19 comparison data
comparison_dat <- left_join(comparison_dat, adm_1_population_dat, by = c('adm' = 'adm_1'))

# Set plot theme
theme_plot <- theme(axis.title = element_text(size=26),
                    axis.text = element_text(size=22),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.text = element_text(size = 26),
                    legend.title = element_text(size = 26),
                    plot.title = element_text(size=30, hjust = 0.5))

# Create sub plots
# Comparison by timing
plot_1 <- ggplot(data = comparison_dat[comparison_dat$Group == "Mobile Phone Data",]) +
  geom_point(aes(x = time.y, y = time_diff, color = Category), alpha = 0.5, size = 5) +
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = time.y, y = time_diff, color = Category), method = "lm", se = FALSE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('True Introduction Time') + theme_plot +
  scale_color_manual(values=c("#41AE76", "#DF65B0")) + theme(legend.position = 'bottom') 

# Comparison by population
plot_2 <- ggplot(data = comparison_dat[comparison_dat$Group == "Mobile Phone Data",]) +
  geom_point(aes(x = log(population_2020_adm_1), y = time_diff, color = Category), alpha = 0.5, size = 5) +
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = log(population_2020_adm_1), y = time_diff, color = Category), method = "lm", se = FALSE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Log Population Size') + theme_plot +
  scale_color_manual(values=c("#41AE76", "#DF65B0")) + theme(legend.position = 'bottom') 

# Combine figures
figure <- plot_grid(plot_1 + theme(legend.position = 'none'),
                    plot_2 + theme(legend.position = 'none'),
                    nrow = 1,
                    labels = c('(a)', '(b)'),
                    label_size = 26, hjust = 0)

# Create legend
legend <- get_legend(ggplot(data = comparison_dat[comparison_dat$Group == "Mobile Phone Data",]) +
                       geom_point(aes(x = log(population_2020_adm_1), y = time_diff, color = Category), alpha = 0.5, size = 5) +
                       geom_hline(aes(yintercept = 0), color = 'black', linetype = 5) +
                       geom_smooth(aes(x = log(population_2020_adm_1), y = time_diff, color = Category), method = "lm", se = FALSE, linewidth = 5) + 
                       theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Log Population Size') + theme_plot +
                       scale_color_manual(values=c("#41AE76", "#DF65B0")) + theme(legend.position = 'bottom') )

# Add legend to figure
figure_combine <- plot_grid(figure,
                            legend,
                            nrow = 2,
                            rel_heights = c(1, 0.1))

# Save figure
ggsave('./figs/figure_covid_statial_inv_comp.jpg', plot = figure_combine, height = 11, width = 25)

#####################################
# 4. CREATE SPATIAL INVASION FIGURE #
#####################################

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
                                                                                        
# Calculate RMSE Values
num <- comparison_dat[comparison_dat$Category == 'Level 2 at Level 1' & comparison_dat$Group == 'Mobile Phone', c(6)]
mp_2_1 <- sqrt(mean(num$time_diff^2))

num <- comparison_dat[comparison_dat$Category == 'Level 2 at Level 1' & comparison_dat$Group == 'No Mobility', c(6)]
nm_2_1 <- sqrt(mean(num$time_diff^2))

num <- comparison_dat[comparison_dat$Category == 'Level 2 at Level 1' & comparison_dat$Group == 'Simulated Mobile Phone', c(6)]
sim_2_1 <- sqrt(mean(num$time_diff^2))

num <- comparison_dat[comparison_dat$Category == 'Level 1' & comparison_dat$Group == 'Mobile Phone', c(6)]
mp_1_1 <- sqrt(mean(num$time_diff^2))

num <- comparison_dat[comparison_dat$Category == 'Level 1' & comparison_dat$Group == 'No Mobility', c(6)]
nm_1_1 <- sqrt(mean(num$time_diff^2))

num <- comparison_dat[comparison_dat$Category == 'Level 1' & comparison_dat$Group == 'Simulated Mobile Phone', c(6)]
sim_1_1 <- sqrt(mean(num$time_diff^2))

# Merge to maps
merge <- comparison_dat[comparison_dat$Category == 'Level 2 at Level 1' & comparison_dat$Group == 'Mobile Phone',]
merge <- merge %>% rename('mp_2_1' = 'time_diff')
choropleth_1 <- left_join(choropleth_1, merge[, c(1, 6)], by = c('ADM1_EN' = 'adm'))

merge <- comparison_dat[comparison_dat$Category == 'Level 2 at Level 1' & comparison_dat$Group == 'No Mobility',]
merge <- merge %>% rename('nm_2_1' = 'time_diff')
choropleth_1 <- left_join(choropleth_1, merge[, c(1, 6)], by = c('ADM1_EN' = 'adm'))

merge <- comparison_dat[comparison_dat$Category == 'Level 2 at Level 1' & comparison_dat$Group == 'Simulated Mobile Phone',]
merge <- merge %>% rename('sim_2_1' = 'time_diff')
choropleth_1 <- left_join(choropleth_1, merge[, c(1, 6)], by = c('ADM1_EN' = 'adm'))

merge <- comparison_dat[comparison_dat$Category == 'Level 1' & comparison_dat$Group == 'Mobile Phone',]
merge <- merge %>% rename('mp_1_1' = 'time_diff')
choropleth_1 <- left_join(choropleth_1, merge[, c(1, 6)], by = c('ADM1_EN' = 'adm'))

merge <- comparison_dat[comparison_dat$Category == 'Level 1' & comparison_dat$Group == 'No Mobility',]
merge <- merge %>% rename('nm_1_1' = 'time_diff')
choropleth_1 <- left_join(choropleth_1, merge[, c(1, 6)], by = c('ADM1_EN' = 'adm'))

merge <- comparison_dat[comparison_dat$Category == 'Level 1' & comparison_dat$Group == 'Simulated Mobile Phone',]
merge <- merge %>% rename('sim_1_1' = 'time_diff')
choropleth_1 <- left_join(choropleth_1, merge[, c(1, 6)], by = c('ADM1_EN' = 'adm'))

# Create RMSE dataset
`Mobility Source` <- c('Mobile Phone Data', 'Simulated Mobile Phone Data', 'No Mobility Data',
                       'Mobile Phone Data', 'Simulated Mobile Phone Data', 'No Mobility Data')
Category <- c('Level 2 at Level 1', 'Level 2 at Level 1', 'Level 2 at Level 1',
              'Level 1', 'Level 1', 'Level 1')
order <- c(2, 2, 2, 1, 1, 1)
RMSE <- c(mp_2_1, sim_2_1, nm_2_1, mp_1_1, sim_1_1, nm_1_1)
rmse_dat <- data.frame(`Mobility Source`, Category, RMSE, order)

# Alter data for graphic
comparison_dat$`Mobility Source` <- comparison_dat$Group
comparison_dat$order <- ifelse(comparison_dat$Category == 'Level 2 at Level 1', 2, 0)
comparison_dat$order <- ifelse(comparison_dat$Category == 'Level 1', 1, comparison_dat$order)
brewer.pal(9, 'PuRd')

map_1 <- ggplot() +
  geom_sf(data = choropleth_1, aes(fill = mp_2_1), color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('Mobile Phone, District Level') + theme(legend.position = 'none',
                                                                        plot.title = element_text(size = 30, hjust = 0.5),
                                                                        panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                        legend.title = element_text(size = 24),
                                                                        legend.text = element_text(size = 24),
                                                                        legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#DF65B0",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-5, 55))

map_2 <- ggplot() +
  geom_sf(data = choropleth_1, aes(fill = nm_2_1), color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('No Mobility, District Level') + theme(legend.position = 'none',
                                                                      plot.title = element_text(size = 30, hjust = 0.5),
                                                                      panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                      legend.title = element_text(size = 24),
                                                                      legend.text = element_text(size = 24),
                                                                      legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#DF65B0",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-5, 55))
                                                                                 
map_3 <- ggplot() +
  geom_sf(data = choropleth_1, aes(fill = sim_2_1), color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('Simulated Mobile Phone, District Level') + theme(legend.position = 'none',
                                                                      plot.title = element_text(size = 30, hjust = 0.5),
                                                                      panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                      legend.title = element_text(size = 24),
                                                                      legend.text = element_text(size = 24),
                                                                      legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#DF65B0",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-5, 55))

map_4 <- ggplot() +
  geom_sf(data = choropleth_1, aes(fill = mp_1_1), color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('Mobile Phone, Province Level') + theme(legend.position = 'none',
                                                                      plot.title = element_text(size = 30, hjust = 0.5),
                                                                      panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                      legend.title = element_text(size = 24),
                                                                      legend.text = element_text(size = 24),
                                                                      legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#DF65B0",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-5, 55))

map_5 <- ggplot() +
  geom_sf(data = choropleth_1, aes(fill = nm_1_1), color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('No Mobility, Province Data') + theme(legend.position = 'none',
                                                                      plot.title = element_text(size = 30, hjust = 0.5),
                                                                      panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                      legend.title = element_text(size = 24),
                                                                      legend.text = element_text(size = 24),
                                                                      legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#DF65B0",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-5, 55))

map_6 <- ggplot() +
  geom_sf(data = choropleth_1, aes(fill = sim_1_1), color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('Simulated Mobile Phone, Province Level') + theme(legend.position = 'none',
                                                                      plot.title = element_text(size = 30, hjust = 0.5),
                                                                      panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                      legend.title = element_text(size = 24),
                                                                      legend.text = element_text(size = 24),
                                                                      legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#DF65B0",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-5, 55))


legend <- ggplot() +
  geom_sf(data = choropleth_1, aes(fill = sim_1_1), color= 'black', linewidth = 1.5) +
  geom_point(data = coordinate_cities, aes(x = long, y = lat), colour="black", fill = 'black', size= 6, alpha = 1, shape=21) +
  geom_label(data = coordinate_cities, aes(x = long - 0.10 , y = lat + 0.14 , label = city), size = 10, fontface = 'bold') +
  theme_void() + ggtitle('Simulated Mobile Phone, Province Level') + theme(legend.position = 'bottom',
                                                                           plot.title = element_text(size = 30, hjust = 0.5),
                                                                           panel.border = element_rect(fill=NA, linewidth = 0.8, color = 'white'),
                                                                           legend.title = element_text(size = 24),
                                                                           legend.text = element_text(size = 24),
                                                                           legend.key.size = unit(1.2, 'cm')) + 
  labs(fill = 'Spatial Invasion\nDifference (days)') + scale_fill_gradient2(
    low = "#4292C6",
    mid = "white",
    high = "#DF65B0",
    space = "Lab",
    midpoint = 0,
    na.value = "gray70",
    guide = "colourbar",
    aesthetics = "fill",
    limits = c(-5, 55)) +
  guides(fill = guide_colorbar(barwidth = 50, barheight = 2))

legend_get <- get_legend(legend)

figure_3 <- plot_grid(map_1, map_2, map_3, map_4, map_5, map_6,
                      nrow = 1, 
                      label_size = 34)

figure_3.5 <- plot_grid(figure_3, legend_get,
                        nrow = 2, 
                        rel_heights = c(1, 0.12))

# Create spatial invasion difference distribution figure
plot_1 <- ggplot(comparison_dat, aes(y = reorder(Category, order), x = time_diff, fill = `Mobility Source`)) + 
  geom_density_ridges(alpha = 0.6, rel_min_height = 0.01, scale = 0.8, color = 'black', size = 1.5) + 
  xlim(-10, 70) + theme_minimal() +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed') +
  scale_fill_manual('Mobility Source', values = c('#F16913', 'darkgray', '#41AB5D')) +
  xlab('Spatial Invasion Difference (days)\n') + 
  ylab('Spatial Resolution Level\n') +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 26),
        plot.title = element_text(size=26, hjust = 0.5),
        legend.key.size = unit(1, 'cm'))

# Create RMSE and Spearman datasets
# Level 2-Level 1
# Create empty objects to fill
spearman_2_1_list <- NULL
rmse_2_1_list <- NULL
# Set counter
count <- 0
# Run through each mobility assumption
for (i in c(1, 2, 3)) {
  count <- count + 1
  
  data <- adm_2_avg_1_run_list[[i]]
  data <- left_join(data, covid_dat_province_intro_25_2_1[, c(1, 2)],
                    by = c('adm_1' = 'adm'))
  data$time_diff_sq <- (data$time.x - data$time.y)^2
  
  spearman <- data %>% group_by(run_num) %>%
    mutate(rmse = sqrt(mean(time_diff_sq)),
           rank.x = min_rank(time.x), 
           rank.y = min_rank(time.y),
           sp_corr = cor(rank.x, rank.y, method = 'spearman')) %>%
    distinct(run_num, rmse, sp_corr, .keep_all = FALSE) %>%
    ungroup() %>%
    mutate(
      `Mobility Source` = ifelse(i == 1, 'Mobile Phone', ifelse(i == 2, 'Simulated Mobile Phone', 'No Mobility')),
      Category = 'Level 2 at Level 1')
  rmse <- spearman %>% 
    mutate(mean_rmse = mean(rmse),
           rmse_975 = quantile(rmse, probs = 0.975),
           rmse_025 = quantile(rmse, probs = 0.025)) %>%
    distinct(mean_rmse, rmse_975, rmse_025, `Mobility Source`, Category)
  # Fill object
  spearman_2_1_list[[count]] <- spearman
  rmse_2_1_list[[count]] <- rmse
}

# Level 1
# Create empty objects to fill
spearman_1_1_list <- NULL
rmse_1_1_list <- NULL
# Set counter
count <- 0
for (i in c(1, 2, 3)) {
  count <- count + 1
  
  data <- adm_1_avg_1_run_list[[i]]
  data <- left_join(data, covid_dat_province_intro_25_1_1[, c(1, 2)],
                    by = c('adm_1' = 'adm'))
  data$time_diff_sq <- (data$time.x - data$time.y)^2
  
  spearman <- data %>% group_by(run_num) %>%
    mutate(rmse = sqrt(mean(time_diff_sq)),
           rank.x = min_rank(time.x), 
           rank.y = min_rank(time.y),
           sp_corr = cor(rank.x, rank.y, method = 'spearman')) %>%
    distinct(run_num, rmse, sp_corr, .keep_all = FALSE) %>%
    ungroup() %>%
    mutate(
      `Mobility Source` = ifelse(i == 1, 'Mobile Phone', ifelse(i == 2, 'Simulated Mobile Phone', 'No Mobility')),
      Category = 'Level 1')
  rmse <- spearman %>% 
    mutate(mean_rmse = mean(rmse),
           rmse_975 = quantile(rmse, probs = 0.975),
           rmse_025 = quantile(rmse, probs = 0.025)) %>%
    distinct(mean_rmse, rmse_975, rmse_025, `Mobility Source`, Category)
  # Fill objects
  spearman_1_1_list[[count]] <- spearman
  rmse_1_1_list[[count]] <- rmse
}


# Level 2
spearman_2_2_list <- NULL
rmse_2_2_list <- NULL
count <- 0
for (i in c(1, 2, 3)) {
  count <- count + 1
  
  data <- adm_2_avg_2_run_list[[i]]
  data <- left_join(data, covid_dat_district_intro_5_2_2[, c(1, 2)],
                    by = c('adm_2' = 'adm'))
  data$time_diff_sq <- (data$time.x - data$time.y)^2
  
  spearman <- data %>% group_by(run_num) %>%
    mutate(rmse = sqrt(mean(time_diff_sq)),
           sp_corr = cor(time.x, time.y, method = 'spearman')) %>%
    distinct(run_num, rmse, sp_corr, .keep_all = FALSE) %>%
    ungroup() %>%
    mutate(
      `Mobility Source` = ifelse(i == 1, 'Mobile Phone', ifelse(i == 2, 'Simulated Mobile Phone', 'No Mobility')),
      Category = 'Level 2')
  rmse <- spearman %>% 
    mutate(mean_rmse = mean(rmse),
           rmse_975 = quantile(rmse, probs = 0.975),
           rmse_025 = quantile(rmse, probs = 0.025)) %>%
    distinct(mean_rmse, rmse_975, rmse_025, `Mobility Source`, Category)
  
  spearman_2_2_list[[count]] <- spearman
  rmse_2_2_list[[count]] <- rmse
}

# Combine RMSE data
rmse_line_dat <- rbind(rmse_2_1_list[[1]], rmse_2_1_list[[2]], rmse_2_1_list[[3]],
                  rmse_1_1_list[[1]], rmse_1_1_list[[2]], rmse_1_1_list[[3]])
rmse_line_dat$order <- c(2, 2, 2, 1, 1, 1)

brewer.pal(9, 'Greens')

# Create RMSE figure
plot_2 <- ggplot(rmse_line_dat, aes(color = `Mobility Source`, y = mean_rmse, x = reorder(Category, order))) + 
  geom_point(position=position_dodge(.5), size = 6, alpha = 0.8) + theme_minimal() +
  geom_errorbar(aes(ymin=rmse_025, ymax=rmse_975),
                size= 4,   
                width=.3,
                alpha = 0.8,
                position=position_dodge(.5)) +
  ylim(0, 40) + 
  ylab('RMSE') +
  xlab('Spatial Resolution Level') +
  scale_color_manual(values = c('#F16913', 'darkgray', '#41AB5D')) + 
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=26, hjust = 0.5))

# Combine Spearman data
spearman_dat <- rbind(spearman_1_1_list[[1]], spearman_1_1_list[[2]], spearman_1_1_list[[3]],
                  spearman_2_1_list[[1]], spearman_2_1_list[[2]], spearman_2_1_list[[3]])
spearman_dat$order <- ifelse(spearman_dat$Category == 'Level 2 at Level 1', 2, 0)
spearman_dat$order <- ifelse(spearman_dat$Category == 'Level 1', 1, spearman_dat$order)

# Data summary function, mean +/- one sd
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y = m,ymin = ymin, ymax = ymax))
}

# Create Spearman figure
plot_3 <- ggplot(spearman_dat, aes(fill = `Mobility Source`, y = sp_corr, x = reorder(Category, order))) + 
  geom_violin(position=position_dodge(.8), alpha = 0.8, color = 'black', linewidth = 1.5, scale="width", width = 0.6) + theme_minimal() +
  stat_summary(fun.data=data_summary, position=position_dodge(.8)) +
  ylim(0, 1) + 
  xlab(' ') + 
  ylab('Spearman Correlation') +
  xlab('Spatial Resolution Level') +
  scale_fill_manual(values = c('#F16913', 'darkgray', '#41AB5D')) + 
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 26),
        plot.title = element_text(size=26, hjust = 0.5))

# Paper callouts
spearman_dat %>% group_by(`Mobility Source`, Category) %>% summarise(mean = mean(sp_corr))


# Grab legend
legend <- get_legend(plot_1)

# Combine figures
figure <- plot_grid(plot_2 + ggtitle(' ') + theme(legend.position = "none"), 
                    plot_3 + ggtitle(' ') + theme(legend.position = "none"),
                    nrow = 1,
                    labels = c('(b)', '(c)'),
                    label_size = 26, hjust = 0)

figure_combined <- plot_grid(plot_1 + theme(legend.position = "none") + ggtitle(' '), 
                             figure,
                             legend,
                             nrow = 3,
                             rel_heights = c(1, 1, 0.1),
                             labels = c('(a)', ''),
                             label_size = 26, hjust = 0)

# Save figure
ggsave('./figs/figure_covid_spatial_inv.jpg', plot = figure_combined, height = 20, width = 25)

# Create poster plot
plot_1 <- ggplot(comparison_dat, aes(y = reorder(Category, order), x = time_diff, fill = `Mobility Source`)) + 
  geom_density_ridges(alpha = 0.7, rel_min_height = 0.01, scale = 0.8) + 
  xlim(-30, 40) + theme_minimal() +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', linewidth = 2) +
  scale_fill_manual('Mobility Source', values = c('#F16913', '#4292C6')) +
  xlab('Spatial Invasion Difference (days)') + 
  ylab('Spatial Aggregation Level') +
  ggtitle('') +
  theme(axis.title = element_text(size=50),
        axis.text = element_text(size=35),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.text = element_text(size = 34),
        legend.title = element_text(size = 34),
        plot.title = element_text(size=54, hjust = 0.5))


plot_2 <- ggplot(rmse_line_dat, aes(color = `Mobility Source`, y = mean_rmse, x = reorder(Category, order))) + 
  geom_point(position=position_dodge(.5), size = 5, alpha = 0.75) + theme_minimal() +
  geom_errorbar(aes(ymin=rmse_025, ymax=rmse_975),
                size= 3,   
                width=.2,
                alpha = 0.7,
                position=position_dodge(.5)) +
  ylim(0, 25) + 
  ylab('RMSE') +
  xlab('Spatial Aggregation Level') +
  scale_color_manual(values = c('#F16913', '#4292C6')) + 
  theme(axis.title = element_text(size=50),
        axis.text = element_text(size=35),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 34),
        legend.title = element_text(size = 34),
        plot.title = element_text(size=54, hjust = 0.5))

legend <- get_legend(ggplot(comparison_dat, aes(y = reorder(Category, order), x = time_diff, fill = `Mobility Source`)) + 
                       geom_density_ridges(alpha = 0.75, rel_min_height = 0.01, scale = 0.8) + 
                       xlim(-30, 40) + theme_minimal() +
                       geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', linewidth = 2) +
                       scale_fill_manual('Mobility Source', values = c('#F16913', '#4292C6')) +
                       xlab('Arrival Time Difference (days)') + 
                       ylab('Spatial Aggregation Level') +
                       ggtitle('') +
                       theme(axis.title = element_text(size=50),
                             axis.text = element_text(size=35),
                             panel.grid.major.x = element_blank(),
                             panel.grid.minor = element_blank(),
                             legend.position = 'bottom',
                             legend.text = element_text(size = 34),
                             legend.title = element_text(size = 34),
                             plot.title = element_text(size=54, hjust = 0.5)))

figure <- plot_grid(plot_1 + theme(legend.position = "none") + ggtitle('Arrival Time Difference Distribution'),
                    plot_2 + ggtitle('RMSE by Spatial Scale and Model') + theme(legend.position = "none"),
                    nrow = 1, rel_widths = c(1, 0.7))

figure_combined <- plot_grid(figure, legend, rel_heights = c(1, 0.1),
                             nrow = 2)

ggsave('./figs/figure_covid_spatial_inv_poster.jpg', plot = figure_combined, height = 10, width = 32)

#######################################
# 4. CREATE EPIDEMIC MAGNITUDE FIGURE #
#######################################

# Load timeseries data for observed and simulated data
load('./tmp/covid_model_timeseries_results.RData')
load('./tmp/covid_data_province_district.RData')

# Filter observed data to relevent time period
covid_dat_district_filt <- covid_dat_district %>%
  filter(date > '2021-04-08') %>% group_by(district) %>%
  mutate(time = seq(1, 51, 1)) %>%
  filter(time < 51) %>%
  dplyr::rename('covid_cases' = 'incident_cases')

covid_dat_province_filt <- covid_dat_province %>%
  filter(date > '2021-04-08') %>% group_by(adm_1) %>%
  mutate(time = seq(1, 51, 1)) %>%
  filter(time < 51)%>%
  dplyr::rename('covid_cases' = 'adm_1_avg')
  
# Merge observed data to simulated data at the province level
adm_1_mobile_avg <- left_join(adm_1_mobile_avg, covid_dat_province_filt,
                               by = c('adm_1' = 'adm_1', 
                                      'time' = 'time'))

# Calculate differences and RMSE
adm_1_mobile_avg$diff <- adm_1_mobile_avg$covid_cases - adm_1_mobile_avg$avg_incid_i
rmse_1_mobile <- sqrt(sum(adm_1_mobile_avg$diff^2))
adm_1_mobile_avg$Province <- adm_1_mobile_avg$adm_1

# Create sum plots
# Province-level differences, assuming mobility
plot_1 <- ggplot(adm_1_mobile_avg) +
  geom_line(aes(x = as.Date(date), y = diff, color = Province), alpha = 0.9, linewidth = 1.5) +
  theme_minimal() + xlab("Date\n") + ylab("Difference in Incident Cases") + ylim(-1000, 1000) +
  scale_x_date(breaks = as.Date(c( "2021-04-15", "2021-05-01", "2021-05-15")),
               date_labels = "%b %d") +
  annotate("text", x= as.Date("2021-05-20"), y=950, label= "RMSE = 2,912.276", size = 9) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')  

# Merge observed data to simulated data at the province level
adm_1_none_avg <- left_join(adm_1_none_avg, covid_dat_province_filt,
                              by = c('adm_1' = 'adm_1', 
                                     'time' = 'time'))

# Calculate differences and RMSE
adm_1_none_avg$diff <- adm_1_none_avg$covid_cases - adm_1_none_avg$avg_incid_i
rmse_1_none <- sqrt(sum(adm_1_none_avg$diff^2))

# Province-level differences, assuming no mobility
plot_2 <- ggplot(adm_1_none_avg) +
  geom_line(aes(x = as.Date(date), y = diff, color = adm_1), alpha = 0.9, linewidth = 1.5) +
  theme_minimal() + xlab("Date\n") + ylab("Difference in Incident Cases") + ylim(-1000, 1000) +
  scale_x_date(breaks = as.Date(c( "2021-04-15", "2021-05-01", "2021-05-15")),
               date_labels = "%b %d") +
  annotate("text", x= as.Date("2021-05-20"), y=950, label= "RMSE = 4,027.259", size = 9) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')  

# Merge observed data to simulated data at the province level
adm_2_1_mobile_avg <- left_join(adm_2_1_mobile_avg, covid_dat_province_filt,
                              by = c('adm_1' = 'adm_1', 
                                     'time' = 'time'))

# Calculate differences and RMSE
adm_2_1_mobile_avg$diff <- adm_2_1_mobile_avg$covid_cases - adm_2_1_mobile_avg$sum_incid_i
rmse_2_1_mobile <- sqrt(sum(adm_2_1_mobile_avg$diff^2))

# District-level differences, assuming mobility
plot_3 <- ggplot(adm_2_1_mobile_avg) +
  geom_line(aes(x = as.Date(date), y = diff, color = adm_1), alpha = 0.9, linewidth = 1.5) +
  theme_minimal() + xlab("Date\n") + ylab("Difference in Incident Cases") + ylim(-1000, 1000) +
  scale_x_date(breaks = as.Date(c( "2021-04-15", "2021-05-01", "2021-05-15")),
               date_labels = "%b %d") +
  annotate("text", x= as.Date("2021-05-20"), y=950, label= "RMSE = 3,242.738", size = 9) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')  

# Merge observed data to simulated data at the province level
adm_2_1_none_avg <- left_join(adm_2_1_none_avg, covid_dat_province_filt,
                            by = c('adm_1' = 'adm_1', 
                                   'time' = 'time'))

# Calculate differences and RMSE
adm_2_1_none_avg$diff <- adm_2_1_none_avg$covid_cases - adm_2_1_none_avg$sum_incid_i
rmse_2_1_none <- sqrt(sum(adm_2_1_none_avg$diff^2))

# District-level differences, assuming no mobility
plot_4 <- ggplot(adm_2_1_none_avg) +
  geom_line(aes(x = as.Date(date), y = diff, color = adm_1), alpha = 0.9, linewidth = 1.5) +
  theme_minimal() + xlab("Date\n") + ylab("Difference in Incident Cases") + ylim(-1000, 1000) +
  scale_x_date(breaks = as.Date(c( "2021-04-15", "2021-05-01", "2021-05-15")),
               date_labels = "%b %d") +
  annotate("text", x= as.Date("2021-05-20"), y=950, label= "RMSE = 4,019.545", size = 9) +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')  

# Create legend
legend <- get_legend(ggplot(adm_1_mobile_avg) +
                       geom_line(aes(x = as.Date(date), y = diff, color = Province), alpha = 0.9, linewidth = 5) +
                       theme_minimal() + xlab("Date\n") + ylab("Difference in Incident Cases") + ylim(-1000, 1000) +
                       scale_x_date(breaks = as.Date(c( "2021-04-15", "2021-05-01", "2021-05-15")),
                                    date_labels = "%b %d") +
                       annotate("text", x= as.Date("2021-05-20"), y=950, label= "RMSE = 2,912.276", size = 9) +
                       theme(axis.title = element_text(size=26),
                             axis.text = element_text(size=22),
                             panel.grid.major.x = element_blank(),
                             legend.text = element_text(size = 22),
                             legend.title = element_text(size = 22),
                             plot.title = element_text(size=30, hjust = 0.5),
                             panel.grid.minor = element_blank(),
                             legend.position = 'bottom')  )

# Create figure
figure <- plot_grid(plot_1 + ggtitle('Administrative Level 1, Mobility Data') + theme(legend.position = "none"), 
                       plot_2 + ggtitle('Administrative Level 1, No Mobility Data') + theme(legend.position = "none"),
                       plot_3 + ggtitle('Administrative Level 2, Mobility Data') + theme(legend.position = "none"), 
                       plot_4 + ggtitle('Administrative Level 2, No Mobility Data') + theme(legend.position = "none"),
                       nrow = 2,
                       labels = c('(a)', '(b)', '(c)', '(d)'),
                       label_size = 26, hjust = 0)

# Add legend
figure_combined <- plot_grid(figure,
                        legend,
                        nrow = 2,
                        rel_heights = c(1, 0.05))

# Save figure
ggsave('./figs/figure_covid_epi_madnitude.jpg', plot = figure_combined, height = 20, width = 25)

################################################################################
################################################################################

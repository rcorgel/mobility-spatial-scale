library(osmdata)
library(ggplot2)

kandy <- matrix(data = c(80.62463, 80.64857, 7.28427, 7.30483),
                    nrow = 2,
                    byrow = TRUE)
# Update column and row names
colnames(kandy) <- c("min", "max")
rownames(kandy) <- c("x", "y")
# Print the matrix to the console
kandy

kandy_major <- getbb("Kandy") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = c("highway")) %>%
  osmdata_sf()

kandy_rail <- getbb("Kandy") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = c("railway")) %>%
  osmdata_sf()

kandy_place <- getbb("Kandy") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = c("building")) %>%
  osmdata_sf()

kandy_natural <- getbb("Kandy") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = "natural") %>%
  osmdata_sf()

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
# Print the plot
street_plot

ggsave('./figs/kandy_map.jpg', plot = street_plot, height = 16, width = 16)




# Load libraries
library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(sf)
library(scales)
library(reshape2)
library(ggpubr)

# Set the seed
set.seed(1234)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load model 
source('./mobility-spatial-scale/04_metapop_model.R')

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

choropleth_2$color <- 'A'
choropleth_2$color[4] <- 'B'
choropleth_2$color[5] <- 'C'
choropleth_2$color[6] <- 'D'


sl_major <- getbb("Sri Lanka") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = c("highway")) %>%
  osmdata_sf()

sl_rail <- getbb("Sri Lanka") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = c("railway")) %>%
  osmdata_sf()

# Main map
map <- ggplot() +
  geom_sf(data = choropleth_0, aes(group = ADM0_EN), fill = '#DDBAA4', color= 'black', linewidth = 1.3, alpha = 0.85) +
  geom_sf(data = choropleth_2[c(4, 5, 6),], aes(fill = color, group = ADM2_EN), linewidth = 1.3, color= 'black') +
  geom_sf(data = sl_major$osm_lines,
          color = "black",
          size = 0.005,
          alpha = 0.1) + 
  theme_void() + theme(legend.position = "none") +
  scale_fill_manual(values = c("#ABDDA4", "#A4C7DD","#D6A4DD")) + 
  coord_sf(ylim = c(6.78, 7.98), xlim = c(80.12, 81.32), expand = TRUE)
 
#coord_sf(xlim = c(80.63, 80.64), ylim = c(7.29, 7.30))



ggsave('./figs/test_map.jpg', plot = map, height = 16, width = 13)



map <- ggplot() +
  geom_sf(data = choropleth_0, aes(group = ADM0_EN), fill = '#DDBAA4', color= 'black', linewidth = 1.0, alpha = 0.85) +
  geom_sf(data = sl_major$osm_lines,
          color = "black",
          size = 0.005,
          alpha = 0.1) + 
  theme_void() + theme(legend.position = "none") +
  scale_fill_manual(values = c("#ABDDA4", "#A4C7DD","#D6A4DD")) 

ggsave('./figs/full_map.jpg', plot = map, height = 16, width = 13)

map <- ggplot() +
  geom_sf(data = choropleth_0, aes(group = ADM0_EN), fill = '#DDBAA4', color= 'black', linewidth = 1.3, alpha = 0.85) +
  geom_sf(data = choropleth_2[c(4, 5, 6),], aes(fill = color, group = ADM2_EN), linewidth = 1.3, color= 'black') +
  geom_sf(data = sl_major$osm_lines,
          color = "black",
          size = 0.005,
          alpha = 0.1) + 
  theme_void() + theme(legend.position = "none") +
  scale_fill_manual(values = c("#ABDDA4", "#A4C7DD","#D6A4DD")) 

#coord_sf(xlim = c(80.63, 80.64), ylim = c(7.29, 7.30))



ggsave('./figs/full_2_map.jpg', plot = map, height = 16, width = 13)


map <- ggplot() +
  geom_sf(data = choropleth_0, aes(group = ADM0_EN), fill = '#DDBAA4', color= 'black', linewidth = 1.0, alpha = 0.85) +
  theme_void() + theme(legend.position = "none") +
  scale_fill_manual(values = c("#ABDDA4", "#A4C7DD","#D6A4DD")) 

ggsave('./figs/full_map_3.jpg', plot = map, height = 16, width = 13)

map <- ggplot() +
  geom_sf(data = choropleth_0, aes(group = ADM0_EN), fill = '#FFFFFF00', color= 'black', linewidth = 1.0, alpha = 0.85) +
  geom_sf(data = choropleth_4, aes(group = ADM4_EN), fill = '#DDBAA4', color= 'black', linewidth = 0.2, alpha = 0.85) +
  theme_void() + theme(legend.position = "none") +
  scale_fill_manual(values = c("#ABDDA4", "#A4C7DD","#D6A4DD")) 

ggsave('./figs/full_map_4.jpg', plot = map, height = 16, width = 13)



# First, load metapopulation model data at different scales
load('./tmp/adm_2_metapop_dat.RData')

# Load simulated mobility data
load('./mobility-spatial-scale/simulated data/mobile_phone_sim_prop_dat.RData')


mobility_dat_adm_2 <- list(adm_2_phone_mobility_mat, adm_2_phone_pred_mobility_mat)


adm_2_obs_col <- run_seir_model_multi(n = 50, density_dep = FALSE, method = 'append',
                                      R_0 = 1.85, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                      adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                      pop_vec = adm_2_pop_vec, intro_adm = 'Colombo', intro_num = 1,
                                      adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_2[[1]], 
                                      max_time = 365, time_step = 1, mobility = TRUE)

adm_2_at_2_obs_col <- adm_2_obs_col %>%
  group_by(time, adm_2) %>%
  mutate(avg_incid_I_adm_2 = mean(incid_I)) %>%
  distinct(time, adm_2, avg_incid_I_adm_2) %>% 
  ungroup() 


adm_2_at_0_obs_col <- adm_2_at_2_obs_col %>%
  group_by(time) %>%
  mutate(avg_incid_I_adm_2 = sum(avg_incid_I_adm_2)) %>%
  distinct(time, avg_incid_I_adm_2) %>% 
  ungroup() 


line <- ggplot() +
  geom_line(data = adm_2_at_0_obs_col, aes(x = time, y = avg_incid_I_adm_2), color = '#DDBAA4', linewidth = 3, alpha = 1) + 
  theme(panel.border = element_blank(), axis.line = element_line(linewidth = 1.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        axis.text = element_blank(), axis.title=element_text(size=26)) + 
  xlab("Time") + ylab("Cases")
line
ggsave('./figs/full_line.jpg', plot = line, height = 4, width = 6)


line <- ggplot() +
  geom_line(data = adm_2_at_0_obs_col, aes(x = time, y = avg_incid_I_adm_2), color = '#FFFFFF00', linewidth = 3, alpha = 1) + 
  theme(panel.border = element_blank(), axis.line = element_line(linewidth = 1.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        axis.text = element_blank(), axis.title=element_text(size=26)) + 
  xlab("Time") + ylab("Cases")
line

ggsave('./figs/no_line.jpg', plot = line, height = 4, width = 6)





adm_2_at_2_obs_col_filt <- adm_2_at_2_obs_col |> filter(adm_2 == 'Matale' | 
                                                          adm_2 == 'Kandy' | 
                                                          adm_2 == 'Nuwara Eliya')


adm_2_at_2_obs_col_filt[adm_2_at_2_obs_col_filt$adm_2 == 'Kandy',]$time <- 
  adm_2_at_2_obs_col_filt[adm_2_at_2_obs_col_filt$adm_2 == 'Kandy',]$time - rep(65, 365)
adm_2_at_2_obs_col_filt[adm_2_at_2_obs_col_filt$adm_2 == 'Nuwara Eliya',]$time <- 
  adm_2_at_2_obs_col_filt[adm_2_at_2_obs_col_filt$adm_2 == 'Nuwara Eliya',]$time + rep(40, 365)

line <- ggplot() +
  geom_line(data = adm_2_at_2_obs_col_filt, aes(x = time, y = avg_incid_I_adm_2, color = adm_2), linewidth = 3, alpha = 1) + 
  theme(panel.border = element_blank(), axis.line = element_line(linewidth = 1.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        axis.text = element_blank(), axis.title=element_text(size=26),
        legend.position = 'none') + ylim(0, 12000) +
  xlab("Time") + ylab("Cases") + scale_color_manual(values = c("#ABDDA4", "#A4C7DD","#D6A4DD"))
line

ggsave('./figs/sub_line.jpg', plot = line, height = 4, width = 6)


#ggsave('./figs/full_map.jpg', plot = map, height = 16, width = 13)


#A4C7DD

#D6A4DD

#DDBAA4


"#ABDDA4" "#FDAE61" "#5E4FA2" "#3288BD"




load('./tmp/adm_phone_mobility_dat.RData')

choropleth_1$X <- st_coordinates(st_centroid(choropleth_1$geometry))[,1]
choropleth_1$Y <- st_coordinates(st_centroid(choropleth_1$geometry))[,2]

adm_1_phone_mobility_dat <- left_join(adm_1_phone_mobility_dat, 
                                      choropleth_1[,c('ADM1_EN', 'X', 'Y')],
                                      by = c('adm_1_origin' = 'ADM1_EN'))
adm_1_phone_mobility_dat <- left_join(adm_1_phone_mobility_dat, 
                                      choropleth_1[,c('ADM1_EN', 'X', 'Y')],
                                      by = c('adm_1_destination' = 'ADM1_EN'))

choropleth_2$X <- st_coordinates(st_centroid(choropleth_2$geometry))[,1]
choropleth_2$Y <- st_coordinates(st_centroid(choropleth_2$geometry))[,2]

adm_2_phone_mobility_dat <- left_join(adm_2_phone_mobility_dat, 
                                      choropleth_2[,c('ADM2_EN', 'X', 'Y')],
                                      by = c('adm_2_origin' = 'ADM2_EN'))
adm_2_phone_mobility_dat <- left_join(adm_2_phone_mobility_dat, 
                                      choropleth_2[,c('ADM2_EN', 'X', 'Y')],
                                      by = c('adm_2_destination' = 'ADM2_EN'))


adm_1_phone_mobility_dat_filt <- adm_1_phone_mobility_dat |> filter(adm_1_origin != adm_1_destination)
map <- ggplot() +
  geom_sf(data = choropleth_1, aes(group = ADM1_EN), fill = 'grey', color= 'black', linewidth = 0.5, alpha = 0.85) +
  theme_void() + theme(legend.position = "none") +
  geom_curve(data = adm_1_phone_mobility_dat_filt,
             aes(x=X.x,
                 y=Y.x,
                 xend=X.y,
                 yend=Y.y,
                 alpha=(trips_avg),
                 linewidth = (trips_avg)), color = 'black')

ggsave('./figs/trace_1.jpg', plot = map, height = 6, width = 4)

adm_2_phone_mobility_dat_filt <- adm_2_phone_mobility_dat |> filter(adm_2_origin != adm_2_destination)
adm_2_phone_mobility_dat_filt_more <- adm_2_phone_mobility_dat_filt |> filter(trips_avg > 1000)

map <- ggplot() +
  geom_sf(data = choropleth_2, aes(group = ADM2_EN), fill = '#ABDDA4', color= 'black', linewidth = 0.5, alpha = 0.85) +
  theme_void() + theme(legend.position = "none") +
  geom_curve(data = adm_2_phone_mobility_dat_filt_more,
             aes(x=X.x,
                 y=Y.x,
                 xend=X.y,
                 yend=Y.y,
                 alpha=(trips_avg),
                 linewidth = (trips_avg)), color = 'black')

ggsave('./figs/trace_2.jpg', plot = map, height = 6, width = 4)



map = map + coord_cartesian() +
  geom_curve(size = 1.3,
             aes(x=as.numeric(Entry_Station_Long),
                 y=as.numeric(Entry_Station_Lat),
                 xend=as.numeric(as.character(Exit_Station_Long)),
                 yend=as.numeric(as.character(Exit_Station_Lat)),
                 color=as.factor(token_id)))
map










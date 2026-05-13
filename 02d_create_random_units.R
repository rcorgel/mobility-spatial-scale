################################################################################
# File Name: 02d_create_random_units                                           #
# Purpose:   Regroup divisions into districts and districts into provinces     #
#            to address the modifiable areal unit problem.                     #
# Steps:                                                                       #
#         1. Set-up the script                                                 #
#         2. Load spatial data                                                 #
#         3. Create new units                                                  #
#         4. Re-aggregate mobility data                                        #
#         5. Format mobility data                                              #  
#         6. Rescale mobility data                                             #
#                                                                              #
# Project: Mobility Spatial Scale                                              #
# Author: Ronan Corgel                                                         #
################################################################################

####################
# 1. SET-UP SCRIPT #
####################

# Start with a clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggplot2)
library(reshape2)
library(assertr)
library(sf)
library(spdep)
library(igraph)

# Set the seed
set.seed(123456)

# Set directory 
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project')

# Load format functions
source('./mobility-spatial-scale/02_format_functions.R')

########################
# 2. LOAD SPATIAL DATA #
########################

# Load spatial data (Admin 3)
choropleth_3 <- read_sf(dsn = './raw/lka_adm_20220816_shp/', 
                        layer = 'lka_admbnda_adm3_slsd_20220816')

# Load mobility to shape cross walk
# The mobility data combines multiple admin 3 units, changing the total from 339 to 330
mobility_shape_xwalk <- readRDS('./tmp/mobility_shape_xwalk.rds')

# Merge on the cross walk
choropleth_3 <- left_join(choropleth_3, mobility_shape_xwalk, by = c('ADM3_EN' = 'adm_3_shape'))

# Join polygons to create 330 mobility admin 3 units
divisions <- choropleth_3 |> 
  group_by(adm_3_mobility) |>
  summarise(geometry = sf::st_union(geometry)) |>
  ungroup()

#######################
# 3. CREATE NEW UNITS #
#######################

# Calculate coordinates 
coords <- st_coordinates(st_centroid(divisions))

# Cluster divisions based on kmeans
kmeans_districts <- kmeans(coords, centers = 25, nstart = 50, iter.max = 300)

# Assign new districts
divisions$new_district <- kmeans_districts$cluster

# Quick map
ggplot(divisions) +
  geom_sf(aes(fill = factor(new_district)), color = "white", size = 0.2) +
  labs(title = "25 New Districts", fill = "District") +
  theme_minimal() +
  theme(legend.position = "none")

# Now assign new Provinces
districts <- divisions |>
  group_by(new_district) |>
  summarise(geometry = sf::st_union(geometry)) |>
  ungroup()

# Calculate coordinates 
coords_districts <- st_coordinates(st_centroid(districts))

# Save shapefile
saveRDS(districts, file = './out/adm_2_new_shape.rds')

# Cluster districts based on kmeans
kmeans_provinces <- kmeans(coords_districts, centers = 9, nstart = 50, iter.max = 300)

# Assign new provinces
districts$new_province <- kmeans_provinces$cluster

# Quick map
ggplot(districts) +
  geom_sf(aes(fill = factor(new_province)), color = "white", size = 0.2) +
  labs(title = "9 New Provinces", fill = "Province") +
  theme_minimal() +
  theme(legend.position = "none")

# Create province shape file
provinces <- districts |>
  group_by(new_province) |>
  summarise(geometry = sf::st_union(geometry)) |>
  ungroup()

# Save shapefile
saveRDS(provinces, file = './out/adm_1_new_shape.rds')

# Create new crosswalks
# Divisions to districts
div_to_dist <- as.data.frame(divisions)
div_to_dist$geometry <- NULL

# Districts to provinces
dist_to_prov <- as.data.frame(districts)
dist_to_prov$geometry <- NULL

# Merge
crosswalk <- left_join(div_to_dist, dist_to_prov, by = 'new_district')

# Rename variables
crosswalk <- crosswalk |> 
  dplyr::rename('adm_3' = 'adm_3_mobility',
                'adm_2' = 'new_district',
                'adm_1' = 'new_province')

# Save
saveRDS(crosswalk, file = './out/admin_xwalk_new.rds')

#################################
# 4. RE-AGGREGATE MOBILITY DATA #
#################################

# Re-aggregate mobility data
mobility <- readRDS('./out/phone_mobility_dat_full.rds')
mobility <- mobility |>
  dplyr::select(c(adm_3_origin, adm_3_destination, date, trips_adj))

# Merge on crosswalk
mobility <- left_join(mobility, crosswalk, by = c('adm_3_origin' = 'adm_3'))
mobility <- left_join(mobility, crosswalk, by = c('adm_3_destination' = 'adm_3'))

# Aggregate data to admin level 2
adm_2_phone_mobility_dat <- mobility |>                                
  group_by(adm_2.x, adm_2.y, date) |>
  mutate(trips_sum = sum(trips_adj)) |>
  distinct(adm_2.x, adm_2.y, date, trips_sum, 
           adm_1.x, adm_1.y, .keep_all = FALSE) |>
  ungroup() |>
  # Calculate average daily trips
  group_by(adm_2.x, adm_2.y) |>
  mutate(trips_avg = mean(trips_sum, na.rm = TRUE)) |>
  distinct(adm_2.x, adm_2.y, trips_avg, 
           adm_1.x, adm_1.y, .keep_all = FALSE) |>
  ungroup() |>
  dplyr::rename('adm_2_origin' = 'adm_2.x',
                'adm_2_destination' = 'adm_2.y',
                'adm_1_origin' = 'adm_1.x',
                'adm_1_destination' = 'adm_1.y')

# Check number of units, should be number of adm 2 units
verify(adm_2_phone_mobility_dat, length(unique(adm_2_origin)) == 25)
verify(adm_2_phone_mobility_dat, length(unique(adm_2_destination)) == 25)

# Admin level 1
# Aggregate data to admin level 1
adm_1_phone_mobility_dat <- mobility |>                                
  group_by(adm_1.x, adm_1.y, date) |>
  mutate(trips_sum = sum(trips_adj)) |>
  distinct(date, trips_sum, 
           adm_1.x, adm_1.y, .keep_all = FALSE) |>
  ungroup() |>
  # Calculate average daily trips
  group_by(adm_1.x, adm_1.y) |>
  mutate(trips_avg = mean(trips_sum, na.rm = TRUE)) |>
  distinct(trips_avg, 
           adm_1.x, adm_1.y, .keep_all = FALSE) |>
  ungroup() |>
  dplyr::rename('adm_1_origin' = 'adm_1.x',
                'adm_1_destination' = 'adm_1.y')

# Check number of units, should be number of adm 2 units
verify(adm_1_phone_mobility_dat, length(unique(adm_1_origin)) == 9)
verify(adm_1_phone_mobility_dat, length(unique(adm_1_destination)) == 9)

###########################
# 5. FORMAT MOBILITY DATA #
###########################

# Administrative Level 2 
# Rename/select name data
adm_2_phone_mobility_dat_name <- adm_2_phone_mobility_dat |>
  dplyr::select(c('adm_2_origin', 'adm_2_destination', 'trips_avg')) |>
  dplyr::rename('adm_origin' = 'adm_2_origin',
                'adm_destination' = 'adm_2_destination')
# Create name matrix and long data
adm_2_phone_mobility_mat <- format_mobility_data(data = adm_2_phone_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = FALSE)
adm_2_phone_mobility_long <- format_mobility_data(data = adm_2_phone_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = FALSE)

# Save data
saveRDS(adm_2_phone_mobility_mat, './out/adm_2_phone_mobility_mat_new.rds')
saveRDS(adm_2_phone_mobility_long, './out/adm_2_phone_mobility_long_new.rds')

# Administrative Level 1 
# Rename/select name data
adm_1_phone_mobility_dat_name <- adm_1_phone_mobility_dat |>
  dplyr::select(c('adm_1_origin', 'adm_1_destination', 'trips_avg')) |>
  dplyr::rename('adm_origin' = 'adm_1_origin',
                'adm_destination' = 'adm_1_destination')
# Create name matrix and long data
adm_1_phone_mobility_mat <- format_mobility_data(data = adm_1_phone_mobility_dat_name, 
                                                 method = 'name', output = 'matrix', na_replace = FALSE)
adm_1_phone_mobility_long <- format_mobility_data(data = adm_1_phone_mobility_dat_name, 
                                                  method = 'name', output = 'long', na_replace = FALSE)

# Save data
saveRDS(adm_1_phone_mobility_mat, './out/adm_1_phone_mobility_mat_new.rds')
saveRDS(adm_1_phone_mobility_long, './out/adm_1_phone_mobility_long_new.rds')

############################
# 5. RESCALE MOBILITY DATA #
############################

# Reload data (overwrite data)
adm_3_phone_mobility_long <- readRDS('./out/adm_3_phone_mobility_long.rds')
adm_1_phone_mobility_long <- readRDS('./out/adm_1_phone_mobility_long_new.rds')

# Reload crosswalk
adm_3_x_walk <- readRDS('./out/admin_xwalk_new.rds')

# Replace NA values with 0
adm_3_phone_mobility_long$value <- ifelse(is.na(adm_3_phone_mobility_long$value), 
                                          0, adm_3_phone_mobility_long$value)

# Merge admin 1 to admin 3 
# Merge on admin 1 origins
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_3_x_walk,
                                       by = c('origin' = 'adm_3'))
# Merge on admin 1 destinations
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_3_x_walk,
                                       by = c('destination' = 'adm_3'))

# Aggregate admin 3 travel to the admin 1
adm_3_phone_mobility_long <- adm_3_phone_mobility_long |> group_by(origin, adm_1.y) |>
  mutate(type_sum = sum(value))

# Make variables characters
adm_3_phone_mobility_long$adm_1.x <- as.character(adm_3_phone_mobility_long$adm_1.x)
adm_3_phone_mobility_long$adm_1.y <- as.character(adm_3_phone_mobility_long$adm_1.y)

# Join admin 1 travel to admin 3 travel
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, adm_1_phone_mobility_long,
                                       by = c('adm_1.x' = 'origin', 'adm_1.y' = 'destination'))

# Rescale admin 3 travel to match travel at admin 1
adm_3_phone_mobility_long$value_rescale <- adm_3_phone_mobility_long$value.x * 
  (adm_3_phone_mobility_long$value.y/adm_3_phone_mobility_long$type_sum)

# Check to make sure rescale went as expected (sum to 1)
adm_3_phone_mobility_long <- adm_3_phone_mobility_long |> group_by(origin) |>
  mutate(check = round(sum(value_rescale), 14)) |> ungroup()
adm_3_phone_mobility_long |> assert(in_set(1), check)

# Select variables
adm_3_phone_mobility_mat_rescale_adm_1_long <- adm_3_phone_mobility_long[, c('origin', 'destination', 'value_rescale')]

# Transform into a matrix
adm_3_phone_mobility_mat_rescale_adm_1 <- reshape::cast(
  adm_3_phone_mobility_long[, c('origin', 'destination', 'value_rescale')], 
  origin ~ destination)   
rownames(adm_3_phone_mobility_mat_rescale_adm_1) <- adm_3_phone_mobility_mat_rescale_adm_1$origin   
adm_3_phone_mobility_mat_rescale_adm_1 <- adm_3_phone_mobility_mat_rescale_adm_1[, -1]

# Save
saveRDS(adm_3_phone_mobility_mat_rescale_adm_1, './out/adm_3_phone_mobility_mat_rescale_adm_1_new.rds')
saveRDS(adm_3_phone_mobility_mat_rescale_adm_1_long, './out/adm_3_phone_mobility_long_rescale_adm_1_new.rds')

################################################################################
################################################################################

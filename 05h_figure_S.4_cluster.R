################################################################################
# File Name: 05g_figure_S.3_cluster                                            #
#                                                                              #
# Purpose:   Create figure S.3 for the manuscript.                             #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Cluster of divisions in districts                              #
#            3. Cluster of divisions in provinces                              #
#            4. Cluster of districts in provinces                              #
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
library(scales)
library(reshape2)
library(ggpubr)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

#####################################
# 2. CLUSTER DIVISIONS IN DISTRICTS #
#####################################

# Load mobility data for each data source
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')
load('./tmp/fmt_adm_2_phone_mobility_dat.RData')
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')
load('./tmp/fmt_adm_3_sim_mobility_dat.RData')
load('./tmp/fmt_adm_2_sim_mobility_dat.RData')
load('./tmp/fmt_adm_1_sim_mobility_dat.RData')

# Load admin crosswalk
admin_xwalk <- read_csv('./tmp/admin_xwalk.csv')

######################
# ADMIN 3 to ADMIN 2 #
######################

#####################
# MOBILE PHONE DATA #
#####################

# Merge on admin 2 destination
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 2)], 
                                       by = c('destination' = 'adm_3'))
# Merge on admin 2 origin
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 2)],
                                       by = c('origin' = 'adm_3'))

# Calculate if division trips stay in the division, stay in the district, 
# or leave the district
adm_3_phone_mobility_long$cluster <- ifelse(adm_3_phone_mobility_long$origin == 
                                              adm_3_phone_mobility_long$destination, 
                                            'Stay', 'In District')
adm_3_phone_mobility_long$cluster <- ifelse(adm_3_phone_mobility_long$adm_2.x != 
                                              adm_3_phone_mobility_long$adm_2.y, 
                                            'Out District', adm_3_phone_mobility_long$cluster)

# Collapse data by trip type
cluster_analysis <- adm_3_phone_mobility_long |> group_by(origin, cluster) |>
  mutate(value_sum = sum(value, na.rm = TRUE),
         value_sum = ifelse(is.na(value_sum), 0, value_sum)) |>
  distinct(origin, cluster, value_sum, .keep_all = FALSE) |>
  ungroup() |>
  group_by(origin) |> mutate(sum = sum(value_sum))

# Separate data by trips in and out of the district
cluster_in <- cluster_analysis |> filter(cluster == 'In District') |>
  dplyr::rename('in' = 'value_sum')
cluster_out <- cluster_analysis |> filter(cluster == 'Out District') |>
  dplyr::rename('out' = 'value_sum')

# Merge the two to compare
cluster_merge_3_2 <- left_join(cluster_in, cluster_out, by = c('origin' = 'origin'))
cluster_merge_3_2$diff <- cluster_merge_3_2$`in` - cluster_merge_3_2$out
mean(cluster_merge_3_2$diff) # call out
hist(cluster_merge_3_2$diff, breaks = 40)

#####################################
# 3. CLUSTER DIVISIONS IN PROVINCES #
#####################################

# Merge on admin 1 destination
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 3)], 
                                       by = c('destination' = 'adm_3'))
# Merge on admin 1 origin
adm_3_phone_mobility_long <- left_join(adm_3_phone_mobility_long, admin_xwalk[, c(1, 3)],
                                       by = c('origin' = 'adm_3'))

# Calculate if division trips stay in the division, stay in the province, 
# or leave the province
adm_3_phone_mobility_long$cluster <- ifelse(adm_3_phone_mobility_long$origin == 
                                              adm_3_phone_mobility_long$destination, 'Stay', 'In District')
adm_3_phone_mobility_long$cluster <- ifelse(adm_3_phone_mobility_long$adm_1.x != 
                                              adm_3_phone_mobility_long$adm_1.y, 'Out District', adm_3_phone_mobility_long$cluster)

# Collapse data by trip type
cluster_analysis <- adm_3_phone_mobility_long |> group_by(origin, cluster) |>
  mutate(
    value_sum = sum(value, na.rm = TRUE),
    value_sum = ifelse(is.na(value_sum), 0, value_sum)) |>
  distinct(origin, cluster, value_sum, .keep_all = FALSE) |>
  ungroup() |>
  group_by(origin) |> mutate(sum = sum(value_sum))

# Separate data by trips in and out of the province
cluster_in <- cluster_analysis |> filter(cluster == 'In District') |>
  dplyr::rename('in' = 'value_sum')
cluster_out <- cluster_analysis |> filter(cluster == 'Out District') |>
  dplyr::rename('out' = 'value_sum')

# Merge the two to compare
cluster_merge_3_1 <- left_join(cluster_in, cluster_out, by = c('origin' = 'origin'))
cluster_merge_3_1$diff <- cluster_merge_3_1$`in` - cluster_merge_3_1$out
mean(cluster_merge_3_1$diff) # call out
hist(cluster_merge_3_1$diff, breaks = 40)

#####################################
# 4. CLUSTER DISTRICTS IN PROVINCES #
#####################################

# Change admin cross walk to admin 2 level
admin_xwalk_adm_2 <- admin_xwalk |>
  group_by(adm_2, adm_1) |>
  distinct(adm_2, adm_1, .keep_all = FALSE)

# Merge on admin 1 destination
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, admin_xwalk_adm_2, 
                                       by = c('destination' = 'adm_2'))
# Merge on admi 1 origin
adm_2_phone_mobility_long <- left_join(adm_2_phone_mobility_long, admin_xwalk_adm_2,
                                       by = c('origin' = 'adm_2'))

# Calculate if district trips stay in the district, stay in the province, 
# or leave the province
adm_2_phone_mobility_long$cluster <- ifelse(adm_2_phone_mobility_long$origin == 
                                              adm_2_phone_mobility_long$destination, 'Stay', 'In District')
adm_2_phone_mobility_long$cluster <- ifelse(adm_2_phone_mobility_long$adm_1.x != 
                                              adm_2_phone_mobility_long$adm_1.y, 'Out District', adm_2_phone_mobility_long$cluster)

# Collapse data by trip type
cluster_analysis <- adm_2_phone_mobility_long |> group_by(origin, cluster) |>
  mutate(
    value_sum = sum(value, na.rm = TRUE),
    value_sum = ifelse(is.na(value_sum), 0, value_sum)) |>
  distinct(origin, cluster, value_sum, .keep_all = FALSE) |>
  ungroup() |>
  group_by(origin) |> mutate(sum = sum(value_sum))

# Separate data by trips in and out of the province
cluster_in <- cluster_analysis |> filter(cluster == 'In District') |>
  dplyr::rename('in' = 'value_sum')
cluster_out <- cluster_analysis |> filter(cluster == 'Out District') |>
  dplyr::rename('out' = 'value_sum')

# Merge the two to compare
cluster_merge_2_1 <- left_join(cluster_in, cluster_out, by = c('origin' = 'origin'))
cluster_merge_2_1$diff <- cluster_merge_2_1$`in` - cluster_merge_2_1$out
mean(cluster_merge_2_1$diff )
hist(cluster_merge_2_1$diff, breaks = 40)

##########################
# 5. CREATE FINAL FIGURE #
##########################

# Add labels to the data
cluster_merge_2_1$Group <- 'Districts in \nProvinces'
cluster_merge_3_1$Group <- 'Divisions in \nProvinces'
cluster_merge_3_2$Group <- 'Divisions in \nDistricts'

# Bind the data together
clust <- rbind(cluster_merge_2_1, cluster_merge_3_1, cluster_merge_3_2)

# Calculate a mean by group to add to plots
mu <- clust |> group_by(Group) |>
  mutate(grp.mean = mean(diff)) |>
  distinct(Group, grp.mean, .keep_all = FALSE)

# Create density plot
cluster_plot <- ggplot(clust, aes(x=diff, color=Group, fill = Group)) +
  geom_density(aes(), alpha = 0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Group),
             linetype="dashed") + theme_minimal() + 
  xlab('Internal - External Trip Proportion Difference') + ylab('Density') +
  theme(legend.position = 'bottom')

# Plot
cluster_plot

# Save
ggsave('./figs/figure_S.3_cluster.jpg', plot = cluster_plot, height = 6, width = 9)

################################################################################
################################################################################

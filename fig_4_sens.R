################################################################################
# File Name: figure_4                                                          #
#                                                                              #
# Purpose:   Create figure 4 for the paper.                                    #
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

# Load model 
source('./mobility-spatial-scale/04_metapop_model.R')

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

############################################
# 2. LOAD FIGURE DATA AND CREATE SUB PLOTS #
############################################

###################################
# Load introduction location data #
###################################

load('./tmp/introduction_location_model_results_obs_sens.RData')

adm_3_at_1_mp <- adm_3_at_1_mp_sens_1
adm_3_at_2_mp <- adm_3_at_2_mp_sens_2
adm_2_at_1_mp <- adm_2_at_1_mp_sens_1 
adm_2_at_2_mp <- adm_2_at_2_mp_sens_2


# Load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Add on administrative name vectors
adm_1_at_1_mp$adm_1 <- adm_1_name_vec
adm_2_at_1_mp$adm_2 <- adm_2_name_vec
adm_2_at_2_mp$adm_2 <- adm_2_name_vec
adm_3_at_1_mp$adm_3 <- adm_3_name_vec
adm_3_at_2_mp$adm_3 <- adm_3_name_vec


# Add on population vectors
adm_1_at_1_mp$adm_1_pop <- adm_1_pop_vec
adm_2_at_1_mp$adm_2_pop <- adm_2_pop_vec
adm_2_at_2_mp$adm_2_pop <- adm_2_pop_vec
adm_3_at_1_mp$adm_3_pop <- adm_3_pop_vec
adm_3_at_2_mp$adm_3_pop <- adm_3_pop_vec


# Add on administrative level crosswalks
adm_2_at_1_mp <- left_join(adm_2_at_1_mp, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_2_at_2_mp <- left_join(adm_2_at_2_mp, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_3_at_1_mp <- left_join(adm_3_at_1_mp, adm_3_x_walk, by = c('adm_3' = 'adm_3'))
adm_3_at_2_mp <- left_join(adm_3_at_2_mp, adm_3_x_walk, by = c('adm_3' = 'adm_3'))

# Merge together different model results at various levels
adm_1_adm_2_mp <- left_join(adm_2_at_1_mp, adm_1_at_1_mp, by = c('adm_1' = 'adm_1'))
adm_1_adm_3_mp <- left_join(adm_3_at_1_mp, adm_1_at_1_mp, by = c('adm_1' = 'adm_1'))
adm_2_adm_3_mp <- left_join(adm_3_at_2_mp, adm_2_at_2_mp, by = c('adm_2' = 'adm_2'))

# Calculate magnitude and timing differences
# Mobile Phone Data
adm_1_adm_2_mp$intro_diff <- adm_1_adm_2_mp$intro_time.y - adm_1_adm_2_mp$intro_time.x
adm_1_adm_2_mp$mag_diff <- adm_1_adm_2_mp$magnitude.y - adm_1_adm_2_mp$magnitude.x

adm_1_adm_3_mp$intro_diff <- adm_1_adm_3_mp$intro_time.y - adm_1_adm_3_mp$intro_time.x
adm_1_adm_3_mp$mag_diff <- adm_1_adm_3_mp$magnitude.y - adm_1_adm_3_mp$magnitude.x

adm_2_adm_3_mp$intro_diff <- adm_2_adm_3_mp$intro_time.y - adm_2_adm_3_mp$intro_time.x
adm_2_adm_3_mp$mag_diff <- adm_2_adm_3_mp$magnitude.y - adm_2_adm_3_mp$magnitude.x

# Combine Data sets
adm_1_adm_2_mp$Cat <- 'Prov. - Dist.'
adm_1_adm_3_mp$Cat <- 'Prov. - Div.'
adm_2_adm_3_mp$Cat <- 'Dist. - Div.'

# Select variables
# Mobile Phone
adm_1_adm_2_mp_plot <- adm_1_adm_2_mp %>% select(intro_diff, mag_diff,
                                                 adm_2_pop, Cat) %>%
  dplyr::rename('pop' = 'adm_2_pop')
adm_1_adm_3_mp_plot <- adm_1_adm_3_mp %>% select(intro_diff, mag_diff,
                                                 adm_3_pop, Cat) %>%
  dplyr::rename('pop' = 'adm_3_pop')
adm_2_adm_3_mp_plot <- adm_2_adm_3_mp %>% select(intro_diff, mag_diff,
                                                 adm_3_pop, Cat) %>%
  dplyr::rename('pop' = 'adm_3_pop')

# Combine
all_comboa_mp <- rbind(adm_1_adm_2_mp_plot, adm_1_adm_3_mp_plot, adm_2_adm_3_mp_plot)

# Set plot theme
theme_plot <- theme(axis.title = element_text(size=26, color = 'black'),
                    axis.text = element_text(size=30, color = 'black'),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.text = element_text(size = 34),
                    legend.title = element_text(size = 34),
                    plot.title = element_text(size=38, hjust = 0.5),
                    legend.position = 'bottom',
                    legend.key.width = unit(1.2, 'cm'),
                    strip.text.x = element_text(size = 26))

loc_intro <- ggplot(all_comboa_mp, aes(x = Cat, y = intro_diff)) + 
  geom_jitter(aes(color = log(pop)), alpha = 0.7, size = 3.5, width = 0.35) + theme_minimal() +
  geom_boxplot(outlier.shape = NA, alpha = 0.70, width=0.35, fill = NA, linewidth = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('') + 
  scale_color_distiller('Log(Population)', palette = "PuRd", direction = 1) + theme_plot +
  scale_y_continuous(breaks = seq(-80, 80, 40), limits = c(-80, 80)) 
ggplot(adm_1_adm_3_mp_plot) + geom_point(aes(x = log(pop), y = intro_diff))
ggplot(adm_2_adm_3_mp_plot) + geom_point(aes(x = log(pop), y = intro_diff))

loc_mag <- ggplot(all_comboa_mp, aes(x = Cat, y = mag_diff)) + 
  geom_jitter(aes(color = log(pop)), alpha = 0.7, size = 3, width = 0.3) + theme_minimal() +
  geom_boxplot(outlier.shape = NA, alpha = 0.80, width=0.4, fill = NA, linewidth = 1.1) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('') + 
  scale_color_distiller('Log(Population)', palette = "PuRd", direction = 1) + theme_plot +
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000))

#############################
# Load tranmissibility data #
#############################

load('./tmp/transmission_model_results_obs_sens.RData')


adm_3_at_1_mp_col <- adm_3_at_1_mp_sens_1_col
adm_3_at_2_mp_col <- adm_3_at_2_mp_sens_2_col
adm_2_at_1_mp_col <- adm_2_at_1_mp_sens_1_col
adm_2_at_2_mp_col <- adm_2_at_2_mp_sens_2_col

adm_3_at_1_mp_mad <- adm_3_at_1_mp_sens_1_mad
adm_3_at_2_mp_mad <- adm_3_at_2_mp_sens_2_mad
adm_2_at_1_mp_mad <- adm_2_at_1_mp_sens_1_mad
adm_2_at_2_mp_mad <- adm_2_at_2_mp_sens_2_mad

# Mobile Phone Data
# Join comparison model results (Admin 2 and Admin 1)
adm_1_adm_2_col_mp <- left_join(adm_2_at_1_mp_col, adm_1_at_1_mp_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_2_mad_mp <- left_join(adm_2_at_1_mp_mad, adm_1_at_1_mp_mad, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_3_col_mp <- left_join(adm_3_at_1_mp_col, adm_1_at_1_mp_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_3_mad_mp <- left_join(adm_3_at_1_mp_mad, adm_1_at_1_mp_mad, by = c('intro_loc' = 'intro_loc'))
adm_2_adm_3_col_mp <- left_join(adm_3_at_2_mp_col, adm_2_at_2_mp_col, by = c('intro_loc' = 'intro_loc'))
adm_2_adm_3_mad_mp <- left_join(adm_3_at_2_mp_mad, adm_2_at_2_mp_mad, by = c('intro_loc' = 'intro_loc'))

# Create label variable
adm_1_adm_2_col_mp$`Introduction Location` <- '\nColombo'
adm_1_adm_2_mad_mp$`Introduction Location` <- '\nMadhu'
adm_1_adm_3_col_mp$`Introduction Location` <- '\nColombo'
adm_1_adm_3_mad_mp$`Introduction Location` <- '\nMadhu'
adm_2_adm_3_col_mp$`Introduction Location` <- '\nColombo'
adm_2_adm_3_mad_mp$`Introduction Location` <- '\nMadhu'
adm_1_adm_2_mad_mp$Cat <- 'Prov. - Dist.'
adm_1_adm_2_col_mp$Cat <- 'Prov. - Dist.'
adm_1_adm_3_mad_mp$Cat <- 'Prov. - Div.'
adm_1_adm_3_col_mp$Cat <- 'Prov. - Div.'
adm_2_adm_3_mad_mp$Cat <- 'Dist. - Div.'
adm_2_adm_3_col_mp$Cat <- 'Dist. - Div.'
# Append urban and rural introduction location results
all_combos_mp <- rbind(adm_1_adm_2_col_mp, adm_1_adm_2_mad_mp, adm_1_adm_3_col_mp, adm_1_adm_3_mad_mp, adm_2_adm_3_col_mp, adm_2_adm_3_mad_mp)

# Calculate magnitude and timing differences
# Mobile Phone Data
all_combos_mp$intro_diff <- all_combos_mp$intro_time.y - all_combos_mp$intro_time.x
all_combos_mp$mag_diff <- all_combos_mp$magnitude.y - all_combos_mp$magnitude.x

trans_intro <- ggplot(all_combos_mp, aes(x = `Introduction Location`, y = intro_diff)) + 
  geom_jitter(aes(color = intro_loc), alpha = 0.70, size = 3.5, width = 0.35) + theme_minimal() +
  geom_boxplot(outlier.shape = NA, alpha = 0.75, width=0.4, fill = NA, linewidth = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('') + 
  scale_color_distiller('Transmissibility   \n', palette = "Blues", direction = 1) + theme_plot +
  scale_y_continuous(breaks = seq(-80,80,40), limits = c(-80, 80)) + 
  facet_wrap(vars(Cat), strip.position = "bottom") +
  theme(axis.text = element_text(size=26))
adm_2_adm_3_col_mp$intro_diff <- adm_2_adm_3_col_mp$intro_time.y - adm_2_adm_3_col_mp$intro_time.x

ggplot(adm_2_adm_3_col_mp) + geom_point(aes(x = intro_loc, y = intro_diff))

ggsave('./figs/trans_pres_sens.jpg', plot = trans_intro, height = 9, width = 11)

ggsave('./figs/loc_pres_sens.jpg', plot = loc_intro, height = 9, width = 11)

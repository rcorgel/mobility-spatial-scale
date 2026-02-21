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

# Load observed and rescaled results
load('./tmp/introduction_location_model_results_obs.RData')
load('./tmp/introduction_location_model_results_obs_sens.RData')

# Observed
adm_3_at_1_mp_col <- adm_3_at_1_mp[, -c(5)]
adm_3_at_2_mp_col <- adm_3_at_2_mp[, -c(5)]
adm_2_at_1_mp_col <- adm_2_at_1_mp
adm_2_at_2_mp_col <- adm_2_at_2_mp
adm_1_at_1_mp_col <- adm_1_at_1_mp

# Rescaled
adm_3_at_1_mp_mad <- adm_3_at_1_mp_sens_1[, -c(5)]
adm_3_at_2_mp_mad <- adm_3_at_2_mp_sens_2[, -c(5)]
adm_2_at_1_mp_mad <- adm_2_at_1_mp_sens_1
adm_2_at_2_mp_mad <- adm_2_at_2_mp
adm_1_at_1_mp_mad <- adm_1_at_1_mp

# Load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Add on administrative name vectors
adm_1_at_1_mp_col$adm_1 <- adm_1_name_vec
adm_2_at_1_mp_col$adm_2 <- adm_2_name_vec
adm_2_at_2_mp_col$adm_2 <- adm_2_name_vec
adm_3_at_1_mp_col$adm_3 <- adm_3_name_vec
adm_3_at_2_mp_col$adm_3 <- adm_3_name_vec
adm_1_at_1_mp_mad$adm_1 <- adm_1_name_vec
adm_2_at_1_mp_mad$adm_2 <- adm_2_name_vec
adm_2_at_2_mp_mad$adm_2 <- adm_2_name_vec
adm_3_at_1_mp_mad$adm_3 <- adm_3_name_vec
adm_3_at_2_mp_mad$adm_3 <- adm_3_name_vec

# Add on population vectors
adm_1_at_1_mp_col$adm_1_pop <- adm_1_pop_vec
adm_2_at_1_mp_col$adm_2_pop <- adm_2_pop_vec
adm_2_at_2_mp_col$adm_2_pop <- adm_2_pop_vec
adm_3_at_1_mp_col$adm_3_pop <- adm_3_pop_vec
adm_3_at_2_mp_col$adm_3_pop <- adm_3_pop_vec
adm_1_at_1_mp_mad$adm_1_pop <- adm_1_pop_vec
adm_2_at_1_mp_mad$adm_2_pop <- adm_2_pop_vec
adm_2_at_2_mp_mad$adm_2_pop <- adm_2_pop_vec
adm_3_at_1_mp_mad$adm_3_pop <- adm_3_pop_vec
adm_3_at_2_mp_mad$adm_3_pop <- adm_3_pop_vec


# Add on administrative level crosswalks
adm_2_at_1_mp_col <- left_join(adm_2_at_1_mp_col, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_2_at_2_mp_col <- left_join(adm_2_at_2_mp_col, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_3_at_1_mp_col <- left_join(adm_3_at_1_mp_col, adm_3_x_walk, by = c('adm_3' = 'adm_3'))
adm_3_at_2_mp_col <- left_join(adm_3_at_2_mp_col, adm_3_x_walk, by = c('adm_3' = 'adm_3'))
adm_2_at_1_mp_mad <- left_join(adm_2_at_1_mp_mad, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_2_at_2_mp_mad <- left_join(adm_2_at_2_mp_mad, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_3_at_1_mp_mad <- left_join(adm_3_at_1_mp_mad, adm_3_x_walk, by = c('adm_3' = 'adm_3'))
adm_3_at_2_mp_mad <- left_join(adm_3_at_2_mp_mad, adm_3_x_walk, by = c('adm_3' = 'adm_3'))

# Merge together different model results at various levels
adm_1_adm_2_mp_col <- left_join(adm_2_at_1_mp_col, adm_1_at_1_mp_col, by = c('adm_1' = 'adm_1'))
adm_1_adm_3_mp_col <- left_join(adm_3_at_1_mp_col, adm_1_at_1_mp_col, by = c('adm_1' = 'adm_1'))
adm_2_adm_3_mp_col <- left_join(adm_3_at_2_mp_col, adm_2_at_2_mp_col, by = c('adm_2' = 'adm_2'))
adm_1_adm_2_mp_mad <- left_join(adm_2_at_1_mp_mad, adm_1_at_1_mp_mad, by = c('adm_1' = 'adm_1'))
adm_1_adm_3_mp_mad <- left_join(adm_3_at_1_mp_mad, adm_1_at_1_mp_mad, by = c('adm_1' = 'adm_1'))
adm_2_adm_3_mp_mad <- left_join(adm_3_at_2_mp_mad, adm_2_at_2_mp_mad, by = c('adm_2' = 'adm_2'))

# Calculate magnitude and timing differences
# Mobile Phone Data
adm_1_adm_2_mp_col$intro_diff <- adm_1_adm_2_mp_col$intro_time.y - adm_1_adm_2_mp_col$intro_time.x
adm_1_adm_2_mp_col$mag_diff <- adm_1_adm_2_mp_col$magnitude.y - adm_1_adm_2_mp_col$magnitude.x

adm_1_adm_3_mp_col$intro_diff <- adm_1_adm_3_mp_col$intro_time.y - adm_1_adm_3_mp_col$intro_time.x
adm_1_adm_3_mp_col$mag_diff <- adm_1_adm_3_mp_col$magnitude.y - adm_1_adm_3_mp_col$magnitude.x

adm_2_adm_3_mp_col$intro_diff <- adm_2_adm_3_mp_col$intro_time.y - adm_2_adm_3_mp_col$intro_time.x
adm_2_adm_3_mp_col$mag_diff <- adm_2_adm_3_mp_col$magnitude.y - adm_2_adm_3_mp_col$magnitude.x

adm_1_adm_2_mp_mad$intro_diff <- adm_1_adm_2_mp_mad$intro_time.y - adm_1_adm_2_mp_mad$intro_time.x
adm_1_adm_2_mp_mad$mag_diff <- adm_1_adm_2_mp_mad$magnitude.y - adm_1_adm_2_mp_mad$magnitude.x

adm_1_adm_3_mp_mad$intro_diff <- adm_1_adm_3_mp_mad$intro_time.y - adm_1_adm_3_mp_mad$intro_time.x
adm_1_adm_3_mp_mad$mag_diff <- adm_1_adm_3_mp_mad$magnitude.y - adm_1_adm_3_mp_mad$magnitude.x

adm_2_adm_3_mp_mad$intro_diff <- adm_2_adm_3_mp_mad$intro_time.y - adm_2_adm_3_mp_mad$intro_time.x
adm_2_adm_3_mp_mad$mag_diff <- adm_2_adm_3_mp_mad$magnitude.y - adm_2_adm_3_mp_mad$magnitude.x

# Create label variable
adm_1_adm_2_mp_col$`Introduction Location` <- '\nObserved'
adm_1_adm_2_mp_mad$`Introduction Location` <- '\nRescaled'
adm_1_adm_3_mp_col$`Introduction Location` <- '\nObserved'
adm_1_adm_3_mp_mad$`Introduction Location` <- '\nRescaled'
adm_2_adm_3_mp_col$`Introduction Location` <- '\nObserved'
adm_2_adm_3_mp_mad$`Introduction Location` <- '\nRescaled'
adm_1_adm_2_mp_mad$Cat <- 'Prov. - Dist.'
adm_1_adm_2_mp_col$Cat <- 'Prov. - Dist.'
adm_1_adm_3_mp_mad$Cat <- 'Prov. - Div.'
adm_1_adm_3_mp_col$Cat <- 'Prov. - Div.'
adm_2_adm_3_mp_mad$Cat <- 'Dist. - Div.'
adm_2_adm_3_mp_col$Cat <- 'Dist. - Div.'


merge <- left_join(adm_1_adm_3_mp_col, adm_1_adm_3_mp_mad, by = c('intro_loc.x', 
                                                                  'intro_loc.y'))

merge$intro_diff_diff <- merge$intro_diff.y -  merge$intro_diff.x


ggplot(merge) + geom_point(aes(y = intro_diff.x, x = diff))

cor(merge$intro_diff.x, merge$diff)
cor(merge$intro_diff.y, merge$diff)


merge <- left_join(merge, adm_3_adm_1_phone[, c(7, 8)], by = c('adm_3_pop.x' = 
                                                                  'population_2020_adm_3'))


ggplot(merge) + geom_point(aes(x = intro_diff_diff, y = diff))

# Select variables
# Mobile Phone
adm_1_adm_2_mp_col_plot <- adm_1_adm_2_mp_col %>% select(intro_diff, mag_diff,
                                                 adm_2_pop, Cat, `Introduction Location`) %>%
  dplyr::rename('pop' = 'adm_2_pop')
adm_1_adm_3_mp_col_plot <- adm_1_adm_3_mp_col %>% select(intro_diff, mag_diff,
                                                 adm_3_pop, Cat, `Introduction Location`) %>%
  dplyr::rename('pop' = 'adm_3_pop')
adm_2_adm_3_mp_col_plot <- adm_2_adm_3_mp_col %>% select(intro_diff, mag_diff,
                                                 adm_3_pop, Cat, `Introduction Location`) %>%
  dplyr::rename('pop' = 'adm_3_pop')

adm_1_adm_2_mp_mad_plot <- adm_1_adm_2_mp_mad %>% select(intro_diff, mag_diff,
                                                         adm_2_pop, Cat, `Introduction Location`) %>%
  dplyr::rename('pop' = 'adm_2_pop')
adm_1_adm_3_mp_mad_plot <- adm_1_adm_3_mp_mad %>% select(intro_diff, mag_diff,
                                                         adm_3_pop, Cat, `Introduction Location`) %>%
  dplyr::rename('pop' = 'adm_3_pop')
adm_2_adm_3_mp_mad_plot <- adm_2_adm_3_mp_mad %>% select(intro_diff, mag_diff,
                                                         adm_3_pop, Cat, `Introduction Location`) %>%
  dplyr::rename('pop' = 'adm_3_pop')


# Combine
all_combos_mp <- rbind(adm_1_adm_2_mp_col_plot, adm_1_adm_3_mp_col_plot, adm_2_adm_3_mp_col_plot,
                       adm_1_adm_2_mp_mad_plot, adm_1_adm_3_mp_mad_plot, adm_2_adm_3_mp_mad_plot)

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

loc_intro <- ggplot(all_combos_mp, aes(x = `Introduction Location`, y = intro_diff)) + 
  geom_jitter(aes(color = log(pop)), alpha = 0.70, size = 3.5, width = 0.35) + theme_minimal() +
  geom_boxplot(outlier.shape = NA, alpha = 0.75, width=0.4, fill = NA, linewidth = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('') + 
  scale_color_distiller('Log(Population)', palette = "PuRd", direction = 1) + theme_plot +
  scale_y_continuous(breaks = seq(-30,30,10), limits = c(-35, 35)) + 
  facet_wrap(vars(Cat), strip.position = "bottom") +
  theme(axis.text = element_text(size=26))

loc_intro

loc_mag <- ggplot(all_combos_mp, aes(x = `Introduction Location`, y = mag_diff)) + 
  geom_jitter(aes(color = log(pop)), alpha = 0.70, size = 3.5, width = 0.35) + theme_minimal() +
  geom_boxplot(outlier.shape = NA, alpha = 0.75, width=0.4, fill = NA, linewidth = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('') + 
  scale_color_distiller('Log(Population)', palette = "PuRd", direction = 1) + theme_plot +
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000)) +
  facet_wrap(vars(Cat), strip.position = "bottom") +
  theme(axis.text = element_text(size=26))

loc_mag

figure_S.7 <- cowplot::plot_grid(ggplot() + theme_void(), ggplot() + theme_void(),
                               loc_intro, loc_mag,
                               rel_heights = c(0.06, 1),
                               rel_widths = c(1, 1),
                               nrow = 2, 
                               labels = c('Spatial Invasion', 
                                          'Epidemic Magnitude', 
                                          '(a)', '(b)' ),
                               label_size = 26, hjust = 0)    


ggsave('./figs/figure_S.7_spectrum_sens.jpg', plot = figure_S.7, height = 13, width = 25)

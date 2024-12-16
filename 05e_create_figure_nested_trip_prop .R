################################################################################
# File Name: 04e_create_figure_nested_trip_prop                                #
#                                                                              #
# Purpose:   Create figures that describe differences in external trip         #
#            proportions between nested units.                                 #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load figure data and create sub plots                          #
#            3. Create figures (one in appendix)                               #
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

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

############################################
# 2. LOAD FIGURE DATA AND CREATE SUB PLOTS #
############################################

# Load mobility data for each data source
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')
load('./tmp/fmt_adm_2_phone_mobility_dat.RData')
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')
load('./tmp/simulated_phone_mobility_format.RData')

# Load admin crosswalk
admin_xwalk <- read_csv('./tmp/admin_xwalk.csv')

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

mean(adm_3_adm_2_phone$diff)
min(adm_3_adm_2_phone$diff)
max(adm_3_adm_2_phone$diff)

# Create plot for nested trip difference
plot_3_2_phone <- ggplot(adm_3_adm_2_phone, aes(diff, adm_2_value)) +
  geom_point(color = '#4292C6', alpha = 0.25, size = 5) + 
  geom_vline(xintercept = 0, color = 'black', linetype = 2) +
  ggtitle('Mobile Phone Data') + xlim(-1, 1) + 
  xlab('Out of District Trip Proportion Difference') + 
  ylab('Out of District Trip Proportion (Admin 2)') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=30, hjust = 0.5))

# Load population data
load('./tmp/adm_population_dat.RData')
# Merge on admin 3 population
adm_3_adm_2_phone <- left_join(adm_3_adm_2_phone, adm_3_population_dat[, c(1, 4)], 
                               by = c('origin' = 'adm_3_mobility'))


# Create scatter plot by population
plot_3_2_scatter <- ggplot(adm_3_adm_2_phone, aes(log(population_2020_adm_3), diff)) +
  geom_point(color = '#4292C6', alpha = 0.2, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(method = "lm", se = FALSE, color = '#4292C6', linewidth = 3) + 
  ggtitle('Admin 2 - Admin 3') + ylim(-1, 1) +
  xlab('Log Population (Administrative Level 3)') + 
  ylab('Out of District Trip Proportion Difference') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=30, hjust = 0.5))

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

# Create plot for nested trip difference
plot_3_2_phone_sim <- ggplot(adm_3_adm_2_phone_sim, aes(diff, adm_2_value)) +
  geom_point(color = '#4292C6', alpha = 0.25, size = 5) + 
  geom_vline(xintercept = 0, color = 'black', linetype = 2) +
  ggtitle('Simulated Mobile Phone Data') + xlim(-1, 1) + 
  xlab('Out of District Trip Proportion Difference') + 
  ylab('Out of District Trip Proportion (Admin 2)') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=30, hjust = 0.5))

# Merge on admin 3 population
adm_3_adm_2_phone_sim <- left_join(adm_3_adm_2_phone_sim, adm_3_population_dat[, c(1, 4)], 
                               by = c('adm_3_origin' = 'adm_3_mobility'))

# Create scatter plot by population
plot_3_2_scatter_sim <- ggplot(adm_3_adm_2_phone_sim, aes(log(population_2020_adm_3), diff)) +
  geom_point(color = '#4292C6', alpha = 0.2, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(method = "lm", se = FALSE, color = '#4292C6', linewidth = 3) + 
  ggtitle('Admin 2 - Admin 3') + ylim(-1, 1) +
  xlab('Log Population (Administrative Level 3)') + 
  ylab('Out of District Trip Proportion Difference') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=30, hjust = 0.5))

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

mean(adm_3_adm_1_phone$diff)
min(adm_3_adm_1_phone$diff)
max(adm_3_adm_1_phone$diff)

# Create plot for nested trip difference
plot_3_1_phone <- ggplot(adm_3_adm_1_phone, aes(diff, adm_1_value)) +
  geom_point(color = '#4292C6', alpha = 0.25, size = 5) + 
  geom_vline(xintercept = 0, color = 'black', linetype = 2) +
  ggtitle('Mobile Phone Data') + xlim(-1, 1) + 
  xlab('Out of Province Trip Proportion Difference') + 
  ylab('Out of Province Trip Proportion (Admin 1)') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=30, hjust = 0.5))

# Load population data
# Merge on admin 3 population
adm_3_adm_1_phone <- left_join(adm_3_adm_1_phone, adm_3_population_dat[, c(1, 4)], 
                               by = c('origin' = 'adm_3_mobility'))

# Create scatter plot by population
plot_3_1_scatter <- ggplot(adm_3_adm_1_phone, aes(log(population_2020_adm_3), diff)) +
  geom_point(color = '#4292C6', alpha = 0.2, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(method = "lm", se = FALSE, color = '#4292C6', linewidth = 3) + 
  ggtitle('Admin 1 - Admin 3') + ylim(-1, 1) +
  xlab('Log Population (Administrative Level 3)') + 
  ylab('Out of Province Trip Proportion Difference') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=30, hjust = 0.5))

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
adm_3_adm_1_phone_sim <- adm_3_phone_mobility_long %>%
  filter(adm_1.x != adm_1.y) %>%
  group_by(origin) %>%
  mutate(adm_3_sum = sum(value, na.rm = TRUE)) %>%
  distinct(origin, adm_1.y, adm_3_sum, .keep_all = FALSE) %>%
  dplyr::rename('adm_1_origin' = 'adm_1.y')

# Merge on origin from admin 1
adm_3_adm_1_phone_sim <- left_join(adm_3_adm_1_phone_sim, 
                               adm_1_phone_mobility_long[
                                 adm_1_phone_mobility_long$origin == 
                                   adm_1_phone_mobility_long$destination, ], 
                               by = c('adm_1_origin' = 'origin'))
# Calculate 1 - stays
adm_3_adm_1_phone_sim$adm_1_value <- 1 - adm_3_adm_1_phone_sim$value
# Calculate difference between nested units
adm_3_adm_1_phone_sim$diff <- adm_3_adm_1_phone_sim$adm_1_value - adm_3_adm_1_phone_sim$adm_3_sum

# Create plot for nested trip difference
plot_3_1_phone_sim <- ggplot(adm_3_adm_1_phone_sim, aes(diff, adm_1_value)) +
  geom_point(color = '#4292C6', alpha = 0.25, size = 5) + 
  geom_vline(xintercept = 0, color = 'black', linetype = 2) +
  ggtitle('Mobile Phone Data') + xlim(-1, 1) + 
  xlab('Out of Province Trip Proportion Difference') + 
  ylab('Out of Province Trip Proportion (Admin 1)') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=30, hjust = 0.5))

# Load population data
# Merge on admin 3 population
adm_3_adm_1_phone_sim <- left_join(adm_3_adm_1_phone_sim, adm_3_population_dat[, c(1, 4)], 
                               by = c('origin' = 'adm_3_mobility'))

# Create scatter plot by population
plot_3_1_scatter_sim <- ggplot(adm_3_adm_1_phone_sim, aes(log(population_2020_adm_3), diff)) +
  geom_point(color = '#4292C6', alpha = 0.2, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(method = "lm", se = FALSE, color = '#4292C6', linewidth = 3) + 
  ggtitle('Admin 1 - Admin 3') + ylim(-1, 1) +
  xlab('Log Population (Administrative Level 3)') + 
  ylab('Out of Province Trip Proportion Difference') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=30, hjust = 0.5))

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

mean(adm_2_adm_1_phone$diff)
min(adm_2_adm_1_phone$diff)
max(adm_2_adm_1_phone$diff)

# Create plot for nested trip difference
plot_2_1_phone <- ggplot(adm_2_adm_1_phone, aes(diff, adm_1_value)) +
  geom_point(color = '#4292C6', alpha = 0.5, size = 5) + 
  geom_vline(aes(xintercept = 0), color = 'black', linetype = 2) +
  ggtitle('Mobile Phone Data') + xlim(-1, 1) +
  xlab('Out of Province Trip Proportion Difference') + 
  ylab('Out of Province Trip Proportion (Admin 1)') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=30, hjust = 0.5))

# Load population data
# Merge on admin 3 population
adm_2_adm_1_phone <- left_join(adm_2_adm_1_phone, adm_2_population_dat[, c(1, 3)], 
                               by = c('origin' = 'adm_2'))

# Create scatter plot by population
plot_2_1_scatter <- ggplot(adm_2_adm_1_phone, aes(log(population_2020_adm_2), diff)) +
  geom_point(color = '#4292C6', alpha = 0.2, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(method = "lm", se = FALSE, color = '#4292C6', linewidth = 3) + 
  ggtitle('Admin 1 - Admin 2') + ylim(-1, 1) +
  xlab('Log Population (Administrative Level 2)') + 
  ylab('Out of Province Trip Proportion Difference') +
  theme_minimal() +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=30, hjust = 0.5))

#######################################
# 3. CREATE FIGURES (ONE IN APPENDIX) #
#######################################

# Create Differences in Out of Unit Travel between Administrative Levels by 
# Population for Mobile Phone Data figure
figure <- plot_grid(plot_2_1_scatter, plot_3_1_scatter, plot_3_2_scatter,
                    nrow = 1,
                    labels = c('(a)', 
                               '(b)',
                               '(c)'),
                    label_size = 26, hjust = 0)
# Save figure
ggsave('./figs/figure_nested_trip_prop_pop.jpg', plot = figure, height = 10, width = 25)

# Create Differences in Out of Unit Travel appendix figure
figure_5 <- plot_grid(plot_3_2_phone, plot_3_2_fb, plot_3_2_survey,
                      plot_2_1_phone, plot_2_1_fb, plot_2_1_survey,
                      nrow = 2,
                      labels = c('(a)', 
                                 '(b)',
                                 '(c)',
                                 '(d)',
                                 '(e)',
                                 '(f)'
                      ),
                      label_size = 26, hjust = 0)
# Save figure 3
ggsave('./figs/figure_nested_trip_prop_appendix.jpg', plot = figure_5, height = 20, width = 25)

################################################################################
################################################################################

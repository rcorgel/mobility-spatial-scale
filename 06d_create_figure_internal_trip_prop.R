################################################################################
# File Name: 04d_create_figure_internal_trip_prop                              #
#                                                                              #
# Purpose:   Examine internal trip proportion distributions by administrative  #
#            levels, data sources, and by population.                          #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure functions                                        #
#            3. Load figure data and create sub plots                          #
#            4. Create figures (one in appendix)                               #
#            5. Text callouts                                                  #
#                                                                              #
# Project:   Sri Lanka Spatial Aggregation                                     #
# Author:    Ronan Corgel                                                      #
################################################################################

####################
# 1. SET-UP SCRIPT #
####################

# Do not clear environment, use objects form previous file

# Load libraries
library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(sf)
library(ggpubr)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

##############################
# 2. CREATE FIGURE FUNCTIONS #
##############################

# Plot functions
# Data summary function, mean +/- one sd
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y = m,ymin = ymin,ymax = ymax))
}

# Create violin plots of internal trip proportions by location
make_violin_plot <- function(data) {
  plot <- ggplot(data, aes(x = level, y = value, fill = level)) +
    geom_violin(trim = FALSE, color = 'black', alpha = 0.75) +
    theme_minimal() + ylim(0, 1) +
    scale_fill_manual(values=c('#807DBA', '#41AE76', '#4292C6')) +
    theme(legend.position = 'none') +
    xlab('Administrative Level') + 
    ylab('Proportion of Trips within Unit') +
    ggtitle(' ') +
    theme(axis.title = element_text(size=26),
          axis.text = element_text(size=22),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=30, hjust = 0.5))
  return(plot)
} 

# Create scatter plot of internal trip proportions by population
make_scatter_plot <- function(data) {
  plot <- ggplot(data) +
    geom_point(aes(x = log(population), y = value, color = level), alpha = 0.25, size = 4) +
    geom_smooth(method = lm, aes(x = log(population), y = value, color = level), 
                alpha = 0.8, se = FALSE, linewidth = 2.5) +
    ylim(0, 1) + xlim(6, 16.5) +
    scale_color_manual(values=c('#807DBA', '#41AE76', '#4292C6')) +
    xlab('Log Population') + 
    ylab('Proportion of Trips within Unit') +
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

############################################
# 3. LOAD FIGURE DATA AND CREATE SUB PLOTS #
############################################

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

# Append data together
adm_phone_dat <- rbind(adm_1_phone_dat, adm_2_phone_dat, adm_3_phone_dat)

# Create phone mobility data violin and scatter plots
phone_violin_plot <- make_violin_plot(data = adm_phone_dat)
phone_scatter_plot <- make_scatter_plot(data = adm_phone_dat)

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

# Append data together
adm_phone_sim_dat <- rbind(adm_1_phone_sim_dat, adm_2_phone_sim_dat, adm_3_phone_sim_dat)

# Create phone mobility data violin and scatter plots
phone_violin_plot_sim <- make_violin_plot(data = adm_phone_sim_dat)
phone_scatter_plot_sim <- make_scatter_plot(data = adm_phone_sim_dat)

#######################################
# 4. CREATE FIGURES (ONE IN APPENDIX) #
#######################################

# Create figure, with labels
figure_int <- plot_grid(phone_violin_plot + stat_summary(fun.data=data_summary) + ggtitle('Mobile Phone Data'), 
                    phone_violin_plot_sim + stat_summary(fun.data=data_summary) + ggtitle('Simulated Mobile Phone Data'), 
                    nrow = 1,
                    labels = c('(g)', 
                               '(h)'),
                    label_size = 26, hjust = 0)

# Save figure
ggsave('./figs/figure_internal_trip_prop.jpg', plot = figure, height = 10, width = 25)

# Create matix combined figure
figure_comb <- plot_grid(figure, figure_int, 
                         nrow = 2,
                         labels = c('', 
                                    ''),
                         label_size = 26, hjust = 0,
                         rel_heights = c(2.15, 1.1))

ggsave('./figs/figure_desc_comb.jpg', plot = figure_comb, height = 26, width = 25)

# Create appendix figure
figure <- plot_grid(phone_scatter_plot + ggtitle('Mobile Phone Data') + theme(legend.position = 'none'), 
                    phone_scatter_plot_sim + ggtitle('Simulated Mobile Phone Data') + theme(legend.position = 'none'), 
                    nrow = 1,
                    labels = c('(a)', 
                               '(b)'),
                    label_size = 26, hjust = 0)

# Create legend for appendix figure
adm_phone_dat$`Administrative Level` <- adm_phone_dat$level
legend <- get_legend(ggplot(adm_phone_dat) +
                       geom_point(aes(x = log(population), y = value, color = `Administrative Level`), alpha = 0.25, size = 4) +
                       geom_smooth(method = lm, aes(x = log(population), y = value, color = `Administrative Level`), 
                                   alpha = 0.8, se = FALSE, linewidth = 5) +
                       ylim(0, 1) + xlim(6, 16.5) +
                       scale_color_manual(values=c('#807DBA', '#41AE76', '#4292C6')) +
                       xlab('Log Population') + 
                       ylab('Proportion of Trips within Unit') +
                       ggtitle(' ') +
                       theme_minimal() + 
                       theme(axis.title = element_text(size=22),
                             axis.text = element_text(size=20),
                             panel.grid.major.x = element_blank(),
                             legend.text = element_text(size = 26),
                             legend.title = element_text(size = 26),
                             panel.grid.minor = element_blank(),
                             legend.position = 'bottom',
                             plot.title = element_text(size=30, hjust = 0.5)))

# Combine legend with figure
figure <- plot_grid(figure,
                    legend,
                    nrow = 2,
                    rel_heights = c(1, 0.1))

# Save appendix figure
ggsave('./figs/figure_internal_trip_prop_pop.jpg', plot = figure, height = 11, width = 25)

####################
# 5. TEXT CALLOUTS #
####################

# Mean internal trip proportions for mobile phone data
mean(adm_3_phone_dat$value)
mean(adm_2_phone_dat$value)
mean(adm_1_phone_dat$value)

mean(adm_3_phone_sim_dat$value)
mean(adm_2_phone_sim_dat$value)
mean(adm_1_phone_sim_dat$value)

################################################################################
################################################################################

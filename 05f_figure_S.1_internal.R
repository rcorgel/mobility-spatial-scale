################################################################################
# File Name: 05f_figure_S.1_internal                                           #
#                                                                              #
# Purpose:   Create figure S.1 for the manuscript.                             #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure functions                                        #
#            3. Create sub figures                                             #
#            4. Combine sub figures                                            #
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
    geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, 
                scale="width", width = 0.6) +
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

# Make a scatter plot
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

########################
# 3. CREATE SUBFIGURES #
########################

############################
# FORMAT MOBILE PHONE DATA #
############################

# Load phone mobility data
load('./tmp/fmt_adm_3_phone_mobility_dat.RData')
load('./tmp/fmt_adm_2_phone_mobility_dat.RData')
load('./tmp/fmt_adm_1_phone_mobility_dat.RData')

# Load population data
load('./tmp/adm_population_dat.RData')

# Calculate internal trip proportions
# Administrative Level 3
adm_3_phone_dat <- adm_3_phone_mobility_long |> 
  dplyr::filter(origin == destination) |>
  dplyr::mutate(level = '3')
adm_3_phone_dat <- left_join(adm_3_phone_dat, adm_3_population_dat[, c(1, 4)], 
                             by = c('origin' = 'adm_3_mobility'))
adm_3_phone_dat <- adm_3_phone_dat |> dplyr::rename('population' = 'population_2020_adm_3')

# Administrative Level 2
adm_2_phone_dat <- adm_2_phone_mobility_long |> 
  dplyr::filter(origin == destination) |>
  dplyr::mutate(level = '2')
adm_2_phone_dat <- left_join(adm_2_phone_dat, adm_2_population_dat[, c(1, 3)], 
                             by = c('origin' = 'adm_2'))
adm_2_phone_dat <- adm_2_phone_dat |> dplyr::rename('population' = 'population_2020_adm_2')

# Administrative Level 1
adm_1_phone_dat <- adm_1_phone_mobility_long |> 
  dplyr::filter(origin == destination) |>
  dplyr::mutate(level = '1')
adm_1_phone_dat <- left_join(adm_1_phone_dat, adm_1_population_dat[, c(1, 2)], 
                             by = c('origin' = 'adm_1'))
adm_1_phone_dat <- adm_1_phone_dat |> dplyr::rename('population' = 'population_2020_adm_1')

###############################
# SIMULATED MOBILE PHONE DATA #
###############################

# Load sim mobility data
load('./tmp/fmt_adm_3_sim_mobility_dat.RData')
load('./tmp/fmt_adm_2_sim_mobility_dat.RData')
load('./tmp/fmt_adm_1_sim_mobility_dat.RData')

# Calculate internal trip proportions
# Administrative Level 3
adm_3_sim_dat <- adm_3_sim_mobility_long |> 
  dplyr::filter(origin == destination) |>
  dplyr::mutate(level = '3')
adm_3_sim_dat <- left_join(adm_3_sim_dat, adm_3_population_dat[, c(1, 4)], 
                           by = c('origin' = 'adm_3_mobility'))
adm_3_sim_dat <- adm_3_sim_dat |> dplyr::rename('population' = 'population_2020_adm_3')

# Administrative Level 2
adm_2_sim_dat <- adm_2_sim_mobility_long |> 
  dplyr::filter(origin == destination) |>
  dplyr::mutate(level = '2')
adm_2_sim_dat <- left_join(adm_2_sim_dat, adm_2_population_dat[, c(1, 3)], 
                           by = c('origin' = 'adm_2'))
adm_2_sim_dat <- adm_2_sim_dat |> dplyr::rename('population' = 'population_2020_adm_2')

# Administrative Level 1
adm_1_sim_dat <- adm_1_sim_mobility_long |> 
  dplyr::filter(origin == destination) |>
  dplyr::mutate(level = '1')
adm_1_sim_dat <- left_join(adm_1_sim_dat, adm_1_population_dat[, c(1, 2)], 
                           by = c('origin' = 'adm_1'))
adm_1_sim_dat <- adm_1_sim_dat |> dplyr::rename('population' = 'population_2020_adm_1')

###################
# APPEND AND MAKE #
###################

# Append admin data together
# Admin 1
adm_1_phone_dat$level <- 'Observed'
adm_1_sim_dat$level <- 'Simulated'
mean(adm_1_phone_dat$value)
mean(adm_1_sim_dat$value)
adm_1_plot <- rbind(adm_1_phone_dat, adm_1_sim_dat)

# Admin 2
adm_2_phone_dat$level <- 'Observed'
adm_2_sim_dat$level <- 'Simulated'
mean(adm_2_phone_dat$value)
mean(adm_2_sim_dat$value)
adm_2_plot <- rbind(adm_2_phone_dat, adm_2_sim_dat)

# Admin 3
adm_3_phone_dat$level <- 'Observed'
adm_3_sim_dat$level <- 'Simulated'
mean(adm_3_phone_dat$value)
mean(adm_3_sim_dat$value)
adm_3_plot <- rbind(adm_3_phone_dat, adm_3_sim_dat)

# Create mobility data violin plots
phone_violin_plot_1 <- make_violin_plot(data = adm_1_plot, color = '#4292C6')
phone_violin_plot_2 <- make_violin_plot(data = adm_2_plot, color = '#41AE76')
phone_violin_plot_3 <- make_violin_plot(data = adm_3_plot, color = '#807DBA')

# Combine all data
adm_3_phone_dat$level <- 'Division'
adm_2_phone_dat$level <- 'District'
adm_1_phone_dat$level <- 'Province'
adm_phone_dat <- rbind(adm_1_phone_dat, adm_2_phone_dat, adm_3_phone_dat)

adm_3_sim_dat$level <- 'Division'
adm_2_sim_dat$level <- 'District'
adm_1_sim_dat$level <- 'Province'
adm_sim_dat <- rbind(adm_1_sim_dat, adm_2_sim_dat, adm_3_sim_dat)

# Create scatter plots
phone_scatter_plot <- make_scatter_plot(data = adm_phone_dat)
phone_scatter_plot_sim <- make_scatter_plot(data = adm_sim_dat)

#########################
# 3. COMBINE SUBFIGURES #
#########################

# Create figure, with labels
internal_plots <- plot_grid(phone_violin_plot_1 + stat_summary(fun.data=data_summary) + 
                              ggtitle('Provinces'),
                            phone_violin_plot_2 + stat_summary(fun.data=data_summary) + 
                              ggtitle('Districts'),
                            phone_violin_plot_3 + stat_summary(fun.data=data_summary) + 
                              ggtitle('Divisions'),
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
ggsave('./figs/figure_S.1_internal.jpg', plot = internal_plots_final, height = 12, width = 18)

################################################################################
################################################################################


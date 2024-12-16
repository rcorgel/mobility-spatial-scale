################################################################################
# File Name: 04h_create_figure_model_diff_dur                                  #
#                                                                              #
# Purpose:   Create figures that describe differences between models with      #
#            various spatial levels of mobility data, exploring disease        #
#            duration.                                                         #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load figure data and create sub plots                          #
#            3. Create figure                                                  #
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

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

############################################
# 2. LOAD FIGURE DATA AND CREATE SUB PLOTS #
############################################

# Load model results
load('./tmp/generation_time_model_results.RData')

# Load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Join comparison model results (Admin 3 and Admin 1)
adm_1_adm_3_col <- left_join(adm_3_at_1_mp_col, adm_1_at_1_mp_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_3_mad <- left_join(adm_3_at_1_mp_mad, adm_1_at_1_mp_mad, by = c('intro_loc' = 'intro_loc'))
# Create label variable
adm_1_adm_3_col$`Introduction Location` <- 'Colombo (Urban)'
adm_1_adm_3_mad$`Introduction Location` <- 'Madhu (Rural)'
# Append urban and rural introduction location results
adm_1_adm_3 <- rbind(adm_1_adm_3_col, adm_1_adm_3_mad)

# Join comparison model results (Admin 2 and Admin 1)
adm_1_adm_2_col <- left_join(adm_2_at_1_mp_col, adm_1_at_1_mp_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_2_mad <- left_join(adm_2_at_1_mp_mad, adm_1_at_1_mp_mad, by = c('intro_loc' = 'intro_loc'))
# Create label variable
adm_1_adm_2_col$`Introduction Location` <- 'Colombo (Urban)'
adm_1_adm_2_mad$`Introduction Location` <- 'Madhu (Rural)'
# Append urban and rural introduction location results
adm_1_adm_2 <- rbind(adm_1_adm_2_col, adm_1_adm_2_mad)

# Join comparison model results (Admin 3 and Admin 2)
adm_2_adm_3_col <- left_join(adm_3_at_2_mp_col, adm_2_at_2_mp_col, by = c('intro_loc' = 'intro_loc'))
adm_2_adm_3_mad <- left_join(adm_3_at_2_mp_mad, adm_2_at_2_mp_mad, by = c('intro_loc' = 'intro_loc'))
# Create label variable
adm_2_adm_3_col$`Introduction Location` <- 'Colombo (Urban)'
adm_2_adm_3_mad$`Introduction Location` <- 'Madhu (Rural)'
# Append urban and rural introduction location results
adm_2_adm_3 <- rbind(adm_2_adm_3_col, adm_2_adm_3_mad)

# Calculate magnitude and timing differences
# Admin 1 - Admin 3
adm_1_adm_3$intro_diff <- adm_1_adm_3$intro_time.y - adm_1_adm_3$intro_time.x
adm_1_adm_3$mag_diff <- adm_1_adm_3$magnitude.y - adm_1_adm_3$magnitude.x

# Admin 1 - Admin 2
adm_1_adm_2$intro_diff <- adm_1_adm_2$intro_time.y - adm_1_adm_2$intro_time.x
adm_1_adm_2$mag_diff <- adm_1_adm_2$magnitude.y - adm_1_adm_2$magnitude.x

# Admin 2 - Admin 3
adm_2_adm_3$intro_diff <- adm_2_adm_3$intro_time.y - adm_2_adm_3$intro_time.x
adm_2_adm_3$mag_diff <- adm_2_adm_3$magnitude.y - adm_2_adm_3$magnitude.x

# Set plot theme
theme_plot <- theme(axis.title = element_text(size=26),
                    axis.text = element_text(size=22),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.text = element_text(size = 26),
                    legend.title = element_text(size = 26),
                    plot.title = element_text(size=30, hjust = 0.5))

# Create sub plots
plot_1 <- ggplot(data = adm_1_adm_3) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Duration of Infectiousness (days)') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,2000000), limits = c(-7000000, 7000000))


plot_2 <- ggplot(data = adm_1_adm_3) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Duration of Infectiousness (days)') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-100,100,25), limits = c(-105, 105)) 

plot_3 <- ggplot(data = adm_1_adm_2) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Duration of Infectiousness (days)') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,2000000), limits = c(-7000000, 7000000))


plot_4 <- ggplot(data = adm_1_adm_2) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Duration of Infectiousness (days)') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-100,100,25), limits = c(-105, 105)) 

plot_5 <- ggplot(data = adm_2_adm_3) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Duration of Infectiousness (days)') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,2000000), limits = c(-7000000, 7000000))


plot_6 <- ggplot(data = adm_2_adm_3) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 2.5) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Duration of Infectiousness (days)') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-100,100,25), limits = c(-105, 105)) 

# Create legends
leg_1 <- ggplot(data = adm_2_adm_3) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 4) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Duration of Infectiousness (days)') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'bottom') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,2000000), limits = c(-7000000, 7000000))


leg_2 <- ggplot(data = adm_2_adm_3) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 4) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2) +
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 4) + 
  theme_minimal() + ylab('Difference in Introduction Timing') + xlab('Duration of Infectiousness (days)') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'bottom') + 
  scale_y_continuous(breaks = seq(-80,80,20), limits = c(-85, 85)) 

####################
# 3. CREATE FIGURE #
####################

# Grab legends
legend_1 <- get_legend(leg_1)
legend_2 <- get_legend(leg_2)

# Create and Combine figures
figure_a <- plot_grid(plot_3 + ggtitle('Admin 1 - Admin 2'), 
                      plot_1 + ggtitle('Admin 1 - Admin 3'), 
                      plot_5 + ggtitle('Admin 2 - Admin 3'),
                      nrow = 1,
                      labels = c('(a)', '(b)', '(c)'),
                      label_size = 26, hjust = 0)

figure_b <- plot_grid(plot_4, plot_2, plot_6,
                      nrow = 1,
                      labels = c( 
                        '(d)', '(e)', '(f)'),
                      label_size = 26, hjust = 0)

figure <- plot_grid(figure_a, legend_1, figure_b, legend_2,
  rel_heights = c(1, 0.1, 1, 0.1),
  nrow = 4)

# Save figure
ggsave('./figs/figure_model_diff_dur.jpg', plot = figure, height = 18, width = 26)

# Create poster figure
# Load previous figures
load('./tmp/trans_figs.RData')
load('./tmp/intro_figs.RData')

# Set theme
theme_plot <- theme(axis.title = element_text(size=50),
                    axis.text = element_text(size=35),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.text = element_text(size = 34),
                    legend.title = element_text(size = 34),
                    plot.title = element_text(size=54, hjust = 0.5))

plot_1_dur <- ggplot(data = adm_1_adm_3) +
  geom_point(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), alpha = 0.3, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = intro_loc, y = mag_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 4) + 
  theme_minimal() + ylab('Difference in Magnitude') + xlab('Duration (days)') + theme_plot +
  scale_color_manual(values=c("#4292C6", "#DF65B0")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-6000000, 6000000,2000000), limits = c(-7000000, 7000000))


plot_2_dur <- ggplot(data = adm_1_adm_3) +
  geom_point(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), alpha = 0.3, size = 5) + 
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2, linewidth = 2) +
  geom_smooth(aes(x = intro_loc, y = intro_diff, color = `Introduction Location`), method = "lm", se = TRUE, linewidth = 4) + 
  theme_minimal() + ylab('Difference in Arrival Timing') + xlab('Duration (days)') + theme_plot +
  scale_color_manual(values=c("#F16913", "#41AE76")) + theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(-100,100,25), limits = c(-105, 105)) 

# Set legends
legend_1 <- get_legend(plot_1_dur + theme(legend.position = 'bottom'))
legend_2 <- get_legend(plot_2_dur + theme(legend.position = 'bottom'))


figure_a <- plot_grid(plot_1_intro + ggtitle('Introduction Location'), 
                       plot_1_inf + ggtitle('Disease Transmissibility'), 
                       plot_1_dur + ggtitle('Infectious Duration'),
                       nrow = 1)

figure_b <- plot_grid(plot_2_intro, plot_2_inf, plot_2_dur,
  nrow = 1)

figure <- plot_grid(figure_a, legend_1, figure_b, legend_2,
  rel_heights = c(1, 0.1, 1, 0.1),
  nrow = 4)

# Save poster figure
ggsave('./figs/figure_model_diff_poster.jpg', plot = figure, height = 22, width = 30)

################################################################################
################################################################################

################################################################################
# File Name: 04c_create_figure_mobility_matrices                               #
#                                                                              #
# Purpose:   Create mobility matrices colored by trip proportion for each      #
#            administrative level and data source.                             #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create figure functions                                        #
#            3. Load figure data and create sub plots                          #
#            4. Create figure and save                                         #
#            5. Text callouts                                                  #
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
library(ggpubr)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

##############################
# 2. CREATE FIGURE FUNCTIONS #
##############################

# Set theme
matrix_theme <- theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_text(size=26),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20),
      legend.position = "bottom",
      legend.box="vertical",
      legend.margin=margin(),
      strip.background = element_blank(),
      legend.spacing.y = unit(0.25, 'cm'),
      legend.key.size = unit(1, 'cm'),
      strip.text = element_text(size = 16),
      plot.title = element_text(size=26, hjust = 0.5))

# Make a mobility matrix colored by trip proportion for data with NAs
make_matrix_plot_na <- function(data, color) {
  plot <- ggplot(data, aes(x = as.character(destination), y = as.character(origin), fill = value_cat)) +
    geom_tile(height = 1, width = 1) +
    scale_fill_manual(values = c(brewer.pal(n = 4, name = color), '#FFFFFF'),
                      breaks = c("1", "2", 
                                 "3", "4", "NA"),
                      labels = c("< 0.001", "0.001-0.01", 
                                 "0.01-0.1", "0.1-1.0", "NA")) +
    xlab("Destination") +
    theme_bw() + 
    ylab("Origin") +
    ggtitle(' ') + 
    guides(fill = guide_legend(title = "", byrow = TRUE)) + 
    matrix_theme
  return(plot)
}

# Make a mobility matrix colored by trip proportion for data with no NAs
make_matrix_plot <- function(data, color) {
  plot <- ggplot(data, aes(x = as.character(destination), y = as.character(origin), fill = value_cat)) +
    geom_tile(height = 1, width = 1) +
    scale_fill_manual(values = brewer.pal(n = nlevels(data$value_cat), name = color),
                      breaks = c("< 0.001", "0.001-0.01", 
                                 "0.01-0.1", "0.1-1.0"),
                      labels = c("< 0.001", "0.001-0.01", 
                                 "0.01-0.1", "0.1-1.0")) +
    xlab("Destination") +
    theme_bw() + 
    ylab("Origin") +
    ggtitle(' \n') + 
    guides(fill = guide_legend(title = "", byrow = TRUE)) + 
    matrix_theme
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

# Administrative Unit 3
# Make category variable
adm_3_phone_mobility_long_code$value_cat <- cut(adm_3_phone_mobility_long_code$value,
                                                breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                                labels = c("< 0.001", "0.001-0.01", 
                                                           "0.01-0.1", "0.1-1.0"))
adm_3_phone_mobility_long_code$value_cat <- ifelse(is.na(adm_3_phone_mobility_long_code$value_cat), "NA", adm_3_phone_mobility_long_code$value_cat)

# Create plot
adm_3_phone_plot <- make_matrix_plot_na(data = adm_3_phone_mobility_long_code, color = 'Blues')

# Administrative Unit 2
# Make category variable
adm_2_phone_mobility_long_code$value_cat <- cut(adm_2_phone_mobility_long_code$value,
                                                breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                                labels = c("< 0.001", "0.001-0.01", 
                                                           "0.01-0.1", "0.1-1.0"))
# Create plot
adm_2_phone_plot <- make_matrix_plot(data = adm_2_phone_mobility_long_code, color = 'BuGn')

# Administrative Unit 1
# Make category variable
adm_1_phone_mobility_long_code$value_cat <- cut(adm_1_phone_mobility_long_code$value,
                                                breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                                labels = c("< 0.001", "0.001-0.01", 
                                                           "0.01-0.1", "0.1-1.0"))
# Create plot
adm_1_phone_plot <- make_matrix_plot(data = adm_1_phone_mobility_long_code, color = 'Purples')

###############################
# SIMULATED MOBILE PHONE DATA #
###############################

# Load simulated mobility data
load('./mobility-spatial-scale/simulated data/mobile_phone_sim_prop_dat.RData')

# Load crosswalk with codes
admin_xwalk <- read.csv('./tmp/admin_xwalk.csv')

# Reshape matrices to long format and merge on codes

# Administrative Unit 3
# Reshape to long
adm_3_phone_mobility_sim_dat <- reshape2::melt(adm_3_phone_pred_mobility_mat, 
                                               id.vars = 'adm_3_origin', 
                                               variable.name = 'adm_3_destination')  
# Rename variables
adm_3_phone_mobility_sim_dat <- adm_3_phone_mobility_sim_dat %>% 
  dplyr::rename('adm_3_origin' = 'Var1',
         'adm_3_destination' = 'Var2')
# Join on codes
adm_3_phone_mobility_sim_dat <- left_join(adm_3_phone_mobility_sim_dat, admin_xwalk[, c(1, 4)],
                                          by = c('adm_3_origin' = 'adm_3'))
adm_3_phone_mobility_sim_dat <- left_join(adm_3_phone_mobility_sim_dat, admin_xwalk[, c(1, 4)],
                                          by = c('adm_3_destination' = 'adm_3'))
# Rename codes
adm_3_phone_mobility_sim_dat <- adm_3_phone_mobility_sim_dat %>%
  dplyr::rename('origin' = 'adm_3_code.x',
         'destination' = 'adm_3_code.y')
# Make category variable
adm_3_phone_mobility_sim_dat$value_cat <- cut(adm_3_phone_mobility_sim_dat$value,
                                                breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                                labels = c("< 0.001", "0.001-0.01", 
                                                           "0.01-0.1", "0.1-1.0"))
# Create plot
adm_3_phone_plot_sim <- make_matrix_plot(data = adm_3_phone_mobility_sim_dat, color = 'Blues')

# Administrative Unit 2
# Subset x walk to admin 2
admin_xwalk_2 <- admin_xwalk %>% group_by(adm_2) %>%
  distinct(adm_2, adm_2_code)
# Reshape to long
adm_2_phone_mobility_sim_dat <- reshape2::melt(adm_2_phone_pred_mobility_mat, 
                                               id.vars = 'adm_2_origin', 
                                               variable.name = 'adm_2_destination')  
# Rename variables
adm_2_phone_mobility_sim_dat <- adm_2_phone_mobility_sim_dat %>% 
  dplyr::rename('adm_2_origin' = 'Var1',
         'adm_2_destination' = 'Var2')
# Join on codes
adm_2_phone_mobility_sim_dat <- left_join(adm_2_phone_mobility_sim_dat, admin_xwalk_2,
                                          by = c('adm_2_origin' = 'adm_2'))
adm_2_phone_mobility_sim_dat <- left_join(adm_2_phone_mobility_sim_dat, admin_xwalk_2,
                                          by = c('adm_2_destination' = 'adm_2'))
# Rename codes
adm_2_phone_mobility_sim_dat <- adm_2_phone_mobility_sim_dat %>%
  dplyr::rename('origin' = 'adm_2_code.x',
         'destination' = 'adm_2_code.y')
# Make category variable
adm_2_phone_mobility_sim_dat$value_cat <- cut(adm_2_phone_mobility_sim_dat$value,
                                                breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                                labels = c("< 0.001", "0.001-0.01", 
                                                           "0.01-0.1", "0.1-1.0"))
# Create plot
adm_2_phone_plot_sim <- make_matrix_plot(data = adm_2_phone_mobility_sim_dat, color = 'BuGn')

# Administrative Unit 1
# Subset x walk to admin 1
admin_xwalk_1 <- admin_xwalk %>% group_by(adm_1) %>%
  distinct(adm_1, adm_1_code)
# Reshape to long
adm_1_phone_mobility_sim_dat <- reshape2::melt(adm_1_phone_pred_mobility_mat, 
                                               id.vars = 'adm_1_origin', 
                                               variable.name = 'adm_1_destination')   
# Rename variables
adm_1_phone_mobility_sim_dat <- adm_1_phone_mobility_sim_dat %>% 
  dplyr::rename('adm_1_origin' = 'Var1',
         'adm_1_destination' = 'Var2')
# Join on codes
adm_1_phone_mobility_sim_dat <- left_join(adm_1_phone_mobility_sim_dat, admin_xwalk_1,
                                          by = c('adm_1_origin' = 'adm_1'))
adm_1_phone_mobility_sim_dat <- left_join(adm_1_phone_mobility_sim_dat, admin_xwalk_1,
                                          by = c('adm_1_destination' = 'adm_1'))
# Rename codes
adm_1_phone_mobility_sim_dat <- adm_1_phone_mobility_sim_dat %>%
  dplyr::rename('origin' = 'adm_1_code.x',
         'destination' = 'adm_1_code.y')
# Make category variable
adm_1_phone_mobility_sim_dat$value_cat <- cut(adm_1_phone_mobility_sim_dat$value,
                                                breaks = c(-Inf, 0.001, 0.01, 0.1, Inf),
                                                labels = c("< 0.001", "0.001-0.01", 
                                                           "0.01-0.1", "0.1-1.0"))
# Create plot
adm_1_phone_plot_sim <- make_matrix_plot(data = adm_1_phone_mobility_sim_dat, color = 'Purples')

# Save formatted simulated data
save(list = c('adm_3_phone_mobility_sim_dat',
              'adm_2_phone_mobility_sim_dat',
              'adm_1_phone_mobility_sim_dat'), 
     file = './tmp/simulated_phone_mobility_format.RData')


#############################
# 4. CREATE FIGURE AND SAVE #
#############################

# Create Legends
legend_3 <- get_legend(adm_3_phone_plot)
adm_2_phone_plot_na <- make_matrix_plot_na(data = adm_3_phone_mobility_long_code, color = 'BuGn')
legend_2 <- get_legend(adm_2_phone_plot_na)
adm_1_phone_plot_na <- make_matrix_plot_na(data = adm_3_phone_mobility_long_code, color = 'Purples')
legend_1 <- get_legend(adm_1_phone_plot_na)
save(list = c('legend_1', 'legend_2', 'legend_3'), 
     file = './tmp/matrix_legends.RData')

# Create figure, with labels
figure <- plot_grid(adm_1_phone_plot + theme(legend.position = "none") + 
                      ggtitle('Administrative Level 1') + ylab("Mobile Phone Data \n\nOrigin"), 
                    adm_2_phone_plot + theme(legend.position = "none") + 
                      ggtitle('Administrative Level 2'),
                    adm_3_phone_plot + theme(legend.position = "none") + 
                      ggtitle('Administrative Level 3'),
                    adm_1_phone_plot_sim + theme(legend.position = "none") + ylab("Simulated Mobile Phone Data \n\nOrigin"),
                    adm_2_phone_plot_sim + theme(legend.position = "none"), 
                    adm_3_phone_plot_sim + theme(legend.position = "none"),
                    legend_1, legend_2, legend_3,
                    nrow = 3,
                    rel_heights = c(1, 1, 0.15),
                    labels = c('(a)', 
                               '(b)', 
                               '(c)', 
                               '(d)',
                               '(e)',
                               '(f)', 
                               ' ',
                               ' ', 
                               ' '),
                    label_size = 26, hjust = 0)

# Save figure
ggsave('./figs/figure_mobility_matrices.jpg', plot = figure, height = 17, width = 25)

####################
# 5. TEXT CALLOUTS #
####################

# Percent missing
sum(is.na(adm_1_phone_mobility_long_code$value)) / length(adm_1_phone_mobility_long_code$value)
sum(is.na(adm_2_phone_mobility_long_code$value)) / length(adm_2_phone_mobility_long_code$value)
sum(is.na(adm_3_phone_mobility_long_code$value)) / length(adm_3_phone_mobility_long_code$value)

# Internal/External trip proportions at each admin level for mobile phone data
adm_2_phone_mobility_long_code_1 <- adm_2_phone_mobility_long_code %>% filter(origin == destination)
adm_2_phone_mobility_long_code_2 <- adm_2_phone_mobility_long_code %>% filter(origin != destination)

mean(adm_2_phone_mobility_long_code_1$value)
mean(adm_2_phone_mobility_long_code_2$value)

adm_1_phone_mobility_long_code_1 <- adm_1_phone_mobility_long_code %>% filter(origin == destination)
adm_1_phone_mobility_long_code_2 <- adm_1_phone_mobility_long_code %>% filter(origin != destination)

mean(adm_1_phone_mobility_long_code_1$value)
mean(adm_1_phone_mobility_long_code_2$value)

adm_3_phone_mobility_long_code_1 <- adm_3_phone_mobility_long_code %>% filter(origin == destination)
adm_3_phone_mobility_long_code_2 <- adm_3_phone_mobility_long_code %>% filter(origin != destination)

mean(adm_3_phone_mobility_long_code_1$value)
mean(adm_3_phone_mobility_long_code_2$value, na.rm = TRUE)

adm_3_phone_mobility_sim_dat$origin <- as.character(adm_3_phone_mobility_sim_dat$origin)
adm_3_phone_mobility_sim_dat$destination <- as.character(adm_3_phone_mobility_sim_dat$destination)

adm_3_phone_mobility_long_code <- left_join(adm_3_phone_mobility_long_code, adm_3_phone_mobility_sim_dat, 
                                            by = c('origin', 'destination'))
cor(adm_3_phone_mobility_long_code$value.y, adm_3_phone_mobility_long_code$value.x, use = 'complete.obs')

adm_2_phone_mobility_sim_dat$origin <- as.character(adm_2_phone_mobility_sim_dat$origin)
adm_2_phone_mobility_sim_dat$destination <- as.character(adm_2_phone_mobility_sim_dat$destination)

adm_2_phone_mobility_long_code <- left_join(adm_2_phone_mobility_long_code, adm_2_phone_mobility_sim_dat, 
                                            by = c('origin', 'destination'))
cor(adm_2_phone_mobility_long_code$value.y, adm_2_phone_mobility_long_code$value.x, use = 'complete.obs')


adm_1_phone_mobility_sim_dat$origin <- as.character(adm_1_phone_mobility_sim_dat$origin)
adm_1_phone_mobility_sim_dat$destination <- as.character(adm_1_phone_mobility_sim_dat$destination)

adm_1_phone_mobility_long_code <- left_join(adm_1_phone_mobility_long_code, adm_1_phone_mobility_sim_dat, 
                                            by = c('origin', 'destination'))
cor(adm_1_phone_mobility_long_code$value.y, adm_1_phone_mobility_long_code$value.x, use = 'complete.obs')



################################################################################
################################################################################

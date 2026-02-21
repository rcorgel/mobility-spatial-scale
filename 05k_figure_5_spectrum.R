################################################################################
# File Name: 05j_figure_5_S.5_spectrum                                         #
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

# Load libraries
library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(sf)
library(scales)
library(reshape2)
library(ggpubr)
library(parallel)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load model 
source('./mobility-spatial-scale/04_metapop_model_2.R')

############################################
# 2. LOAD FIGURE DATA AND CREATE SUB PLOTS #
############################################

###################################
# Load introduction location data #
###################################

load('./tmp/introduction_location_model_results_obs.RData')
load('./tmp/introduction_location_model_results_obs_sim.RData')

# Load metapopulation model data at different scales
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

adm_3_at_1_mp <- readRDS('tmp/adm_3_at_1_mp')
adm_3_at_2_mp <- readRDS('tmp/adm_3_at_2_mp')
adm_2_at_2_mp <- readRDS('tmp/adm_2_at_2_mp')
adm_2_at_1_mp <- readRDS('tmp/adm_2_at_1_mp')
adm_1_at_1_mp <- readRDS('tmp/adm_1_at_1_mp')


load('./tmp/rescale_phone_mobility_dat.RData')

# Add on administrative name vectors
adm_1_at_1_mp$adm_1 <- adm_1_name_vec
adm_2_at_1_mp$adm_2 <- adm_2_name_vec
adm_2_at_2_mp$adm_2 <- adm_2_name_vec
adm_3_at_1_mp$adm_3 <- adm_3_name_vec
adm_3_at_2_mp$adm_3 <- adm_3_name_vec
adm_1_at_1_sim$adm_1 <- adm_1_name_vec
adm_2_at_1_sim$adm_2 <- adm_2_name_vec
adm_2_at_2_sim$adm_2 <- adm_2_name_vec
adm_3_at_1_sim$adm_3 <- adm_3_name_vec
adm_3_at_2_sim$adm_3 <- adm_3_name_vec

# Add on population vectors
adm_1_at_1_mp$adm_1_pop <- adm_1_pop_vec
adm_2_at_1_mp$adm_2_pop <- adm_2_pop_vec
adm_2_at_2_mp$adm_2_pop <- adm_2_pop_vec
adm_3_at_1_mp$adm_3_pop <- adm_3_pop_vec
adm_3_at_2_mp$adm_3_pop <- adm_3_pop_vec
adm_1_at_1_sim$adm_1_pop <- adm_1_pop_vec
adm_2_at_1_sim$adm_2_pop <- adm_2_pop_vec
adm_2_at_2_sim$adm_2_pop <- adm_2_pop_vec
adm_3_at_1_sim$adm_3_pop <- adm_3_pop_vec
adm_3_at_2_sim$adm_3_pop <- adm_3_pop_vec



# Add on stay vectors

adm_3_phone_mobility_mat_0 <- adm_3_phone_mobility_mat
adm_3_phone_mobility_mat_0[is.na(adm_3_phone_mobility_mat_0)] <- 0
adm_2_phone_mobility_mat_0 <- adm_2_phone_mobility_mat
adm_2_phone_mobility_mat_0[is.na(adm_2_phone_mobility_mat_0)] <- 0
adm_1_phone_mobility_mat_0 <- adm_1_phone_mobility_mat
adm_1_phone_mobility_mat_0[is.na(adm_1_phone_mobility_mat_0)] <- 0


library(igraph)
g_adm3 <- graph_from_adjacency_matrix(adm_3_phone_mobility_mat_0, mode = "directed", weighted = TRUE)
g_adm2 <- graph_from_adjacency_matrix(adm_2_phone_mobility_mat_0, mode = "directed", weighted = TRUE)
g_adm1 <- graph_from_adjacency_matrix(adm_1_phone_mobility_mat_0, mode = "directed", weighted = TRUE)


distances_adm3 <- distances(g_adm3, v = 41, mode = "out")
distances_adm2 <- distances(g_adm2, v = 5, mode = "out")

mean(distances_adm3[is.finite(distances_adm3)])
mean(distances_adm2[is.finite(distances_adm2)])

betweenness_adm3 <- betweenness(g_adm3, normalized = TRUE)
betweenness_adm2 <- betweenness(g_adm2, normalized = TRUE)
betweenness_adm1 <- betweenness(g_adm1, normalized = TRUE)


# Add on between vectors
adm_1_at_1_mp$adm_1_bet <- betweenness_adm1
adm_2_at_1_mp$adm_2_bet <- betweenness_adm2
adm_2_at_2_mp$adm_2_bet <- betweenness_adm2
adm_3_at_1_mp$adm_3_bet <- betweenness_adm3
adm_3_at_2_mp$adm_3_bet <- betweenness_adm3

as.matrix(adm_3_phone_mobility_mat_rescale_adm_2)


adm_3 <- mclapply(1:100, run_seir_model, beta = 0.2142857, gamma = 1/7, sigma = 1/2, prop_s = 0.90,
                  adm_name_vec = adm_3_name_vec, adm_level = '3',
                  pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = 41,
                  adm_x_walk = adm_3_x_walk, travel_mat = adm_3_phone_mobility_mat,
                  max_time = 365, time_step = 1)

adm_3_results <- do.call(rbind, adm_3)

res_3 <- adm_3_results |> 
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(run_num, time, adm_2) |> 
  mutate(sum_incid_I = sum(incid_I)) |>
  distinct(run_num, time, adm_2, sum_incid_I) |> 
  ungroup() |>
  group_by(run_num, adm_2) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(sum_incid_I),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  dplyr::filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(run_num, time) |>
  group_by(run_num) |>
  arrange(time) |>
  mutate(intro_loc = 'Col',
         Scale = 'Division',
         Count = row_number()) |>
  dplyr::select(run_num, time, adm_2, time, Count, Scale)

adm_2 <- mclapply(1:100, run_seir_model, beta = 0.2142857, gamma = 1/7, sigma = 1/2, prop_s = 0.90,
                  adm_name_vec = adm_2_name_vec, adm_level = '2',
                  pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = 5,
                  adm_x_walk = adm_2_x_walk, travel_mat = adm_2_phone_mobility_mat,
                  max_time = 365, time_step = 1)

adm_2_results <- do.call(rbind, adm_2)

res_2 <- adm_2_results |> 
  group_by(run_num) |>
  dplyr::filter(sum(incid_I) > 100) |>
  ungroup() |>
  # Sum to relevant spatial scale
  group_by(run_num, time, adm_2) |> 
  mutate(sum_incid_I = sum(incid_I)) |>
  distinct(run_num, time, adm_2, sum_incid_I) |> 
  ungroup() |>
  group_by(run_num, adm_2) |> 
  # Calculate cumulative cases at the spatial scale
  mutate(cum_sum_I = cumsum(sum_incid_I),
         intro = ifelse(cum_sum_I > 1, 1, 0)) |>
  # Indicate the first instance when cumulative > 1
  mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  # Filter to first instance for all admin
  dplyr::filter(intro_first == TRUE) |>
  ungroup() |>
  arrange(run_num, time) |>
  group_by(run_num) |>
  arrange(time) |>
  mutate(intro_loc = 'Col',
         Scale = 'District',
         Count = row_number()) |>
  dplyr::select(run_num, time, adm_2, time, Count, Scale)


test <- as.data.frame(1 - log(adm_2_phone_mobility_mat[5,]))
test$name <- rownames(test)


res <- rbind(res_2, res_3)
res <- left_join(res, test, by = c('adm_2' = 'name'))
library(forcats)

ggplot(res, aes(x = time, y = fct_reorder(adm_2, `1 - log(adm_2_phone_mobility_mat[5, ])`), fill = Scale)) +
  #geom_violin(trim = FALSE, color = 'black', linewidth = 1.5, alpha = 1, 
  #scale="width", width = 0.6, position = position_dodge(width = 0.9)) +
  geom_boxplot(position = position_dodge(width = 0.6), width=0.5, color = 'black', outlier.shape = NA, coef = 0) +
  theme_minimal() + #xlim(0, 75) +
  scale_fill_manual(values=c(  "#41AE76", "#9e9ac8")) +
  theme(legend.position = 'none') +
  ylab('Unit Number') +
  xlab('Time (days)') +
  ggtitle(' ') + coord_cartesian(xlim = c(0, 165)) +
theme(axis.title = element_text(size=26),
      axis.text = element_text(size=22),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size=30, hjust = 0.5)) 

#ggplot(res) + geom_point(aes(x = `1 - log(adm_2_phone_mobility_mat[5, ])`, y = time, color = Scale))


adm_1_at_1_mp$stay <-  diag(adm_1_phone_mobility_mat) #+ diag(adm_1_phone_mobility_mat)^2 + colSums(adm_1_phone_mobility_mat_0)
adm_2_at_1_mp$stay <-  diag(adm_2_phone_mobility_mat) #+ diag(adm_2_phone_mobility_mat)^2 + colSums(adm_2_phone_mobility_mat_0)
adm_2_at_2_mp$stay <-  diag(adm_2_phone_mobility_mat) #+ diag(adm_2_phone_mobility_mat)^2 + colSums(adm_2_phone_mobility_mat_0)
adm_3_at_1_mp$stay <-   diag(adm_3_phone_mobility_mat_0) #+ diag(adm_3_phone_mobility_mat_0)^2 + colSums(adm_3_phone_mobility_mat_0)
adm_3_at_2_mp$stay <-diag(adm_3_phone_mobility_mat_0) #+ diag(adm_3_phone_mobility_mat_0)^2 + colSums(adm_3_phone_mobility_mat_0)

# Add on administrative level crosswalks
adm_2_at_1_mp <- left_join(adm_2_at_1_mp, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_2_at_2_mp <- left_join(adm_2_at_2_mp, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_3_at_1_mp <- left_join(adm_3_at_1_mp, adm_3_x_walk, by = c('adm_3' = 'adm_3'))
adm_3_at_2_mp <- left_join(adm_3_at_2_mp, adm_3_x_walk, by = c('adm_3' = 'adm_3'))
#adm_2_at_1_sim <- left_join(adm_2_at_1_sim, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
#adm_2_at_2_sim <- left_join(adm_2_at_2_sim, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
#adm_3_at_1_sim <- left_join(adm_3_at_1_sim, adm_3_x_walk, by = c('adm_3' = 'adm_3'))
#adm_3_at_2_sim <- left_join(adm_3_at_2_sim, adm_3_x_walk, by = c('adm_3' = 'adm_3'))

# Merge together different model results at various levels
adm_1_adm_2_mp <- left_join(adm_2_at_1_mp, adm_1_at_1_mp, by = c('adm_1' = 'adm_1'))
adm_1_adm_3_mp <- left_join(adm_3_at_1_mp, adm_1_at_1_mp, by = c('adm_1' = 'adm_1'))
adm_2_adm_3_mp <- left_join(adm_3_at_2_mp, adm_2_at_2_mp, by = c('adm_2' = 'adm_2'))
#adm_1_adm_2_sim <- left_join(adm_2_at_1_sim, adm_1_at_1_sim, by = c('adm_1' = 'adm_1'))
#adm_1_adm_3_sim <- left_join(adm_3_at_1_sim, adm_1_at_1_sim, by = c('adm_1' = 'adm_1'))
#adm_2_adm_3_sim <- left_join(adm_3_at_2_sim, adm_2_at_2_sim, by = c('adm_2' = 'adm_2'))

# Calculate magnitude and timing differences
# Mobile Phone Data
adm_1_adm_2_mp$intro_diff <- adm_1_adm_2_mp$intro_time.y - adm_1_adm_2_mp$intro_time.x
adm_1_adm_2_mp$mag_diff <- adm_1_adm_2_mp$magnitude.y - adm_1_adm_2_mp$magnitude.x
adm_1_adm_2_mp$pop_rat <- adm_1_adm_2_mp$adm_2_pop / adm_1_adm_2_mp$adm_1_pop
adm_1_adm_2_mp$bet_diff <- adm_1_adm_2_mp$adm_2_bet - adm_1_adm_2_mp$adm_1_bet


adm_1_adm_3_mp$intro_diff <- adm_1_adm_3_mp$intro_time.y - adm_1_adm_3_mp$intro_time.x
adm_1_adm_3_mp$mag_diff <- adm_1_adm_3_mp$magnitude.y - adm_1_adm_3_mp$magnitude.x
adm_1_adm_3_mp$pop_rat <- adm_1_adm_3_mp$adm_3_pop / adm_1_adm_3_mp$adm_1_pop
adm_1_adm_3_mp$bet_diff <- adm_1_adm_3_mp$adm_3_bet - adm_1_adm_3_mp$adm_1_bet


adm_2_adm_3_mp$intro_diff <- adm_2_adm_3_mp$intro_time.y - adm_2_adm_3_mp$intro_time.x
adm_2_adm_3_mp$mag_diff <- adm_2_adm_3_mp$magnitude.y - adm_2_adm_3_mp$magnitude.x
adm_2_adm_3_mp$pop_rat <- adm_2_adm_3_mp$adm_3_pop / adm_2_adm_3_mp$adm_2_pop
adm_2_adm_3_mp$bet_diff <- adm_2_adm_3_mp$adm_3_bet - adm_2_adm_3_mp$adm_2_bet

# # Simulated Mobile Phone Data
# adm_1_adm_2_sim$intro_diff <- adm_1_adm_2_sim$intro_time.y - adm_1_adm_2_sim$intro_time.x
# adm_1_adm_2_sim$mag_diff <- adm_1_adm_2_sim$magnitude.y - adm_1_adm_2_sim$magnitude.x
# 
# adm_1_adm_3_sim$intro_diff <- adm_1_adm_3_sim$intro_time.y - adm_1_adm_3_sim$intro_time.x
# adm_1_adm_3_sim$mag_diff <- adm_1_adm_3_sim$magnitude.y - adm_1_adm_3_sim$magnitude.x
# 
# adm_2_adm_3_sim$intro_diff <- adm_2_adm_3_sim$intro_time.y - adm_2_adm_3_sim$intro_time.x
# adm_2_adm_3_sim$mag_diff <- adm_2_adm_3_sim$magnitude.y - adm_2_adm_3_sim$magnitude.x
# 

# Add on mobility diff
# adm_1_adm_3_mp$mobility <- (adm_3_phone_mobility_mat_0 %*% adm_3_adm_1_phone$diff)[, 1]
# adm_2_adm_3_mp$mobility <- (adm_3_phone_mobility_mat_0 %*% adm_3_adm_2_phone$diff)[, 1]
# adm_1_adm_2_mp$mobility <- (adm_2_phone_mobility_mat %*% adm_2_adm_1_phone$diff)[, 1]

# 
# adm_1_adm_3_mp$mobility <- adm_3_adm_1_phone$diff
# adm_2_adm_3_mp$mobility <- adm_3_adm_2_phone$diff
# adm_1_adm_2_mp$mobility <- adm_2_adm_1_phone$diff
# 
# 
# 
# test <- adm_3_phone_mobility_mat_0 %*% adm_3_adm_1_phone$diff
# test <- adm_3_phone_mobility_mat_0 %*% adm_3_adm_2_phone$diff
# 
# 
# 
# test <-  adm_3_phone_mobility_mat_0 %*% adm_3_adm_1_phone$diff
# 
# 
# 




# Combine Data sets
adm_1_adm_2_mp$Cat <- 'Prov. - Dist.'
adm_1_adm_3_mp$Cat <- 'Prov. - Div.'
adm_2_adm_3_mp$Cat <- 'Dist. - Div.'
# adm_1_adm_2_sim$Cat <- 'Prov. - Dist.'
# adm_1_adm_3_sim$Cat <- 'Prov. - Div.'
# adm_2_adm_3_sim$Cat <- 'Dist. - Div.'

# Select variables
# Mobile Phone
adm_1_adm_2_mp_plot <- adm_1_adm_2_mp %>% select(intro_diff, mag_diff, pop_rat,
                                                 adm_2_pop, Cat, intro_time.x, stay.x, stay.y, bet_diff) %>%
  dplyr::rename('pop' = 'adm_2_pop')
adm_1_adm_3_mp_plot <- adm_1_adm_3_mp %>% select(intro_diff, mag_diff, pop_rat,
                                                 adm_3_pop, Cat, intro_time.x, stay.x, stay.y, bet_diff) %>%
  dplyr::rename('pop' = 'adm_3_pop')
adm_2_adm_3_mp_plot <- adm_2_adm_3_mp %>% select(intro_diff, mag_diff, pop_rat,
                                                 adm_3_pop, Cat, intro_time.x, stay.x, stay.y, bet_diff) %>%
  dplyr::rename('pop' = 'adm_3_pop')
# Simulated
# adm_1_adm_2_sim_plot <- adm_1_adm_2_sim %>% select(intro_diff, mag_diff,
#                                                  adm_2_pop, Cat) %>%
#   dplyr::rename('pop' = 'adm_2_pop')
# adm_1_adm_3_sim_plot <- adm_1_adm_3_sim %>% select(intro_diff, mag_diff,
#                                                  adm_3_pop, Cat) %>%
#   dplyr::rename('pop' = 'adm_3_pop')
# adm_2_adm_3_sim_plot <- adm_2_adm_3_sim %>% select(intro_diff, mag_diff,
#                                                  adm_3_pop, Cat) %>%
#   dplyr::rename('pop' = 'adm_3_pop')

# Combine
all_comboa_mp <- rbind(adm_1_adm_2_mp_plot, adm_1_adm_3_mp_plot, adm_2_adm_3_mp_plot)
# all_comboa_sim <- rbind(adm_1_adm_2_sim_plot, adm_1_adm_3_sim_plot, adm_2_adm_3_sim_plot)

# Set plot theme
theme_plot <- theme(axis.title = element_text(size=26, color = 'black'),
                    axis.text = element_text(size=26, color = 'black'),
                    panel.grid.minor = element_blank(),
                    legend.text = element_text(size = 34),
                    legend.title = element_text(size = 34),
                    plot.title = element_text(size=38, hjust = 0.5),
                    legend.position = 'none',
                    legend.key.width = unit(1.2, 'cm'),
                    strip.text.x = element_text(size = 26))

colors_time <- c('Prov. - Dist.' = '#e6a532', 'Prov. - Div.' = '#41afaa', 'Dist. - Div.' = '#af4b91')

colors_mag <- c('Prov. - Dist.' =  '#6cc24a', 'Prov. - Div.' = '#5b7dc6', 'Dist. - Div.' = '#e8615a')


loc_intro <- ggplot(all_comboa_mp, aes(x = stay.x - stay.y, y = intro_diff)) + 
  geom_point(aes(color = Cat), alpha = 0.35, size = 4) + 
  theme_minimal() +
  geom_hline(yintercept=0, lty=2, linewidth = 2) +  # add a dotted line at x=1 after flip
  geom_smooth(aes(color = Cat), method='lm', se = T, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Difference in Prob. of Stay') + 
  #scale_color_distiller('Log(Population)\n', palette = "PuRd", direction = 1) + theme_plot +
  #scale_y_continuous(breaks = seq(-30, 30, 10), limits = c(-30, 30)) + 
  theme_plot + theme(legend.position = 'bottom') +
  
  scale_color_manual(name = '', values = colors_time) 
  #scale_x_continuous(breaks = seq(6, 14, 2), limits = c(6, 15)) +
loc_intro

loc_intro <- ggplot(all_comboa_mp[all_comboa_mp$Cat ==  'Dist. - Div.',], aes(x = stay.x - stay.y, y = pop_rat)) + 
  geom_point(aes(color = intro_diff), alpha = 0.35, size = 4) + 
  theme_minimal() +
  geom_hline(yintercept=0, lty=2, linewidth = 2) +  # add a dotted line at x=1 after flip
  #geom_smooth(aes(color = ), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Log Population Size') + 
  #scale_color_distiller('Log(Population)\n', palette = "PuRd", direction = 1) + theme_plot +
  #scale_y_continuous(breaks = seq(-30, 30, 10), limits = c(-30, 30)) + 
  theme_plot + theme(legend.position = 'bottom') +
  #scale_color_manual(name = '', values = colors_time)
#scale_x_continuous(breaks = seq(6, 14, 2), limits = c(6, 15))
  scale_color_gradient2(
    low = "red",      # Color for the low end of the scale
    mid = "white",    # Color for the midpoint
    high = "blue",    # Color for the high end
    midpoint = 0      # The data value that maps to the mid color (default is 0)
  )

loc_intro



loc_mag <- ggplot(all_comboa_mp, aes(x = stay.x - stay.y, y = mag_diff)) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_hline(yintercept=0, lty=2, linewidth = 2) +  # add a dotted line at x=1 after flip
  geom_smooth(aes(color = Cat), method='lm', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('') + 
  scale_color_manual(name = '', values = colors_mag) + theme_plot + theme(legend.position = 'bottom') +
  scale_y_continuous(breaks = seq(-500000, 500000,100000), limits = c(-500000, 500000)) 
  #scale_x_continuous(breaks = seq(6, 14, 2), limits = c(6, 15))
loc_mag










loc_intro_sim <- ggplot(all_comboa_sim, aes(x = log(pop), y = intro_diff)) + 
  geom_point(aes(color = Cat), alpha = 0.35, size = 4) + theme_minimal() +
  geom_hline(yintercept=1, lty=2, linewidth = 2) +  # add a dotted line at x=1 after flip
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('') + theme_plot +
  scale_y_continuous(breaks = seq(-120, 40, 20), limits = c(-120, 50)) +
  scale_color_manual(name = '', values = colors_time) +
  scale_x_continuous(breaks = seq(6, 14, 2), limits = c(6, 15))

loc_mag_sim <- ggplot(all_comboa_sim, aes(x = log(pop), y = mag_diff)) + 
  geom_point(aes(color = Cat), alpha = 0.35, size = 4) + theme_minimal() +
  geom_hline(yintercept=1, lty=2, linewidth = 2) +  # add a dotted line at x=1 after flip
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('') + theme_plot +
  scale_y_continuous(breaks = seq(-6000000, 9000000,3000000), limits = c(-7000000, 9000000)) +
  scale_color_manual(name = '', values = colors_mag) +
  scale_x_continuous(breaks = seq(6, 14, 2), limits = c(6, 15))

#############################
# Load tranmissibility data #
#############################

load('./tmp/transmissibility_model_results.RData')

adm_3_at_1_mp_col <- adm_3_at_1_mp_col |> select(-c(level))
adm_3_at_1_mp_mad <- adm_3_at_1_mp_mad |> select(-c(level))
adm_3_at_2_mp_col <- adm_3_at_2_mp_col |> select(-c(level))
adm_3_at_2_mp_mad <- adm_3_at_2_mp_mad |> select(-c(level))

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

# Simulated Mobile Phone Data
adm_3_at_1_sim_col <- adm_3_at_1_sim_col |> select(-c(level))
adm_3_at_1_sim_mad <- adm_3_at_1_sim_mad |> select(-c(level))
adm_3_at_2_sim_col <- adm_3_at_2_sim_col |> select(-c(level))
adm_3_at_2_sim_mad <- adm_3_at_2_sim_mad |> select(-c(level))

# Join comparison model results (Admin 2 and Admin 1)
adm_1_adm_2_col_sim <- left_join(adm_2_at_1_sim_col, adm_1_at_1_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_2_mad_sim <- left_join(adm_2_at_1_sim_mad, adm_1_at_1_sim_mad, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_3_col_sim <- left_join(adm_3_at_1_sim_col, adm_1_at_1_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_3_mad_sim <- left_join(adm_3_at_1_sim_mad, adm_1_at_1_sim_mad, by = c('intro_loc' = 'intro_loc'))
adm_2_adm_3_col_sim <- left_join(adm_3_at_2_sim_col, adm_2_at_2_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_2_adm_3_mad_sim <- left_join(adm_3_at_2_sim_mad, adm_2_at_2_sim_mad, by = c('intro_loc' = 'intro_loc'))

# Create label variable
adm_1_adm_2_col_sim$`Introduction Location` <- '\nColombo'
adm_1_adm_2_mad_sim$`Introduction Location` <- '\nMadhu'
adm_1_adm_3_col_sim$`Introduction Location` <- '\nColombo'
adm_1_adm_3_mad_sim$`Introduction Location` <- '\nMadhu'
adm_2_adm_3_col_sim$`Introduction Location` <- '\nColombo'
adm_2_adm_3_mad_sim$`Introduction Location` <- '\nMadhu'
adm_1_adm_2_mad_sim$Cat <- 'Prov. - Dist.'
adm_1_adm_2_col_sim$Cat <- 'Prov. - Dist.'
adm_1_adm_3_mad_sim$Cat <- 'Prov. - Div.'
adm_1_adm_3_col_sim$Cat <- 'Prov. - Div.'
adm_2_adm_3_mad_sim$Cat <- 'Dist. - Div.'
adm_2_adm_3_col_sim$Cat <- 'Dist. - Div.'
# Append urban and rural introduction location results
all_combos_sim <- rbind(adm_1_adm_2_col_sim, adm_1_adm_2_mad_sim, adm_1_adm_3_col_sim, adm_1_adm_3_mad_sim, adm_2_adm_3_col_sim, adm_2_adm_3_mad_sim)

# Calculate magnitude and timing differences
# Mobile Phone Data
all_combos_mp$intro_diff <- all_combos_mp$intro_time.y - all_combos_mp$intro_time.x
all_combos_mp$mag_diff <- all_combos_mp$magnitude.y - all_combos_mp$magnitude.x

# Simulated Mobile Phone Data
all_combos_sim$intro_diff <- all_combos_sim$intro_time.y - all_combos_sim$intro_time.x
all_combos_sim$mag_diff <- all_combos_sim$magnitude.y - all_combos_sim$magnitude.x

trans_intro_col <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nColombo',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Transmissibility') + 
  scale_y_continuous(breaks = seq(0, 40, 20), limits = c(0, 50)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

trans_intro_mad <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nMadhu',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Transmissibility') + 
  scale_y_continuous(breaks = seq(0, 40, 20), limits = c(0, 50)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

trans_mag_mad <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nMadhu',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Transmissibility') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(0, 6000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag) 

trans_mag_col <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nColombo',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Transmissibility') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(0, 6000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag)

trans_intro_col_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nColombo',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Transmissibility') + 
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

trans_intro_mad_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nMadhu',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Transmissibility') + 
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

trans_mag_mad_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nMadhu',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Transmissibility') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(0, 8000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag) 

trans_mag_col_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nColombo',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Transmissibility') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(0, 8000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag)

######################
# Load duration data #
######################

load('./tmp/duration_model_results.RData')

adm_3_at_1_mp_col <- adm_3_at_1_mp_col |> select(-c(level))
adm_3_at_1_mp_mad <- adm_3_at_1_mp_mad |> select(-c(level))
adm_3_at_2_mp_col <- adm_3_at_2_mp_col |> select(-c(level))
adm_3_at_2_mp_mad <- adm_3_at_2_mp_mad |> select(-c(level))

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

# Simulated Mobile Phone Data
adm_3_at_1_sim_col <- adm_3_at_1_sim_col |> select(-c(level))
adm_3_at_1_sim_mad <- adm_3_at_1_sim_mad |> select(-c(level))
adm_3_at_2_sim_col <- adm_3_at_2_sim_col |> select(-c(level))
adm_3_at_2_sim_mad <- adm_3_at_2_sim_mad |> select(-c(level))

# Join comparison model results (Admin 2 and Admin 1)
adm_1_adm_2_col_sim <- left_join(adm_2_at_1_sim_col, adm_1_at_1_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_2_mad_sim <- left_join(adm_2_at_1_sim_mad, adm_1_at_1_sim_mad, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_3_col_sim <- left_join(adm_3_at_1_sim_col, adm_1_at_1_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_3_mad_sim <- left_join(adm_3_at_1_sim_mad, adm_1_at_1_sim_mad, by = c('intro_loc' = 'intro_loc'))
adm_2_adm_3_col_sim <- left_join(adm_3_at_2_sim_col, adm_2_at_2_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_2_adm_3_mad_sim <- left_join(adm_3_at_2_sim_mad, adm_2_at_2_sim_mad, by = c('intro_loc' = 'intro_loc'))

# Create label variable
adm_1_adm_2_col_sim$`Introduction Location` <- '\nColombo'
adm_1_adm_2_mad_sim$`Introduction Location` <- '\nMadhu'
adm_1_adm_3_col_sim$`Introduction Location` <- '\nColombo'
adm_1_adm_3_mad_sim$`Introduction Location` <- '\nMadhu'
adm_2_adm_3_col_sim$`Introduction Location` <- '\nColombo'
adm_2_adm_3_mad_sim$`Introduction Location` <- '\nMadhu'
adm_1_adm_2_mad_sim$Cat <- 'Prov. - Dist.'
adm_1_adm_2_col_sim$Cat <- 'Prov. - Dist.'
adm_1_adm_3_mad_sim$Cat <- 'Prov. - Div.'
adm_1_adm_3_col_sim$Cat <- 'Prov. - Div.'
adm_2_adm_3_mad_sim$Cat <- 'Dist. - Div.'
adm_2_adm_3_col_sim$Cat <- 'Dist. - Div.'
# Append urban and rural introduction location results
all_combos_sim <- rbind(adm_1_adm_2_col_sim, adm_1_adm_2_mad_sim, adm_1_adm_3_col_sim, adm_1_adm_3_mad_sim, adm_2_adm_3_col_sim, adm_2_adm_3_mad_sim)

# Calculate magnitude and timing differences
# Mobile Phone Data
all_combos_mp$intro_diff <- all_combos_mp$intro_time.y - all_combos_mp$intro_time.x
all_combos_mp$mag_diff <- all_combos_mp$magnitude.y - all_combos_mp$magnitude.x

# Simulated Mobile Phone Data
all_combos_sim$intro_diff <- all_combos_sim$intro_time.y - all_combos_sim$intro_time.x
all_combos_sim$mag_diff <- all_combos_sim$magnitude.y - all_combos_sim$magnitude.x


dur_intro_col <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nColombo',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Duration') + 
  scale_y_continuous(breaks = seq(0, 40, 20), limits = c(0, 50)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

dur_intro_mad <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nMadhu',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Duration') + 
  scale_y_continuous(breaks = seq(0, 40, 20), limits = c(0, 50)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

dur_mag_mad <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nMadhu',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Duration') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(-1000000, 6000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag) 

dur_mag_col <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nColombo',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Duration') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(-1000000, 6000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag)

dur_intro_col_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nColombo',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Duration') + 
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

dur_intro_mad_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nMadhu',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Duration') + 
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

dur_mag_mad_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nMadhu',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Duration') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(-1000000, 8000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag) 

dur_mag_col_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nColombo',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Duration') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(-1000000, 8000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag)

####################
# Load latent data #
####################

load('./tmp/latent_model_results.RData')

adm_3_at_1_mp_col <- adm_3_at_1_mp_col |> select(-c(level))
adm_3_at_1_mp_mad <- adm_3_at_1_mp_mad |> select(-c(level))
adm_3_at_2_mp_col <- adm_3_at_2_mp_col |> select(-c(level))
adm_3_at_2_mp_mad <- adm_3_at_2_mp_mad |> select(-c(level))

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

# Simulated Mobile Phone Data
adm_3_at_1_sim_col <- adm_3_at_1_sim_col |> select(-c(level))
adm_3_at_1_sim_mad <- adm_3_at_1_sim_mad |> select(-c(level))
adm_3_at_2_sim_col <- adm_3_at_2_sim_col |> select(-c(level))
adm_3_at_2_sim_mad <- adm_3_at_2_sim_mad |> select(-c(level))

# Join comparison model results (Admin 2 and Admin 1)
adm_1_adm_2_col_sim <- left_join(adm_2_at_1_sim_col, adm_1_at_1_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_2_mad_sim <- left_join(adm_2_at_1_sim_mad, adm_1_at_1_sim_mad, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_3_col_sim <- left_join(adm_3_at_1_sim_col, adm_1_at_1_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_1_adm_3_mad_sim <- left_join(adm_3_at_1_sim_mad, adm_1_at_1_sim_mad, by = c('intro_loc' = 'intro_loc'))
adm_2_adm_3_col_sim <- left_join(adm_3_at_2_sim_col, adm_2_at_2_sim_col, by = c('intro_loc' = 'intro_loc'))
adm_2_adm_3_mad_sim <- left_join(adm_3_at_2_sim_mad, adm_2_at_2_sim_mad, by = c('intro_loc' = 'intro_loc'))

# Create label variable
adm_1_adm_2_col_sim$`Introduction Location` <- '\nColombo'
adm_1_adm_2_mad_sim$`Introduction Location` <- '\nMadhu'
adm_1_adm_3_col_sim$`Introduction Location` <- '\nColombo'
adm_1_adm_3_mad_sim$`Introduction Location` <- '\nMadhu'
adm_2_adm_3_col_sim$`Introduction Location` <- '\nColombo'
adm_2_adm_3_mad_sim$`Introduction Location` <- '\nMadhu'
adm_1_adm_2_mad_sim$Cat <- 'Prov. - Dist.'
adm_1_adm_2_col_sim$Cat <- 'Prov. - Dist.'
adm_1_adm_3_mad_sim$Cat <- 'Prov. - Div.'
adm_1_adm_3_col_sim$Cat <- 'Prov. - Div.'
adm_2_adm_3_mad_sim$Cat <- 'Dist. - Div.'
adm_2_adm_3_col_sim$Cat <- 'Dist. - Div.'
# Append urban and rural introduction location results
all_combos_sim <- rbind(adm_1_adm_2_col_sim, adm_1_adm_2_mad_sim, adm_1_adm_3_col_sim, adm_1_adm_3_mad_sim, adm_2_adm_3_col_sim, adm_2_adm_3_mad_sim)

# Calculate magnitude and timing differences
# Mobile Phone Data
all_combos_mp$intro_diff <- all_combos_mp$intro_time.y - all_combos_mp$intro_time.x
all_combos_mp$mag_diff <- all_combos_mp$magnitude.y - all_combos_mp$magnitude.x

# Simulated Mobile Phone Data
all_combos_sim$intro_diff <- all_combos_sim$intro_time.y - all_combos_sim$intro_time.x
all_combos_sim$mag_diff <- all_combos_sim$magnitude.y - all_combos_sim$magnitude.x

lat_intro_col <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nColombo',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Latent Period') + 
  scale_y_continuous(breaks = seq(0, 40, 20), limits = c(0, 50)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

lat_intro_mad <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nMadhu',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Latent Period') + 
  scale_y_continuous(breaks = seq(0, 40, 20), limits = c(0, 50)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

lat_mag_mad <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nMadhu',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Latent Period') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(-1000000, 6000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag) 

lat_mag_col <- ggplot(all_combos_mp[all_combos_mp$`Introduction Location` == '\nColombo',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Latent Period') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(-1000000, 6000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag)

lat_intro_col_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nColombo',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Latent Period') + 
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

lat_intro_mad_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nMadhu',] , aes(x = intro_loc, y = abs(intro_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Latent Period') + 
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) + theme_plot +
  scale_color_manual(name = '', values = colors_time) 

lat_mag_mad_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nMadhu',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Latent Period') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(-1000000, 8000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag) 

lat_mag_col_sim <- ggplot(all_combos_sim[all_combos_mp$`Introduction Location` == '\nColombo',], aes(x = intro_loc, y = abs(mag_diff))) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('Latent Period') + 
  scale_y_continuous(breaks = seq(0, 6000000, 3000000), limits = c(-1000000, 8000000)) + theme_plot +
  scale_color_manual(name = '', values = colors_mag)




lat_intro <- ggplot(all_combos_mp, aes(x = `Introduction Location`, y = intro_diff)) +
  geom_jitter(aes(color = intro_loc), alpha = 0.6, size = 3, width = 0.35) + theme_minimal() +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width=0.4, fill = NA, linewidth = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('') + 
  scale_color_distiller('Latent Period \n(days)', palette = "Oranges", direction = 1) + theme_plot +
  scale_y_continuous(breaks = seq(-40,40,20), limits = c(-50, 50)) + 
  facet_wrap(vars(Cat), strip.position = "bottom") +
  theme(axis.text = element_text(size=20))

lat_mag <- ggplot(all_combos_mp, aes(x = `Introduction Location`, y = mag_diff)) + 
  geom_jitter(aes(color = intro_loc), alpha = 0.6, size = 3, width = 0.35) + theme_minimal() +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width=0.4, fill = NA, linewidth = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('') + 
  scale_color_distiller('Latent Period \n(days)', palette = "Oranges", direction = 1) + theme_plot +
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000)) + 
  facet_wrap(vars(Cat), strip.position = "bottom") +
  theme(axis.text = element_text(size=20))

lat_intro_sim <- ggplot(all_combos_sim, aes(x = `Introduction Location`, y = intro_diff)) +
  geom_jitter(aes(color = intro_loc), alpha = 0.6, size = 3, width = 0.35) + theme_minimal() +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width=0.4, fill = NA, linewidth = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('') + 
  scale_color_distiller('Latent Period \n(days)', palette = "Oranges", direction = 1) + theme_plot +
  scale_y_continuous(breaks = seq(-120,40,20), limits = c(-120, 50)) + 
  facet_wrap(vars(Cat), strip.position = "bottom") +
  theme(axis.text = element_text(size=20))

lat_mag_sim <- ggplot(all_combos_sim, aes(x = `Introduction Location`, y = mag_diff)) + 
  geom_jitter(aes(color = intro_loc), alpha = 0.6, size = 3, width = 0.35) + theme_minimal() +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width=0.4, fill = NA, linewidth = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('') + 
  scale_color_distiller('Latent Period \n(days)', palette = "Oranges", direction = 1) + theme_plot +
  scale_y_continuous(breaks = seq(-6000000, 9000000,3000000), limits = c(-7000000, 9000000)) + 
  facet_wrap(vars(Cat), strip.position = "bottom") +
  theme(axis.text = element_text(size=20))



col_1 <- plot_grid(lat_intro_col + ggtitle('Altered Latent Period\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   lat_intro_mad + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)

col_2 <- plot_grid(trans_intro_col + ggtitle('Altered Transmissibility\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   trans_intro_mad + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)

col_3 <- plot_grid(dur_intro_col + ggtitle('Altered Duration\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   dur_intro_mad + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)

figure_1 <- plot_grid(loc_intro + ggtitle('Altered Introduction Location\n') + theme(legend.position = 'none'), 
                      col_1,
                      col_2,
                      col_3,
                      nrow = 1)

legend_get_1 <- get_legend(loc_intro)

col_1 <- plot_grid(lat_mag_col + ggtitle('Altered Latent Period\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   lat_mag_mad + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)

col_2 <- plot_grid(trans_mag_col + ggtitle('Altered Transmissibility\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   trans_mag_mad + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)

col_3 <- plot_grid(dur_mag_col + ggtitle('Altered Duration\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   dur_mag_mad + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)


legend_get_2 <- get_legend(loc_mag)


figure_2 <- plot_grid(loc_mag + ggtitle('Altered Introduction Location\n') + theme(legend.position = 'none') + xlab('Log Population Size'), 
                      col_1,
                      col_2,
                      col_3,
                      nrow = 1)


figure <- plot_grid(ggplot() + theme_void(), figure_1, legend_get_1, ggplot() + theme_void(), figure_2, legend_get_2,
                    nrow = 6, rel_heights = c(0.1, 1, 0.1, 0.1, 1, 0.1),
                    labels = c('Spatial Invasion Timing', '(a)', '', 'Epidemic Magnitude', '(b)', ''),
                    label_size = 34, hjust = 0)


# Save figure
ggsave('./figs/figure_model_all_combos.jpg', plot = figure, height = 22, width = 35)


col_1 <- plot_grid(lat_intro_col_sim + ggtitle('Altered Latent Period\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   lat_intro_mad_sim + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)

col_2 <- plot_grid(trans_intro_col_sim + ggtitle('Altered Transmissibility\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   trans_intro_mad_sim + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)

col_3 <- plot_grid(dur_intro_col_sim + ggtitle('Altered Duration\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   dur_intro_mad_sim + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)

figure_1 <- plot_grid(loc_intro_sim + ggtitle('Altered Introduction Location\n') + theme(legend.position = 'none'), 
                      col_1,
                      col_2,
                      col_3,
                      nrow = 1)

col_1 <- plot_grid(lat_mag_col_sim + ggtitle('Altered Latent Period\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   lat_mag_mad_sim + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)

col_2 <- plot_grid(trans_mag_col_sim + ggtitle('Altered Transmissibility\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   trans_mag_mad_sim + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)

col_3 <- plot_grid(dur_mag_col_sim + ggtitle('Altered Duration\n\nColombo Introduction') + ylab('Absolute Difference'), 
                   dur_mag_mad_sim + ggtitle('\n\nMadhu Introduction') + ylab('Absolute Difference'), nrow = 2)




figure_2 <- plot_grid(loc_mag_sim + ggtitle('Altered Introduction Location\n') + theme(legend.position = 'none') + xlab('Log Population Size'), 
                      col_1,
                      col_2,
                      col_3,
                      nrow = 1)


figure <- plot_grid(ggplot() + theme_void(), figure_1, legend_get_1, ggplot() + theme_void(), figure_2, legend_get_2,
                    nrow = 6, rel_heights = c(0.1, 1, 0.1, 0.1, 1, 0.1),
                    labels = c('Spatial Invasion Timing', '(a)', '', 'Epidemic Magnitude', '(b)', ''),
                    label_size = 34, hjust = 0)


# Save figure
ggsave('./figs/figure_model_all_combos_sim.jpg', plot = figure, height = 22, width = 35)



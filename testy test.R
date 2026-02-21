





load('./tmp/introduction_location_model_results_obs.RData')
load('./tmp/introduction_location_model_results_obs_sim.RData')

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

# Add on administrative level crosswalks
adm_2_at_1_mp <- left_join(adm_2_at_1_mp, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_2_at_2_mp <- left_join(adm_2_at_2_mp, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_3_at_1_mp <- left_join(adm_3_at_1_mp, adm_3_x_walk, by = c('adm_3' = 'adm_3'))
adm_3_at_2_mp <- left_join(adm_3_at_2_mp, adm_3_x_walk, by = c('adm_3' = 'adm_3'))
adm_2_at_1_sim <- left_join(adm_2_at_1_sim, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_2_at_2_sim <- left_join(adm_2_at_2_sim, adm_2_x_walk, by = c('adm_2' = 'adm_2'))
adm_3_at_1_sim <- left_join(adm_3_at_1_sim, adm_3_x_walk, by = c('adm_3' = 'adm_3'))
adm_3_at_2_sim <- left_join(adm_3_at_2_sim, adm_3_x_walk, by = c('adm_3' = 'adm_3'))

# Merge together different model results at various levels
adm_1_adm_2_mp <- left_join(adm_2_at_1_mp, adm_1_at_1_mp, by = c('adm_1' = 'adm_1'))
adm_1_adm_3_mp <- left_join(adm_3_at_1_mp, adm_1_at_1_mp, by = c('adm_1' = 'adm_1'))
adm_2_adm_3_mp <- left_join(adm_3_at_2_mp, adm_2_at_2_mp, by = c('adm_2' = 'adm_2'))
adm_1_adm_2_sim <- left_join(adm_2_at_1_sim, adm_1_at_1_sim, by = c('adm_1' = 'adm_1'))
adm_1_adm_3_sim <- left_join(adm_3_at_1_sim, adm_1_at_1_sim, by = c('adm_1' = 'adm_1'))
adm_2_adm_3_sim <- left_join(adm_3_at_2_sim, adm_2_at_2_sim, by = c('adm_2' = 'adm_2'))

# Calculate magnitude and timing differences
# Mobile Phone Data
adm_1_adm_2_mp$intro_diff <- adm_1_adm_2_mp$intro_time.y - adm_1_adm_2_mp$intro_time.x
adm_1_adm_2_mp$mag_diff <- adm_1_adm_2_mp$magnitude.y - adm_1_adm_2_mp$magnitude.x

adm_1_adm_3_mp$intro_diff <- adm_1_adm_3_mp$intro_time.y - adm_1_adm_3_mp$intro_time.x
adm_1_adm_3_mp$mag_diff <- adm_1_adm_3_mp$magnitude.y - adm_1_adm_3_mp$magnitude.x

adm_2_adm_3_mp$intro_diff <- adm_2_adm_3_mp$intro_time.y - adm_2_adm_3_mp$intro_time.x
adm_2_adm_3_mp$mag_diff <- adm_2_adm_3_mp$magnitude.y - adm_2_adm_3_mp$magnitude.x

# Simulated Mobile Phone Data
adm_1_adm_2_sim$intro_diff <- adm_1_adm_2_sim$intro_time.y - adm_1_adm_2_sim$intro_time.x
adm_1_adm_2_sim$mag_diff <- adm_1_adm_2_sim$magnitude.y - adm_1_adm_2_sim$magnitude.x

adm_1_adm_3_sim$intro_diff <- adm_1_adm_3_sim$intro_time.y - adm_1_adm_3_sim$intro_time.x
adm_1_adm_3_sim$mag_diff <- adm_1_adm_3_sim$magnitude.y - adm_1_adm_3_sim$magnitude.x

adm_2_adm_3_sim$intro_diff <- adm_2_adm_3_sim$intro_time.y - adm_2_adm_3_sim$intro_time.x
adm_2_adm_3_sim$mag_diff <- adm_2_adm_3_sim$magnitude.y - adm_2_adm_3_sim$magnitude.x


# Combine Data sets
adm_1_adm_2_mp$Cat <- 'Prov. - Dist.'
adm_1_adm_3_mp$Cat <- 'Prov. - Div.'
adm_2_adm_3_mp$Cat <- 'Dist. - Div.'
adm_1_adm_2_sim$Cat <- 'Prov. - Dist.'
adm_1_adm_3_sim$Cat <- 'Prov. - Div.'
adm_2_adm_3_sim$Cat <- 'Dist. - Div.'

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
# Simulated
adm_1_adm_2_sim_plot <- adm_1_adm_2_sim %>% select(intro_diff, mag_diff,
                                                   adm_2_pop, Cat) %>%
  dplyr::rename('pop' = 'adm_2_pop')
adm_1_adm_3_sim_plot <- adm_1_adm_3_sim %>% select(intro_diff, mag_diff,
                                                   adm_3_pop, Cat) %>%
  dplyr::rename('pop' = 'adm_3_pop')
adm_2_adm_3_sim_plot <- adm_2_adm_3_sim %>% select(intro_diff, mag_diff,
                                                   adm_3_pop, Cat) %>%
  dplyr::rename('pop' = 'adm_3_pop')

# Combine
all_comboa_mp <- rbind(adm_1_adm_2_mp_plot, adm_1_adm_3_mp_plot, adm_2_adm_3_mp_plot)
all_comboa_sim <- rbind(adm_1_adm_2_sim_plot, adm_1_adm_3_sim_plot, adm_2_adm_3_sim_plot)

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


loc_intro <- ggplot(all_comboa_mp, aes(x = log(pop), y = intro_diff)) + 
  geom_point(aes(color = Cat), alpha = 0.35, size = 4) + 
  theme_minimal() +
  geom_hline(yintercept=0, lty=2, linewidth = 2) +  # add a dotted line at x=1 after flip
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Spatial Invasion Difference (days)') + xlab('Log Population Size') + 
  #scale_color_distiller('Log(Population)\n', palette = "PuRd", direction = 1) + theme_plot +
  scale_y_continuous(breaks = seq(-40, 40, 20), limits = c(-45, 45)) + theme_plot + theme(legend.position = 'bottom') +
  scale_color_manual(name = '', values = colors_time) +
  scale_x_continuous(breaks = seq(6, 14, 2), limits = c(6, 15))


adm_3_phone_mobility_mat_0 <- adm_3_phone_mobility_mat

is.na(adm_3_phone_mobility_mat_0) <- 0
adm_3_phone_mobility_mat_0[is.na(adm_3_phone_mobility_mat_0)] <- 0

adm_3_adm_1_phone_sort <- adm_3_adm_1_phone |>
  arrange(origin)

test <-  adm_3_phone_mobility_mat_0 %*% adm_3_adm_1_phone_sort$diff

test <- as.data.frame(test)
test$V2 <- adm_1_adm_3_mp_plot$intro_diff
test$V3 <- adm_3_adm_1_phone_sort$diff
test$V4 <- adm_3_adm_1_phone_sort$population_2020_adm_3
test$V5 <- adm_3_adm_1_phone_sort$adm_1_value

ggplot(test) + geom_point(aes(x = V1, y = V2)) + geom_smooth(aes(x = V1, y = V2), method = 'lm')

loc_mag <- ggplot(all_comboa_mp, aes(x = log(pop), y = mag_diff)) + 
  geom_point(aes(color = Cat), alpha = 0.3, size = 4) + theme_minimal() +
  geom_hline(yintercept=0, lty=2, linewidth = 2) +  # add a dotted line at x=1 after flip
  geom_smooth(aes(color = Cat), method='loess', se = F, linewidth = 3, alpha = 0.8) +
  ylab('Epidemic Magnitude Difference (cases)') + xlab('') + 
  scale_color_manual(name = '', values = colors_mag) + theme_plot + theme(legend.position = 'bottom') +
  scale_y_continuous(breaks = seq(-6000000, 6000000,3000000), limits = c(-7000000, 7000000)) +
  scale_x_continuous(breaks = seq(6, 14, 2), limits = c(6, 15))

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
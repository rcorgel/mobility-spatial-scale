

library(ggplot2)
library(usmap) 
library(tidyverse)

statepop <- statepop %>% 
  mutate(pop_2015 = ifelse(abbr == "NY" | abbr == "MD" | abbr == "DC", 1, 0))
main <- plot_usmap("states", data = statepop, 
           include = c("NY", "MD", "DC", "PA", 
                       "NJ", "DE"), 
           values = "pop_2015", linewidth = 0.5) +
  ggplot2::scale_fill_continuous(low = "white", 
                                 high = "#6BAED6", 
                                 guide = FALSE)

ggsave('./figs/map_1.png', plot = main, height = 20, width = 15)

library(cowplot)

plot_1 <- plot_usmap("states", include = c("MD", "PA"), linewidth = 0.5)
plot_2 <- plot_usmap("counties", include = c("MD", "PA"), linewidth = 0.5)
map_2 <- plot_grid(plot_1, plot_2, nrow = 1)
ggsave('./figs/plot_1.png', plot = plot_1, height = 12, width = 24)

adm_2 <- run_sir_model_multi(n = 10, method = 'average',
                             R_0 = 2, gamma = 1/7, prop_s = 0.80, 
                             adm_name_vec = adm_2_name_vec, adm_level = '2', 
                             pop_vec = adm_2_pop_vec, intro_adm = 'largest',
                             adm_x_walk = adm_2_x_walk, travel_mat = adm_2_day_avg_mat, 
                             max_time = 365, time_step = 1)

adm_2_sub <- adm_2 %>% dplyr::filter(adm_2 == 'Colombo' | adm_2 == 'Jaffna')

graph_1 <- ggplot(adm_2) +
  geom_line(aes(x = time, y = incid_I_avg, group = adm_2), color = '#6BAED6') +
  theme_classic() + xlab(" ") + ylab(" ") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_text(size=16),
        legend.text=element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(size = 16))

graph_2 <- ggplot(adm_2_sub) +
  geom_line(aes(x = time, y = incid_I_avg, group = adm_2), color = '#6BAED6') +
  theme_classic() + xlab(" ") + ylab(" ") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_text(size=16),
        legend.text=element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(size = 16))

ggsave('./figs/graph_1.png', plot = graph_1, height = 8, width = 8)
ggsave('./figs/graph_2.png', plot = graph_2, height = 8, width = 8)


# Load mobility data
load('./tmp/mobility_dat.RData')

un_adj <- mobility_dat %>% group_by(date) %>%
  mutate(tot_trips = sum(trips)) %>%
  distinct(date, tot_trips, .keep_all = FALSE)

adj <- mobility_dat %>% group_by(date) %>%
  mutate(tot_trips = sum(trips_adj)) %>%
  distinct(date, tot_trips, .keep_all = FALSE)

plot_1 <- ggplot(un_adj) +
  geom_line(aes(x = date, y = tot_trips), color = '#6BAED6', linewidth = 1) +
  theme_minimal() + xlab("Date") + ylab("Number of Trips") + ylim(0, 50000000)

plot_2 <- ggplot(adj) +
  geom_line(aes(x = date, y = tot_trips), color = '#6BAED6', linewidth = 1) +
  theme_minimal() + xlab("Date") + ylab("Number of Trips") + ylim(0, 50000000)

ggsave('./figs/line_plot_1.png', plot = plot_1, height = 4, width = 6)
ggsave('./figs/line_plot_2.png', plot = plot_2, height = 4, width = 6)


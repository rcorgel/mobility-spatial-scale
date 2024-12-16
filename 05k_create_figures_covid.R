################################################################################
# File Name: 04k_create_covid_figures                                          #
#                                                                              #
# Purpose:   Create COVID-19 figures for the appendix.                         #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create COVID-19 epidemic curves                                #
#            3. Create COVID-19 R_t curves                                     #
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
library(readxl)
library(assertr)
library(zoo)
library(ggplot2)
library(ggpubr)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

######################################
# 2. CREATE COVID-19 EPIDEMIC CURVES #
######################################

# Load covid-19 data
load('./tmp/covid_dat.RData')

# Load admin 2 crosswalk
load('./tmp/adm_2_metapop_dat.RData')

# Merge admin 1 units to covid-19 data
covid_dat_district <- left_join(covid_dat_district, adm_2_x_walk, by = c('district' = 'adm_2'))
# Collapse to province level
covid_dat_province <- covid_dat_district %>% group_by(date, adm_1) %>%
  filter(!is.na(rolling_avg)) %>%
  mutate(adm_1_avg = sum(rolling_avg)) %>%
  distinct(date, adm_1, adm_1_avg)

# Collapse to country level
covid_dat_total <- covid_dat_district %>% group_by(date) %>%
  mutate(total_avg = sum(rolling_avg)) %>%
  filter(!is.na(total_avg)) %>%
  distinct(date, total_avg)

# Save all levels of data
save(list = c('covid_dat_district', 'covid_dat_province', 'covid_dat_total'),
     file = './tmp/covid_data_province_district.RData')

# Create country level epidemic curve
plot_country <- ggplot(covid_dat_total) +
  geom_line(aes(x = as.Date(date), y = total_avg), color = '#4292C6', alpha = 1, linewidth = 1.5) +
  theme_minimal() + xlab("Date") + ylab("Incident Cases") + ylim(0, 3500) +
  scale_x_date(breaks = as.Date(c("2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01")),
               date_labels = "%b") +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank())

# Create province level epidemic curve
covid_dat_province$Province <- covid_dat_province$adm_1
plot_province <- ggplot(covid_dat_province) +
  geom_line(aes(x = as.Date(date), y = adm_1_avg, color = Province), alpha = 0.9, linewidth = 1.5) +
  theme_minimal() + xlab("Date") + ylab("Incident Cases") + ylim(0, 1600) +
  scale_x_date(breaks = as.Date(c("2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01")),
               date_labels = "%b") +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')

# Create district level epidemic curve
covid_dat_district$District <- covid_dat_district$district
covid_dat_district <- covid_dat_district %>% filter(!is.na(rolling_avg))
plot_district <- ggplot(covid_dat_district) +
  geom_line(aes(x = as.Date(date), y = rolling_avg, color = District), alpha = 0.9, linewidth = 1.5) +
  theme_minimal() + xlab("Date") + ylab("Incident Cases") + ylim(0, 700) +
  scale_x_date(breaks = as.Date(c("2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01")),
               date_labels = "%b") +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')

# Create alpha relative frequency plot
# Data from: https://gisaid.org/hcov-19-variants-dashboard/
# Compile data
alpha <- data.frame(date = c("2020-11-20", "2020-12-01", "2021-01-01", "2021-02-01", 
                             "2021-03-01", "2021-04-01", "2021-05-01", "2021-05-29"),
                    alpha_freq = c(0, 0, 0.1089, 0.0211, 0.1667, 0.8421, 0.9158, 0.8374))

# Create plot
plot_alpha <- ggplot(alpha) +
  geom_line(aes(x = as.Date(date), y = alpha_freq), color = '#41AE76', alpha = 1, linewidth = 1.5) +
  theme_minimal() + xlab("Date") + ylab("Relative Frequency") + ylim(0, 1) +
  scale_x_date(breaks = as.Date(c("2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01")),
               date_labels = "%b") +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank())          

# Extract legends
legend_1 <- get_legend(ggplot(covid_dat_district) +
                         geom_line(aes(x = as.Date(date), y = rolling_avg, color = District), alpha = 0.9, linewidth = 5) +
                         theme_minimal() + xlab("Date") + ylab("Incident Cases") + ylim(0, 700) +
                         scale_x_date(breaks = as.Date(c("2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01")),
                                      date_labels = "%b") +
                         theme(axis.title = element_text(size=26),
                               axis.text = element_text(size=22),
                               panel.grid.major.x = element_blank(),
                               legend.text = element_text(size = 22),
                               legend.title = element_text(size = 22),
                               plot.title = element_text(size=30, hjust = 0.5),
                               panel.grid.minor = element_blank(),
                               legend.position = 'bottom'))
legend_2 <- get_legend(ggplot(covid_dat_province) +
                         geom_line(aes(x = as.Date(date), y = adm_1_avg, color = Province), alpha = 0.9, linewidth = 5) +
                         theme_minimal() + xlab("Date") + ylab("Incident Cases") + ylim(0, 1600) +
                         scale_x_date(breaks = as.Date(c("2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01")),
                                      date_labels = "%b") +
                         theme(axis.title = element_text(size=26),
                               axis.text = element_text(size=22),
                               panel.grid.major.x = element_blank(),
                               legend.text = element_text(size = 22),
                               legend.title = element_text(size = 22),
                               plot.title = element_text(size=30, hjust = 0.5),
                               panel.grid.minor = element_blank(),
                               legend.position = 'bottom'))
# Create figure
fig <- plot_grid(plot_district + ggtitle('COVID-19 Incident Cases, Sri Lanka by District') +
                   theme(legend.position = 'none'), 
                 legend_1,
                 plot_province + ggtitle('COVID-19 Incident Cases, Sri Lanka by Province') +
                   theme(legend.position = 'none'), 
                 legend_2,
                 plot_country + ggtitle('COVID-19 Incident Cases, Sri Lanka'), 
                 plot_alpha + ggtitle('Alpha Variant Relative Frequency, Sri Lanka'), 
                 rel_heights = c(1, 0.3, 1, 0.3, 1, 1),
                 nrow = 6, labels = c('(a)', '', '(b)','', '(c)', '(d)'),
                 label_size = 26, hjust = 0)

# Save figure
ggsave('./figs/figure_A3.jpg', plot = fig, height = 30, width = 25)

#################################
# 2. CREATE COVID-19 R_t CURVES #
#################################

# Serial interval sources:
# For SI sd:
# https://pubmed.ncbi.nlm.nih.gov/32191173/
# For mean SI:
# https://bmcinfectdis.biomedcentral.com/articles/10.1186/s12879-021-05950-x

# Load EpiEstim library
library(EpiEstim)

# Create zoomed in data
covid_dat_district_zoom <- covid_dat_district %>% 
  filter(date > '2021-03-01')
covid_dat_province_zoom <- covid_dat_province %>% 
  filter(date > '2021-03-01') 
covid_dat_total_zoom <- covid_dat_total %>% 
  filter(date > '2021-03-01') 

# Create district only time-series (2 populous, 2 rural)
covid_dat_colombo_zoom <- covid_dat_district_zoom %>% filter(district == 'Colombo')
covid_dat_gampaha_zoom <- covid_dat_district_zoom %>% filter(district == 'Gampaha')
covid_dat_mannar_zoom <- covid_dat_district_zoom %>% filter(district == 'Mannar')
covid_dat_matale_zoom <- covid_dat_district_zoom %>% filter(district == 'Matale')

# Create data set to add all R_t
R_t_dat <- covid_dat_colombo_zoom[, 2]
R_t_dat <- filter(R_t_dat, date > '2021-03-08')

# Total R_t
result_total <- estimate_R(covid_dat_total_zoom$total_avg, method = "parametric_si",
                           config = make_config(list(
                             mean_si = 5.2, std_si = 4.75)))
R_t_dat$`Sri Lanka` <- result_total[["R"]][["Mean(R)"]]

# Colombo R_t
result_colombo <- estimate_R(covid_dat_colombo_zoom$rolling_avg, method = "parametric_si",
                           config = make_config(list(
                             mean_si = 5.2, std_si = 4.75)))
R_t_dat$`Colombo` <- result_colombo[["R"]][["Mean(R)"]]

# Gampaha R_t
result_gampaha <- estimate_R(covid_dat_gampaha_zoom$rolling_avg, method = "parametric_si",
                             config = make_config(list(
                               mean_si = 5.2, std_si = 4.75)))
R_t_dat$`Gampaha` <- result_gampaha[["R"]][["Mean(R)"]]

# Matale R_t
result_matale <- estimate_R(covid_dat_matale_zoom$rolling_avg, method = "parametric_si",
                             config = make_config(list(
                               mean_si = 5.2, std_si = 4.75)))
R_t_dat$`Matale` <- result_matale[["R"]][["Mean(R)"]]

# Mannar R_t
result_mannar <- estimate_R(covid_dat_mannar_zoom$rolling_avg, method = "parametric_si",
                             config = make_config(list(
                               mean_si = 5.2, std_si = 4.75)))
R_t_dat$`Mannar` <- result_mannar[["R"]][["Mean(R)"]]

# Create plots
plot_R_t_country <- ggplot(R_t_dat) +
  geom_line(aes(x = as.Date(date), y = `Sri Lanka`), color = '#4292C6', alpha = 0.9, linewidth = 1.5) +
  theme_minimal() + xlab("Date") + ylab("R_t") + ylim(0, 3) +
  scale_x_date(breaks = as.Date(c("2021-03-15", "2021-04-01", "2021-04-15", "2021-05-01", "2021-05-15")),
               date_labels = "%b %d") + ggtitle('Sri Lanka') +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')


plot_R_t_colombo <- ggplot(R_t_dat) +
  geom_line(aes(x = as.Date(date), y = `Colombo`), color = '#41AE76', alpha = 0.9, linewidth = 1.5) +
  theme_minimal() + xlab("Date") + ylab("R_t") + ylim(0, 3) +
  scale_x_date(breaks = as.Date(c("2021-03-15", "2021-04-01", "2021-04-15", "2021-05-01", "2021-05-15")),
               date_labels = "%b %d") + ggtitle('Colombo District') +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')


plot_R_t_gampaha <- ggplot(R_t_dat) +
  geom_line(aes(x = as.Date(date), y = `Gampaha`), color = '#F16913', alpha = 0.9, linewidth = 1.5) +
  theme_minimal() + xlab("Date") + ylab("R_t") + ylim(0, 3) +
  scale_x_date(breaks = as.Date(c("2021-03-15", "2021-04-01", "2021-04-15", "2021-05-01", "2021-05-15")),
               date_labels = "%b %d") + ggtitle('Gampaha District') +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')

plot_R_t_mannar <- ggplot(R_t_dat) +
  geom_line(aes(x = as.Date(date), y = `Mannar`), color = '#807DBA', alpha = 0.9, linewidth = 1.5) +
  theme_minimal() + xlab("Date") + ylab("R_t") + ylim(0, 3) +
  scale_x_date(breaks = as.Date(c("2021-03-15", "2021-04-01", "2021-04-15", "2021-05-01", "2021-05-15")),
               date_labels = "%b %d") + ggtitle('Mannar District') +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')

plot_R_t_matale <- ggplot(R_t_dat) +
  geom_line(aes(x = as.Date(date), y = `Matale`), color = '#DF65B0', alpha = 0.9, linewidth = 1.5) +
  theme_minimal() + xlab("Date") + ylab("R_t") + ylim(0, 3.5) +
  scale_x_date(breaks = as.Date(c("2021-03-15", "2021-04-01", "2021-04-15", "2021-05-01", "2021-05-15")),
               date_labels = "%b %d") + ggtitle('Matale District') +
  theme(axis.title = element_text(size=26),
        axis.text = element_text(size=22),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.title = element_text(size=30, hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')

# Create figure
fig <- plot_grid(plot_R_t_country,
                 plot_R_t_colombo,
                 plot_R_t_gampaha,
                 plot_R_t_mannar,
                 plot_R_t_matale,
                 nrow = 5, labels = c('(a)', '(b)','(c)', '(d)', '(e)'),
                 label_size = 26, hjust = 0)

# Save figure
ggsave('./figs/figure_A4.jpg', plot = fig, height = 30, width = 25)

################################################################################
################################################################################

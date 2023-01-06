
####################
# 1. SET-UP SCRIPT #
####################

# Start with a clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(lubridate)
library(geosphere)
library(sp)
library(raster)
library(sf)
library(readxl)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')


load('./tmp/adm_3_population_dat.RData')
load('./tmp/adm_2_population_dat.RData')
load('./tmp/phone_mobility_dat.RData')

adm_3_stays <- mobility_dat %>%
  filter(adm_3_origin == adm_3_destination) %>%
  group_by(adm_3_origin, adm_3_destination) %>%
  mutate(sum_stays = sum(trips)) %>%
  distinct(adm_3_origin, adm_3_destination, sum_stays, adm_2_origin, .keep_all = FALSE)

adm_3_outgoing <- mobility_dat %>%
  group_by(adm_3_origin) %>%
  mutate(tot_outgoing = sum(trips)) %>%
  distinct(adm_3_origin, tot_outgoing, .keep_all = FALSE)

adm_3 <- left_join(adm_3_stays, adm_3_outgoing, by = c('adm_3_origin'))
adm_3$perc_stays = adm_3$sum_stays /adm_3$tot_outgoing
adm_3 <- left_join(adm_3, adm_3_population_dat, by = c('adm_3_origin' = 'adm_3_mobility'))

adm_2_stays <- mobility_dat %>%
  filter(adm_2_origin == adm_2_destination) %>%
  group_by(adm_2_origin, adm_2_destination) %>%
  mutate(sum_stays = sum(trips)) %>%
  distinct(adm_2_origin, adm_2_destination, adm_1_origin, sum_stays, .keep_all = FALSE)

adm_2_outgoing <- mobility_dat %>%
  group_by(adm_2_origin) %>%
  mutate(tot_outgoing = sum(trips)) %>%
  distinct(adm_2_origin, tot_outgoing, .keep_all = FALSE)
 
adm_2 <- left_join(adm_2_stays, adm_2_outgoing, by = c('adm_2_origin'))
adm_2$perc_stays = adm_2$sum_stays /adm_2$tot_outgoing
adm_2 <- left_join(adm_2, adm_2_population_dat, by = c('adm_2_origin' = 'adm_2'))


adm_1_stays <- mobility_dat %>%
  filter(adm_1_origin == adm_1_destination) %>%
  group_by(adm_1_origin, adm_1_destination) %>%
  mutate(sum_stays = sum(trips)) %>%
  distinct(adm_1_origin, adm_1_destination, sum_stays, .keep_all = FALSE)

adm_1_outgoing <- mobility_dat %>%
  group_by(adm_1_origin) %>%
  mutate(tot_outgoing = sum(trips)) %>%
  distinct(adm_1_origin, tot_outgoing, .keep_all = FALSE)

adm_1 <- left_join(adm_1_stays, adm_1_outgoing, by = c('adm_1_origin'))
adm_1$perc_stays = adm_1$sum_stays /adm_1$tot_outgoing

adm_3_merge <- left_join(adm_3, adm_2, by = c('adm_2_origin'))
adm_3_merge$perc_stay_diff <- adm_3_merge$perc_stays.y - adm_3_merge$perc_stays.x

adm_2_merge <- left_join(adm_2, adm_1, by = c('adm_1_origin'))
adm_2_merge$perc_stay_diff <- adm_2_merge$perc_stays.y - adm_2_merge$perc_stays.x



ggplot(adm_3_merge, aes(x=perc_stay_diff, y=population_2020_adm_3, color = adm_2_origin)) + geom_point()
ggplot(adm_2_merge, aes(x=perc_stay_diff, y=population_2020_adm_2, color = adm_1_origin)) + geom_point()





adm_2_top_mob <- mobility_dat %>%
  group_by(adm_2_origin, adm_2_destination) %>%
  mutate(tot = sum(trips_adj)) %>%
  dplyr::distinct(adm_2_origin, adm_2_destination, tot, .keep_all = FALSE) %>%
  ungroup() %>%
  mutate(stay = ifelse(adm_2_origin == adm_2_destination, 1, 0),
         trip_tot = sum(tot)) %>%
  group_by(stay) %>%
  mutate(stay_perc = sum(tot)/trip_tot)


adm_3_trips <- mobility_dat %>%
  group_by(date, adm_3_origin) %>%
  mutate(sum_trips = sum(trips)) %>%
  distinct(date, adm_3_origin, sum_trips, adm_2_origin, adm_1_origin, .keep_all = FALSE)


# trips as a % of pop
adm_3_trips <- phone_mobility_dat %>%
  group_by(date, adm_3_origin) %>%
  mutate(sum_trips = sum(trips_adj)) %>%
  distinct(date, adm_3_origin, sum_trips, adm_2_origin, adm_1_origin, .keep_all = FALSE)

adm_3_trips <- left_join(adm_3_trips, adm_3_population_dat, by = c('adm_3_origin' = 'adm_3_mobility'))

adm_3_trips$ratio <- adm_3_trips$sum_trips / adm_3_trips$population_2020_adm_3

ggplot(adm_3_trips, aes(x=date, y=ratio, color = adm_3_origin)) + geom_line() + theme(legend.position = "none")

# trips as a % of pop
adm_2_trips <- phone_mobility_dat %>%
  group_by(date, adm_2_origin) %>%
  mutate(sum_trips = sum(trips_adj)) %>%
  distinct(date, adm_2_origin, sum_trips, adm_1_origin, .keep_all = FALSE)

adm_2_trips <- left_join(adm_2_trips, adm_2_population_dat, by = c('adm_2_origin' = 'adm_2'))

adm_2_trips$ratio <- adm_2_trips$sum_trips / adm_2_trips$population_2020_adm_2

ggplot(adm_2_trips, aes(x=date, y=ratio, color = adm_2_origin)) + geom_line() 

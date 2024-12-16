################################################################################
# File Name: 01e_process_covid_data                                            #
#                                                                              #
# Purpose:   Load and format COVID-19 data used in analyses. Note: no longer   #
#            used in analysis.                                                 #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Load and format COVID-19 data                                  #
#            3. Convert health districts to districts                          #
#            4. Calculate incidence                                            #
#            5. Save data                                                      #
#            6. Estimate April 9 infectious cases                              #
#            7. Estimate COVID-19 alpha introductions                          #  
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

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project')

####################################
# 2. LOAD AND FORMAT COVID-19 DATA #
####################################

# Load COVID-19 data
covid_dat <- read_excel('./raw/sri_lanka_covid_data.xlsx', col_types = c('text', 'text', 'date'))

# Drop NAs
covid_dat <- covid_dat |> filter(!is.na(health_district))

# Clean up health_district variable
covid_dat$health_district <- str_to_title(str_trim(covid_dat$health_district))

# Change cases to numerical
covid_dat$cumulative_cases <- str_trim(covid_dat$cumulative_cases)
covid_dat$cumulative_cases <- as.numeric(covid_dat$cumulative_cases)

############################################
# 3. CONVERT HEALTH DISTRICTS TO DISTRICTS #
############################################

# 26 health districts, but only 25 districts
# Kalmunai reports separatly from Ampara, combine into one
covid_dat$district <- ifelse(covid_dat$health_district == 'Kalmunai', 'Ampara',
                             covid_dat$health_district)

# Collapse data to district level
covid_dat_district <- covid_dat |>
  group_by(district, date) |>
  mutate(cases = sum(cumulative_cases)) |>
  distinct(district, date, cases, .keep_all = FALSE) |>
  ungroup()

##########################
# 4. CALCULATE INCIDENCE #
##########################

# Calculate incidence and rolling average
covid_dat_district <- covid_dat_district |>
  group_by(district) |>
  arrange(date, .by_group = TRUE) |>
  mutate(incident_cases = cases - lag(cases, default = first(cases))) |>
  mutate(rolling_avg = rollmean(incident_cases, k = 7, fill = NA, align = 'right')) |>
  ungroup()

# Confirm that all incident cases are >= 0
verify(covid_dat_district, incident_cases >= 0)

################
# 5. SAVE DATA #
################

# Rename districts
covid_dat_district$district <- ifelse(covid_dat_district$district == 'Vavunia', 'Vavuniya', 
                                      covid_dat_district$district)
covid_dat_district$district <- ifelse(covid_dat_district$district == 'Nuwaraeliya', 'Nuwara Eliya', 
                                      covid_dat_district$district)
covid_dat_district$district <- ifelse(covid_dat_district$district == 'Mullativu', 'Mullaitivu', 
                                      covid_dat_district$district)
covid_dat_district$district <- ifelse(covid_dat_district$district == 'Moneragala', 'Monaragala', 
                                      covid_dat_district$district)
covid_dat_district$district <- ifelse(covid_dat_district$district == 'Batticoloa', 'Batticaloa', 
                                      covid_dat_district$district)
# Save
saveRDS(covid_dat_district, './tmp/covid_dat.rds')

########################################
# 6. ESTIMATE APRIL 9 INFECTIOUS CASES #
########################################

# Load admin crosswalk
admin_xwalk <- readRDS('./tmp/admin_xwalk.rds')

# Change admin cross walk to admin 2 and 1 levels
admin_xwalk_adm_2 <- admin_xwalk |>
  group_by(adm_2) |>
  distinct(adm_2, adm_1, .keep_all = FALSE)

# Merge on province information
covid_dat_district <- left_join(covid_dat_district, admin_xwalk_adm_2, by = c('district' = 'adm_2'))

# Collapse to province level
covid_dat_province <- covid_dat_district |> group_by(adm_1, date) |>
  mutate(rolling_avg_sum = sum(rolling_avg),
         incident_cases_sum = sum(incident_cases)) |>
  distinct(adm_1, date, rolling_avg_sum, incident_cases_sum, .keep_all = FALSE)

# Restrict data to 9 days before and including April 9
# District
covid_dat_district_restrict <- covid_dat_district |>
  filter(date < '2021-04-10') |>
  filter(date >= '2021-04-01') |> group_by(district) |>
  mutate(sum_incident = sum(incident_cases)) |>
  distinct(district, sum_incident)
# Create matrix
covid_dat_district_restrict_mat <- as.matrix(covid_dat_district_restrict[, c(2)])
rownames(covid_dat_district_restrict_mat) <- covid_dat_district_restrict$district
colnames(covid_dat_district_restrict_mat) <- 1

# Restrict data to 9 days before and including April 9
# Province
covid_dat_province_restrict <- covid_dat_province |> 
  filter(date < '2021-04-10') |>
  filter(date >= '2021-04-01') |> group_by(adm_1) |>
  mutate(sum_incident = sum(incident_cases_sum)) |>
  distinct(adm_1, sum_incident) |> arrange(adm_1)
# Create matrix
covid_dat_province_restrict_mat <- as.matrix(covid_dat_province_restrict[, c(2)])
rownames(covid_dat_province_restrict_mat) <- covid_dat_province_restrict$adm_1
colnames(covid_dat_province_restrict_mat) <- 1

# Divide cases by population for divisional secretariat level (Admin Level 3)
load('./tmp/adm_population_dat.RData')
# Merge cases to population data
adm_3_population_dat <- left_join(adm_3_population_dat, covid_dat_district_restrict, 
                                  by = c('adm_2' = 'district'))
adm_3_population_dat <- left_join(adm_3_population_dat, adm_2_population_dat[, c(1, 3)], 
                                  by = c('adm_2' = 'adm_2'))
# Make cases proportional to population
adm_3_population_dat$cases <- adm_3_population_dat$sum_incident * 
  (adm_3_population_dat$population_2020_adm_3/adm_3_population_dat$population_2020_adm_2)
# Order data alphabetically
adm_3_population_dat <- adm_3_population_dat |> arrange(adm_3_mobility)
# Create matrix
covid_dat_division_restrict_mat <- as.matrix(adm_3_population_dat[, c(7)])
rownames(covid_dat_division_restrict_mat) <- adm_3_population_dat$adm_3_mobility
colnames(covid_dat_division_restrict_mat) <- 1

# Save all matrices
save(list = c('covid_dat_division_restrict_mat', 'covid_dat_province_restrict_mat',
              'covid_dat_district_restrict_mat'), 
     file = './tmp/covid_model_case_inputs.RData')

############################################
# 7. ESTIMATE COVID-19 ALPHA INTRODUCTIONS #
############################################

# Filter district data to April 9 and calculate introduction timings
covid_dat_district <- covid_dat_district |> filter(date > '2021-04-09')
# Introduction = 10 cases
covid_dat_district_intro_10 <- covid_dat_district %>% group_by(district) |>
  mutate(intro_first = rolling_avg > 10 & !duplicated(rolling_avg > 10)) |>
  filter(intro_first == TRUE) |>
  ungroup() |>
  mutate(intro_time = as.Date(date) - as.Date('2021-04-09'),
         intro_time = as.numeric(intro_time))
# Introduction = 5 cases
covid_dat_district_intro_5 <- covid_dat_district |> group_by(district) |>
  mutate(intro_first = rolling_avg > 5 & !duplicated(rolling_avg > 5)) |>
  filter(intro_first == TRUE) |>
  ungroup() |>
  mutate(intro_time = as.Date(date) - as.Date('2021-04-09'),
         intro_time = as.numeric(intro_time))

# Filter province data to April 9 and calculate introduction timings
covid_dat_province <- covid_dat_province |> filter(date > '2021-04-09')
# Introduction = 25 cases
covid_dat_province_intro_25 <- covid_dat_province %>% group_by(adm_1) |>
  mutate(intro_first = rolling_avg_sum > 25 & !duplicated(rolling_avg_sum > 25)) |>
  filter(intro_first == TRUE) |>
  ungroup() |>
  mutate(intro_time = as.Date(date) - as.Date('2021-04-09'),
         intro_time = as.numeric(intro_time))
# Introduction = 50 cases
covid_dat_province_intro_50 <- covid_dat_province |> group_by(adm_1) |>
  mutate(intro_first = rolling_avg_sum > 50 & !duplicated(rolling_avg_sum > 50)) |>
  filter(intro_first == TRUE) |>
  ungroup() |>
  mutate(intro_time = as.Date(date) - as.Date('2021-04-09'),
         intro_time = as.numeric(intro_time))

# Save data
save(list = c('covid_dat_district_intro_5', 'covid_dat_district_intro_10',
              'covid_dat_province_intro_25', 'covid_dat_province_intro_50'), 
     file = './tmp/covid_introductions.RData')

################################################################################
################################################################################

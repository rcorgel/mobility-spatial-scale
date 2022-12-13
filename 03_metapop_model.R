################################################################################
# File Name: 03_metapop_model                                                  #
#                                                                              #
# Purpose:   Prepare data for gravity model.                                   #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
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
library(lubridate)
library(reshape)
library(geosphere)
library(readxl)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')


########
#########
# Include birth and death data?
# http://www.statistics.gov.lk/Population/StaticalInformation/VitalStatistics/NumberofBirthsDeathsMarriagesDistrict2020-2021
#########
########
##################################
# 2. CREATE METAPOPULATION MODEL #
##################################

# Create a discrete time seir function
run_seir_model <- function(params, initial_state_dat, adm_vec, 
                           pop_vec, travel_mat, max_time, time_step) {
  
  # Set the parameters of beta, sigma, gamma, mu, eta, theta, and c
  beta = params['beta']       ## transmission coefficient
  sigma = params['sigma']     ## exposure to infection rate
  gamma = params['gamma']     ## recovery rate
  mu0 = params['mu']          ## birth rate
  eta = params['eta']         ## death rate

  # Set the time vector from the max time and time step 
  times = seq(0, max_time, time_step)
  # Make an object for the number of time steps
  nsteps = length(times)
  
  # Create empty matrices to fill with each camp's SEIR results
  S_mat <- NULL
  I_mat <- NULL
  R_mat <- NULL
  incidI_dat <- NULL
  incidBirths_dat <- NULL
  N_dat <- NULL


  # Loop through all camps and set the initial states
  for (i in 1:n_camps) {
    # Create vectors full of NAs by the number of discrete time points for each state (M, S, E, I, R)
    # Replace the first index with the initial state at time 0
    S <- rep(NA, nsteps); S[1] = initial_state_dat$S[i]
    I <- rep(NA, nsteps); I[1] = initial_state_dat$I[i]
    R <- rep(NA, nsteps); R[1] = initial_state_dat$R[i]
    # Create a vector for incident cases (new infections) at each time step
    # Replace the first index with 0 because no new cases occur at time 0
    incidI <- rep(NA, nsteps); incidI[1] = 0
    # Create a vector for incident births (new births) at each time step
    # Replace the first index with 0 because no new births occur at time 0
    incidBirths <- rep(NA, nsteps); incidBirths[1] = 0
    # Set N as the total population (number of people in all states)
    N = rep(NA, nsteps); N[1] = S[1] + E[1] + I[1] + R[1] + M[1]
    
    # Calculate the carrying capacity (formula from Plowright et. al.)
    K <- N[1] / (1 - (eta/mu0))
    
    # Calculate maternal proportion immune
    maternal_prop_immune <- R[1] / N[1]
    
    # Add initial state data to each vector
    M_dat[[i]] <- M
    S_dat[[i]] <- S
    E_dat[[i]] <- E
    I_dat[[i]] <- I
    R_dat[[i]] <- R
    incidI_dat[[i]] <- incidI
    incidBirths_dat[[i]] <- incidBirths
    N_dat[[i]] <- N
    K_list <- c(K_list, K)
    maternal_prop_list <- c(maternal_prop_list, maternal_prop_immune)
  }
  
  # Factor in distance matrix scaling
  distance_mat <- distance_mat * distance_scale
  
  # Iterate from 2 to the total number of time steps for each camp
  for(ii in 2:nsteps) {
    for (j in 1:n_camps) {
      
      # Based on function specifications, add the introduction event here
      if (intro_time == (ii-1) & intro_camp == j) {
        I_dat[[j]][ii-1] = max(I_dat[[j]][ii-1], 0) + 1
      }
      
      # Calculate the probability of new exposure at each time point
      # Incorporate mixing between all camps, depending on connectivity and distance
      pr_new_exp = 1 - exp(-time_step*beta*sum(
        unlist(Map('*', lapply(I_dat, function(l) l[[ii-1]]), 
                   lapply(distance_mat[j, ],function(l) exp(-c*l[[1]]))))))
      
      # Calculate the probability of a new infection at each time point
      pr_new_inf = 1 - exp(-time_step*sigma)
      # Calculate the probability of a new recovery at each time point
      pr_new_rec = 1 - exp(-time_step*gamma)
      # Calculate the probability of birth
      # If it is between October 1 and December 31 birth rate in non-zero
      if ((ii/365) - floor(ii/365) > 0.7507 & (ii/365) - floor(ii/365) < 1.000) {
        # As N approaches the carrying capacity, mu scales down
        pr_birth = ifelse(1 - exp(-time_step*(mu0*(1+mu1*-cos(2*pi*ii/(365/4))))*
                                    (1 - max(N_dat[[j]][ii-1], 0) / K_list[[j]])) < 0,
                          0, 
                          1 - exp(-time_step*(mu0*(1+mu1*-cos(2*pi*ii/(365/4))))*
                                    (1 - max(N_dat[[j]][ii-1], 0) / K_list[[j]])))
      }
      else {
        pr_birth = 0
      }
      # Calculate the probability of death
      pr_death = 1 - exp(-time_step*eta)
      
      # Calculate the number of newborns moving into the susceptible category
      # This occurs between April 1 and June 31 each year as maternal antibodies wane
      if ((ii/365) - floor(ii/365) > 0.249 & (ii/365) - floor(ii/365) < 0.4986) {
        new_sus = ceiling(max(M_dat[[j]][ii-1], 0) * theta)
      }
      # Outside of this window there should be no one left in the M compartment
      # And the M compartment will build up October - December again
      else {
        new_sus = 0
      }
      
      # Calculate the number of new exposures based on a binomial distribution
      # where the probability of exposure and the number of susceptible people at the 
      # previous time point is used to get a number of new exposures
      new_exp = rbinom(1, max(S_dat[[j]][ii-1], 0), pr_new_exp)
      
      # Calculate the number of new infections based on a binomial distribution
      # where the probability of infection and the number of exposed people at the 
      # previous time point is used to get a number of new infections
      new_inf = rbinom(1, max(E_dat[[j]][ii-1], 0), pr_new_inf)
      
      # Calculate the number of new recoveries based on a binomial distribution
      # where the probability of recovery and the number of infected people at the 
      # previous time point is used to get a number of new recoveries
      new_rec = rbinom(1, max(I_dat[[j]][ii-1], 0), pr_new_rec)
      
      # Calculate the number of new births based on a binomial distribution
      # where the probability of birth and total population at the 
      # previous time point is used to get a number of new births
      new_births = rbinom(1, max(N_dat[[j]][ii-1], 0), pr_birth)
      
      # Calculate the number of new deaths in each compartment based on a binomial 
      # distribution where the probability of death and population in each compartment
      # at the previous time point is used to get a number of new deaths
      new_deaths_M = rbinom(1, max(M_dat[[j]][ii-1], 0), pr_death)
      new_deaths_S = rbinom(1, max(S_dat[[j]][ii-1], 0), pr_death)
      new_deaths_E = rbinom(1, max(E_dat[[j]][ii-1], 0), pr_death)
      new_deaths_I = rbinom(1, max(I_dat[[j]][ii-1], 0), pr_death)
      new_deaths_R = rbinom(1, max(R_dat[[j]][ii-1], 0), pr_death)
      
      # Fill in the states, new cases, and total population at each iteration
      # Factoring in births and deaths
      # Assuming babies are born susceptible and with waning maternal antibodies 
      # deaths occur in each compartment, and the probability of death is the same for each compartment
      M_dat[[j]][ii] = ifelse(max(M_dat[[j]][(ii-1)], 0) - new_sus + 
                                ceiling(new_births*maternal_prop_list[[j]]) - new_deaths_M < 0, 
                              0,
                              max(M_dat[[j]][(ii-1)], 0) - new_sus + 
                                ceiling(new_births*maternal_prop_list[[j]]) - new_deaths_M)
      S_dat[[j]][ii] = ifelse(max(S_dat[[j]][(ii-1)], 0) - new_exp + 
                                floor(new_births*(1 - maternal_prop_list[[j]])) + new_sus - new_deaths_S < 0,  
                              0,
                              max(S_dat[[j]][(ii-1)], 0) - new_exp + 
                                floor(new_births*(1 - maternal_prop_list[[j]])) + new_sus - new_deaths_S)
      E_dat[[j]][ii] = ifelse(max(E_dat[[j]][(ii-1)], 0) + new_exp - new_inf - new_deaths_E < 0,
                              0,
                              max(E_dat[[j]][(ii-1)], 0) + new_exp - new_inf - new_deaths_E)
      I_dat[[j]][ii] = ifelse(max(I_dat[[j]][(ii-1)], 0) + new_inf - new_rec - new_deaths_I < 0,
                              0,
                              max(I_dat[[j]][(ii-1)], 0) + new_inf - new_rec - new_deaths_I)
      R_dat[[j]][ii] = ifelse(max(R_dat[[j]][(ii-1)], 0) + new_rec - new_deaths_R < 0,
                              0,
                              max(R_dat[[j]][(ii-1)], 0) + new_rec - new_deaths_R)
      incidI_dat[[j]][ii] = new_inf
      incidBirths_dat[[j]][ii] = new_births
      N_dat[[j]][ii] = max(S_dat[[j]][ii], 0) + max(I_dat[[j]][ii], 0) + 
        max(R_dat[[j]][ii], 0) + max(E_dat[[j]][ii], 0) + max(M_dat[[j]][ii], 0)
    }
  }
  
  # Create empty output vector to fill with combined data form each camp
  out <- NULL
  # Loop through all camps to bring together each compartments data into a single data frame
  for (i in 1:n_camps) {
    out[[i]] <- data.frame(cbind(times, M = M_dat[[i]], S = S_dat[[i]], 
                                 E = E_dat[[i]], I = I_dat[[i]], R = R_dat[[i]], 
                                 N = N_dat[[i]], incidI = incidI_dat[[i]], 
                                 incidBirths = incidBirths_dat[[i]], 
                                 intro_time = intro_time_var)) 
  }
  
  # Return the vector of all data frames from each camp
  return(out)
} 

test <- as_tibble(diag(adm_1_day_avg_mat))
test$adm_1 <- adm_1_population_dat$adm_1 
test$total <- rowSums(adm_1_day_avg_mat)
test$dist <- rowSums(adm_1_dist_mat)
test$avg_dist <- test$dist / 8
test$perc_stay <- test$value / test$total
test <- left_join(test, adm_1_population_dat, by = c('adm_1'))
test <- left_join(test, adm_1_area, by = c('adm_1'))
plot(test$population_2020_adm_1, test$value)
plot(test$area_sqkm, test$value)
plot(test$avg_dist, test$value)

test <- as_tibble(diag(adm_2_day_avg_mat))
test$adm_2 <- adm_2_population_dat$adm_2  
test$total <- rowSums(adm_2_day_avg_mat)
test$dist <- rowSums(adm_2_dist_mat)
test$avg_dist <- test$dist / 24
test$perc_stay <- test$value / test$total
test <- left_join(test, adm_2_population_dat, by = c('adm_2'))
test <- left_join(test, adm_2_area, by = c('adm_2'))
plot(test$population_2020_adm_2, test$value)
plot(test$area_sqkm, test$value)
plot(test$avg_dist, test$value)


test <- as_tibble(diag(adm_3_day_avg_mat))
test$adm_3_mobility <- adm_3_population_dat$adm_3_mobility  
test$total <- rowSums(adm_3_day_avg_mat)
test$popularity <- colSums(adm_3_day_avg_mat) / rowSums(adm_3_day_avg_mat)
test$dist <- rowSums(adm_3_dist_mat)
test$avg_dist <- test$dist / 329
test$perc_stay <- test$value / test$total
test <- left_join(test, adm_3_population_dat, by = c('adm_3_mobility'))
test <- left_join(test, adm_3_area, by = c('adm_3_mobility'))
plot( test$population_2020_adm_3, test$value)
plot(test$area_sqkm, test$value)
plot(test$avg_dist, test$value)
plot(test$popularity, test$perc_stay)

library(splines)
mod <- glm(value ~ ns(population_2020_adm_2, 3), family = gaussian(link = 'identity'), data = test, )
summary(mod)
mod <- glm(value ~ population_2020_adm_2 + area_sqkm + avg_dist, family = gaussian(link = 'identity'), data = test, )
summary(mod)
glm(family = gaussian())
library(raster)
x <- shapefile('./raw/lka_adm_20220816_shp/lka_admbnda_adm2_slsd_20220816.shp')
crs(x)
x$area_sqkm <- area(x) / 1000000
adm_2_area <- x@data[, c(1, 18)]
adm_2_area$adm_2 <- adm_2_area$ADM2_EN

x <- shapefile('./raw/lka_adm_20220816_shp/lka_admbnda_adm1_slsd_20220816.shp')
crs(x)
x$area_sqkm <- area(x) / 1000000
adm_1_area <- x@data[, c(1, 14)]
adm_1_area$adm_1 <- adm_1_area$ADM1_EN


x <- shapefile('./raw/lka_adm_20220816_shp/lka_admbnda_adm3_slsd_20220816.shp')
crs(x)
x$area_sqkm <- area(x) / 1000000
adm_3_area <- x@data[, c(1, 22)]
adm_3_area$adm_3_mobility <- adm_3_area$ADM3_EN

library(rgeos)
list.nb <- gTouches(x, byid = TRUE, returnDense = FALSE)



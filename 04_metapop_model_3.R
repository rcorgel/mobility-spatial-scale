################################################################################
# File Name: 04_metapop_model                                                  #
#                                                                              #
# Purpose:   Create a metapopulation model for different spatial scales.       #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Create metapopulation model                                    #
#            3. Create multi-run model function                                #
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

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

##################################
# 2. CREATE METAPOPULATION MODEL #
##################################

# Create a discrete time sir function
run_seir_model <- function(j, beta, gamma, sigma, prop_s,  
                          adm_name_vec, adm_level = c('1', '2', '3'), pop_vec,
                          intro_adm = c('All'), intro_num,
                          adm_x_walk, travel_mat, max_time, time_step) {
  
  # Set number of locations
  N <- length(adm_name_vec)
  
  # Set the time vector from the max time and time step 
  times <- seq(1, max_time, time_step)
  # Make an object for the number of time steps
  n_steps <- length(times)
  # Create vector of repeated times for output data
  time_vec <- rep(seq(1, max_time, time_step), each = N)
  
  # Confirm adm_level set correctly
  adm_level <- match.arg(adm_level)
  
  # Convert population vector to round numbers
  pop_vec <- ceiling(pop_vec)
  
  # Create empty matrices to fill with each location's SIR results
  s_mat <- matrix(NA, N, n_steps)
  rownames(s_mat) <- adm_name_vec
  e_mat <- matrix(NA, N, n_steps)
  rownames(e_mat) <- adm_name_vec
  i_mat <- matrix(NA, N, n_steps)
  rownames(i_mat) <- adm_name_vec
  r_mat <- matrix(NA, N, n_steps)
  rownames(r_mat) <- adm_name_vec
  incid_i_mat <- matrix(NA, N, n_steps)
  rownames(incid_i_mat) <- adm_name_vec

  # Set initial states
  s_mat[, 1] <- ceiling(pop_vec*prop_s)          # set susceptible population
  e_mat[, 1] <- pop_vec*0                        # set exposed population
  i_mat[, 1] <- pop_vec*0                        # set infected population
  r_mat[, 1] <- ceiling(pop_vec*(1 - prop_s))    # set number recovered/immune
  incid_i_mat[, 1] <- 0                          # set number of incident infections
  
  # Set introduction information
  intro_adm <- match.arg(intro_adm)
  if (intro_adm == 'All') {
    if (adm_level == '1') {
      # Place an infected individual in the admin unit
      i_mat[intro_num, 1] <- 1
    }
    if (adm_level == '2') {
      # Place an infected individual in the admin unit
      i_mat[intro_num, 1] <- 1
    }
    if (adm_level == '3') {
      # Place an infected individual in the admin unit
      i_mat[intro_num, 1] <- 1
    }
  }
  
  # Set some variables outside of the loop
  travel_mat[is.na(travel_mat)] <- 0
  N_eff <- c(pop_vec %*% travel_mat)
  repeated_N_eff <- replicate(length(N_eff), N_eff)
  repeated_N <- replicate(length(pop_vec), pop_vec)
  mobility_mat_0 <- travel_mat
  diag(mobility_mat_0) <- 0
  mobility_mat_1 <- matrix(1, nrow = N, ncol = N)
  diag(mobility_mat_1) <- 0
  #mixing_mat_0 <- mixing_mat
  #diag(mixing_mat_0) <- 0
  
  # Loop through time steps
  for (i in 2:n_steps) {
    
    # Equation 2 from https://www.medrxiv.org/content/medrxiv/early/2024/03/11/2023.11.22.23298916.full.pdf

    # Create necessary variables
    I_eff <- c(i_mat[, i-1] %*% travel_mat)
    repeated_I <- t(replicate(length(i_mat[, i-1]), i_mat[, i-1]))
    repeated_I_eff <- t(replicate(length(I_eff), I_eff))
      
    # Calculate FOI for people who stay
    lambda_stay <-  (diag(travel_mat)^2 * i_mat[, i-1] / N_eff) +
      (rowSums(diag(travel_mat) * (repeated_I / repeated_N_eff) * t(mobility_mat_0)))
      
    # Calculate FOI for people who leave
    lambda_leave <- rowSums((repeated_I_eff / t(repeated_N_eff)) * mobility_mat_0)
      
    # Sum the FOIs
    lambda <- lambda_stay + lambda_leave
      
    # Calculate probability of exposure
    prob_expose <- 1 - exp(-time_step * beta * lambda)
    
    # Estimate the number of new infections, pulling from a binomial distribution 
    new_expose <- as.vector(s_mat[, i-1]) * as.vector(prob_expose)
    
    # Estimate the probability of infection
    prob_infect <- 1 - exp(-time_step * sigma)
    # Estimate the number of new recoveries, pulling from a binomial distribution 
    new_infect <- as.vector(e_mat[, i-1]) * as.vector(prob_infect)
    
    # Estimate the probability of recovery
    prob_recover <- 1 - exp(-time_step * gamma)
    # Estimate the number of new recoveries, pulling from a binomial distribution 
    new_recover <- as.vector(i_mat[, i-1]) * as.vector(prob_recover)
    
    # Fill in each state accordingly
    # Susceptible
    s_mat[, i] <- s_mat[, i-1] - new_expose
    # Exposures
    e_mat[, i] <- e_mat[, i-1] + new_expose - new_infect
    # Infections
    i_mat[, i] <- i_mat[, i-1] + new_infect - new_recover
    # Incident Infections
    incid_i_mat[, i] <- new_infect
    # Recoveries
    r_mat[, i] <- r_mat[, i-1] + new_recover
  }
  
  # Create single run data set
  # Merge together the matrices, this converts them from wide to long
  combine_mat <- cbind(c(time_vec), c(s_mat), c(e_mat), c(i_mat), c(incid_i_mat), c(r_mat))
  # Convert to data frame
  combine_dat <- as.data.frame(combine_mat)
  # Assign variable names
  names(combine_dat) <- cbind('time', 'S', 'E', 'I', 'incid_I', 'R')
  # Add admin level variable
  combine_dat$adm_level <- adm_level
  # Add district name variable
  combine_dat$adm_name <- c(adm_name_vec)
  # Add run number
  combine_dat$run_num <- j
  
  # Merge on higher level admin information
  if (adm_level == '1') {
    combine_dat <- combine_dat |>
      dplyr::rename('adm_1' = 'adm_name')
  }
  if (adm_level == '2') {
    combine_dat <- combine_dat |>
      dplyr::rename('adm_2' = 'adm_name')
    combine_dat <- left_join(combine_dat, adm_x_walk,  by = c('adm_2' = 'adm_2'))
  }
  if (adm_level == '3') {
    combine_dat <- combine_dat |>
      dplyr::rename('adm_3' = 'adm_name')
    combine_dat <- left_join(combine_dat, adm_x_walk,  by = c('adm_3' = 'adm_3'))
  }
  return(combine_dat)
}

################################################################################
################################################################################

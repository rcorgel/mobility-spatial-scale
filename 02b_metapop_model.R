################################################################################
# File Name: 02b_metapop_model                                                 #
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
setwd('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution-project')

##################################
# 2. CREATE METAPOPULATION MODEL #
##################################

# Create a discrete time sir function
run_sir_model <- function(density_dep , R_0, gamma, prop_s,  
                          adm_name_vec, adm_level = c('1', '2', '3'), pop_vec,
                          intro_adm = c('Colombo', 'Gampaha', 'Jaffna', 'Hambantota'),
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
  
  # Set density divisor and beta coefficient
  # If model is density dependent, then divisor = 1 and beta = (R_0 * gamma) / N
  # If model is frequency dependent, then divisor = N and beta = R_0 * gamma
  if (density_dep == TRUE) {
    divisor <- 1
    # Mean admin population is used to calculate beta
    beta <- (R_0 * gamma) / mean(pop_vec)
  }
  if (density_dep == FALSE) {
    divisor <- pop_vec
    beta <- R_0 * gamma
  }
  
  # Create empty matrices to fill with each location's SIR results
  s_mat <- matrix(NA, N, n_steps)
  rownames(s_mat) <- adm_name_vec
  i_mat <- matrix(NA, N, n_steps)
  rownames(i_mat) <- adm_name_vec
  r_mat <- matrix(NA, N, n_steps)
  rownames(r_mat) <- adm_name_vec
  incid_i_mat <- matrix(NA, N, n_steps)
  rownames(incid_i_mat) <- adm_name_vec

  # Set initial states
  s_mat[, 1] <- ceiling(pop_vec*prop_s)          # set susceptible population
  i_mat[, 1] <- pop_vec*0                        # set no infected population
  r_mat[, 1] <- ceiling(pop_vec*(1 - prop_s))    # set number recovered/immune
  incid_i_mat[, 1] <- 0                          # set number of incident infections
  
  # Set introduction information
  intro_adm <- match.arg(intro_adm)
  if (intro_adm == 'Colombo') {
    if (adm_level == '1') {
      # Place an infected individual in the admin unit
      i_mat['Western', 1] <- 1
    }
    if (adm_level == '2') {
      # Place an infected individual in the admin unit
      i_mat['Colombo', 1] <- 1
    }
    if (adm_level == '3') {
      # Place an infected individual in the admin unit
      i_mat['Colombo', 1] <- 1
    }
  }
  if (intro_adm == 'Gampaha') {
    if (adm_level == '1') {
      # Place an infected individual in the admin unit
      i_mat['Western', 1] <- 1
    }
    if (adm_level == '2') {
      # Place an infected individual in the admin unit
      i_mat['Gampaha', 1] <- 1
    }
    if (adm_level == '3') {
      # Place an infected individual in the admin unit
      i_mat['Katana', 1] <- 1
    }
  }
  if (intro_adm == 'Jaffna') {
    if (adm_level == '1') {
      # Place an infected individual in the admin unit
      i_mat['Northern', 1] <- 1
    }
    if (adm_level == '2') {
      # Place an infected individual in the admin unit
      i_mat['Jaffna', 1] <- 1
    }
    if (adm_level == '3') {
      # Place an infected individual in the admin unit
      i_mat['Valikamam North (Thllippalai)', 1] <- 1
    }
  }
  if (intro_adm == 'Hambantota') {
    if (adm_level == '1') {
      # Place an infected individual in the admin unit
      i_mat['Southern', 1] <- 1
    }
    if (adm_level == '2') {
      # Place an infected individual in the admin unit
      i_mat['Hambantota', 1] <- 1
    }
    if (adm_level == '3') {
      # Place an infected individual in the admin unit
      i_mat['Hambantota', 1] <- 1
    }
  }
  
  # Loop through time steps
  for (i in 2:n_steps) {
    # Stochastic element of travel, pulling from a poisson distribution
    travel_mat_pois <- matrix(rpois(length(travel_mat), travel_mat), 
                              nrow = nrow(travel_mat))
    # Convert matrix to percentages
    travel_mat_pois_perc <- travel_mat_pois / rowSums(travel_mat_pois)
    
    # Estimate probability of infection
    prob_infect <- 1 - exp(-time_step * beta * (travel_mat_pois_perc %*% (i_mat[, i-1] / divisor)))
    # Estimate the number of new infections, pulling from a binomial distribution 
    new_infect <- rbinom(N, as.vector(s_mat[, i-1]), as.vector(prob_infect))
    
    # Estimate the probability of recovery
    prob_recover <- 1 - exp(-time_step * gamma)
    # Estimate the number of new recoveries, pulling from a binomial distribution 
    new_recover <- rbinom(N, as.vector(i_mat[, i-1]), as.vector(prob_recover))
    
    # Fill in each state accordingly
    # Susceptible
    s_mat[, i] <- s_mat[, i-1] - new_infect
    # Infections
    i_mat[, i] <- i_mat[, i-1] + new_infect - new_recover
    # Incident Infections
    incid_i_mat[, i] <- new_infect
    # Recoveries
    r_mat[, i] <- r_mat[, i-1] + new_recover
  }
  
  # Create single run data set
  # Merge together the matrices, this converts them from wide to long
  combine_mat <- cbind(c(time_vec), c(s_mat), c(i_mat), c(incid_i_mat), c(r_mat))
  # Convert to data frame
  combine_dat <- as.data.frame(combine_mat)
  # Assign variable names
  names(combine_dat) <- cbind('time', 'S', 'I', 'incid_I', 'R')
  # Add admin level variable
  combine_dat$adm_level <- adm_level
  # Add district name variable
  combine_dat$adm_name <- c(adm_name_vec)
  # Merge on higher level admin information
  if (adm_level == '1') {
    combine_dat <- combine_dat %>%
      dplyr::rename('adm_1' = 'adm_name')
  }
  if (adm_level == '2') {
    combine_dat <- combine_dat %>%
      dplyr::rename('adm_2' = 'adm_name')
    combine_dat <- left_join(combine_dat, adm_x_walk,  by = c('adm_2' = 'adm_2'))
  }
  if (adm_level == '3') {
    combine_dat <- combine_dat %>%
      dplyr::rename('adm_3' = 'adm_name')
    combine_dat <- left_join(combine_dat, adm_x_walk,  by = c('adm_3' = 'adm_3'))
  }
  return(combine_dat)
}

######################################
# 2. CREATE MULTI-RUN MODEL FUNCTION #
######################################

run_sir_model_multi <- function(n, density_dep, method = c('average', 'append'),
                                R_0, gamma, prop_s, adm_name_vec, 
                                adm_level = c('1', '2', '3'), pop_vec,
                                intro_adm = c('Colombo', 'Gampaha', 'Jaffna', 'Hambantota'),
                                adm_x_walk, travel_mat, max_time, time_step) {
  
  # Create empty object to append to
  multi_run <- NA
  
  # Loop through multiple runs of the SIR model
  for (i in 1:n) {
    # Run SIR model
    single_run <- run_sir_model(density_dep = density_dep, R_0 = R_0, gamma = gamma, prop_s = prop_s, 
                                adm_name_vec = adm_name_vec, adm_level = adm_level, 
                                pop_vec = pop_vec, adm_x_walk = adm_x_walk,
                                intro_adm = intro_adm, travel_mat = travel_mat, 
                                max_time = max_time, time_step = time_step)
    
    # Add run number variable to output
    single_run$run_num <- i
    
    # Append runs together
    multi_run <- rbind(multi_run, single_run)
  }
  
  # Confirm method set correctly
  method <- match.arg(method)
  
  # Separate out different methods
  # If append, keep as is
  if (method == 'append') {
    multi_run <- multi_run %>%
      dplyr::filter(!is.na(run_num))
  }
  # If average, average accross time periods and admin units
  # Different by admin level
  if (method == 'average') {
    if (adm_level == '1') {
      multi_run <- multi_run %>%
        dplyr::filter(!is.na(run_num)) %>%
        group_by(time, adm_1) %>%
        mutate(S_avg = mean(S), 
               I_avg = mean(I),
               incid_I_avg = mean(incid_I),
               R_avg = mean(R)) %>%
        distinct(time, adm_1, S_avg, I_avg, incid_I_avg, R_avg, adm_level, 
                 .keep_all = FALSE)
    }
    if (adm_level == '2') {
      multi_run <- multi_run %>%
        dplyr::filter(!is.na(run_num)) %>%
        group_by(time, adm_2) %>%
        mutate(S_avg = mean(S), 
               I_avg = mean(I),
               incid_I_avg = mean(incid_I),
               R_avg = mean(R)) %>%
        distinct(time, adm_2, S_avg, I_avg, incid_I_avg, R_avg, adm_level, 
                 adm_1, .keep_all = FALSE)
    }
    if (adm_level == '3') {
      multi_run <- multi_run %>%
        dplyr::filter(!is.na(run_num)) %>%
        group_by(time, adm_3) %>%
        mutate(S_avg = mean(S), 
               I_avg = mean(I),
               incid_I_avg = mean(incid_I),
               R_avg = mean(R)) %>%
        distinct(time, adm_3, S_avg, I_avg, incid_I_avg, R_avg, adm_level,
                 adm_2, adm_1, .keep_all = FALSE)
    }
  }
  # Return output data
  return(multi_run)
}

################################################################################
################################################################################

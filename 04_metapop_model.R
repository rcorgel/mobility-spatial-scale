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
run_seir_model <- function(j, density_dep , R_0, gamma, sigma, prop_s,  
                          adm_name_vec, adm_level = c('1', '2', '3'), pop_vec,
                          intro_adm = c('Colombo', 'Madhu', 'Random', 'All'), intro_num,
                          adm_x_walk, travel_mat, max_time, time_step, mobility) {
  
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
    beta <- (R_0 * (gamma)) / mean(pop_vec)
  }
  if (density_dep == FALSE) {
    divisor <- pop_vec
    beta <- R_0 * (gamma)
  }
  
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
  if (intro_adm == 'Random') {
    if (adm_level == '1') {
      # Place an infected individual in the admin unit
      i_mat[sample(1:9, 1), 1] <- 1
    }
    if (adm_level == '2') {
      # Place an infected individual in the admin unit
      i_mat[sample(1:25, 1), 1] <- 1
    }
    if (adm_level == '3') {
      # Place an infected individual in the admin unit
      i_mat[sample(1:330, 1), 1] <- 1
    }
  }
  if (intro_adm == 'Madhu') {
    if (adm_level == '1') {
      # Place an infected individual in the admin unit
      i_mat['Northern', 1] <- 1
    }
    if (adm_level == '2') {
      # Place an infected individual in the admin unit
      i_mat['Mannar', 1] <- 1
    }
    if (adm_level == '3') {
      # Place an infected individual in the admin unit
      i_mat['Madhu', 1] <- 1
    }
  }
  
  # Loop through time steps
  for (i in 2:n_steps) {
    # Replace NAs with 0 in travel matrix
    travel_mat[is.na(travel_mat)] <- 0
    # Create population matrix for travel stochastic element so trips are estimated based on population
    #pop_mat <- matrix(ceiling(pop_vec), nrow = length(pop_vec), ncol = length(pop_vec))
    # Stochastic element of  travel, pulling from a binomial distribution
    #travel_mat_binom <- matrix(rbinom(length(travel_mat), pop_mat, travel_mat), 
                               #nrow = nrow(travel_mat))
    travel_mat_binom <- travel_mat
    # There should not be NAs, but replace just in case
    travel_mat_binom[is.na(travel_mat_binom)] <- 0
    # Normalize new travel matrix since output it no longer proportions
    travel_mat_binom_norm <- travel_mat_binom / rowSums(travel_mat_binom)
    
    # Create inputs for force of infection equation
    # Probability of contact between stayers
    p_stay <- diag(travel_mat_binom_norm)^2
    # Repetition of stay probabilities for visitor contact with stayers
    p_stay_rep <- matrix(p_stay, nrow=length(p_stay), ncol=length(p_stay), byrow=FALSE)
    # Probability of visitors
    p_visit <- t(travel_mat_binom_norm)
    diag(p_visit) <- 0 # exclude stayers
    # Probability of returners
    p_return <- travel_mat_binom_norm
    diag(p_return) <- 0 # exclude stayers
    
    # Estimate probability of exposure
    # Equation 2 from https://www.medrxiv.org/content/medrxiv/early/2024/03/11/2023.11.22.23298916.full.pdf
    if (mobility == TRUE) {
      n_eff <- t(travel_mat_binom_norm) %*% divisor
      n_eff_rep <- matrix(n_eff, nrow=length(n_eff), ncol=length(n_eff), byrow=FALSE)
      i_eff <- t(travel_mat_binom_norm) %*% i_mat[, i-1]

      prob_expose <- 1 - exp(-time_step *
                               ((beta * p_stay * (i_mat[, i-1] / n_eff)) +
                                (beta * rowSums((diag(travel_mat_binom_norm) * p_visit) * t((i_mat[, i-1] / t(n_eff_rep))))) +
                                (beta * p_return %*% (i_eff / n_eff))))

      # Old FOI equation
      # # Equation 2 from https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1006600
      # prob_expose <- 1 - exp(-time_step * beta * (travel_mat_binom_norm %*%
      #                                              ((t(travel_mat_binom_norm) %*% (i_mat[, i-1] / divisor)) /
      #                                                ((t(travel_mat_binom_norm) %*% ((s_mat[, i-1] + r_mat[, i-1] + e_mat[, i-1]) / divisor)) +
      #                                                   (t(travel_mat_binom_norm) %*% (i_mat[, i-1] / divisor))))))
    }
    if (mobility == FALSE) {
      prob_expose <- 1 - exp(-time_step * beta * (i_mat[, i-1] / divisor))
    }
    # Estimate the number of new infections, pulling from a binomial distribution 
    new_expose <- rbinom(N, as.vector(s_mat[, i-1]), as.vector(prob_expose))
    
    # Estimate the probability of infection
    prob_infect <- 1 - exp(-time_step * sigma)
    # Estimate the number of new recoveries, pulling from a binomial distribution 
    new_infect <- rbinom(N, as.vector(e_mat[, i-1]), as.vector(prob_infect))
    
    # Estimate the probability of recovery
    prob_recover <- 1 - exp(-time_step * gamma)
    # Estimate the number of new recoveries, pulling from a binomial distribution 
    new_recover <- rbinom(N, as.vector(i_mat[, i-1]), as.vector(prob_recover))
    
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

######################################
# 2. CREATE MULTI-RUN MODEL FUNCTION #
######################################

run_seir_model_multi <- function(n, density_dep, method = c('average', 'append', 'run_sum'),
                                R_0, gamma, sigma, prop_s, adm_name_vec, 
                                adm_level = c('1', '2', '3'), pop_vec,
                                intro_adm = c('Colombo', 'Madhu', 'Random', 'All'), intro_num,
                                adm_x_walk, travel_mat, max_time, time_step, mobility) {
  
  # Loop through multiple runs of the SIR model
  for (i in 1:n) {
    # Run SEIR model
    single_run <- run_seir_model(density_dep = density_dep, R_0 = R_0, gamma = gamma, sigma = sigma, prop_s = prop_s, 
                                adm_name_vec = adm_name_vec, adm_level = adm_level, 
                                pop_vec = pop_vec, adm_x_walk = adm_x_walk,
                                intro_adm = intro_adm, intro_num = intro_num, travel_mat = travel_mat, 
                                max_time = max_time, time_step = time_step, mobility = mobility)
    
    # Create run number variable
    single_run$run_num <- i
    
    # If first run, assign as multi_run
    if (i == 1) {
      multi_run <- single_run
    }
    
    # Add single_run to multi_run
    if (i != 1) {
      # Append runs together
      multi_run <- rbind(multi_run, single_run)
    }
  }
  
  # Confirm method set correctly
  method <- match.arg(method)
  
  # Separate out different methods
  # If append, keep as is
  if (method == 'append') {
  }
  # If run_sum, sum incident infections by run number
  if (method == 'run_sum') {
    multi_run <- multi_run |>
      group_by(run_num) |>
      mutate(incid_I_sum = sum(incid_I)) |>
      distinct(run_num, incid_I_sum, .keep_all = FALSE)
  }
  # If average, average across time periods and admin units
  # Different by admin level
  if (method == 'average') {
    if (adm_level == '1') {
      multi_run <- multi_run |>
        group_by(time, adm_1) |>
        mutate(S_avg = mean(S), 
               E_avg = mean(E),
               I_avg = mean(I),
               incid_I_avg = mean(incid_I),
               R_avg = mean(R)) |>
        distinct(time, adm_1, S_avg, E_avg, I_avg, incid_I_avg, R_avg, adm_level, 
                 .keep_all = FALSE)
    }
    if (adm_level == '2') {
      multi_run <- multi_run |>
        group_by(time, adm_2) |>
        mutate(S_avg = mean(S), 
               E_avg = mean(E),
               I_avg = mean(I),
               incid_I_avg = mean(incid_I),
               R_avg = mean(R)) |>
        distinct(time, adm_2, S_avg, E_avg, I_avg, incid_I_avg, R_avg, adm_level, 
                 adm_1, .keep_all = FALSE)
    }
    if (adm_level == '3') {
      multi_run <- multi_run |>
        group_by(time, adm_3) |>
        mutate(S_avg = mean(S),
               E_avg = mean(E),
               I_avg = mean(I),
               incid_I_avg = mean(incid_I),
               R_avg = mean(R)) |>
        distinct(time, adm_3, S_avg, E_avg, I_avg, incid_I_avg, R_avg, adm_level,
                 adm_2, adm_1, .keep_all = FALSE)
    }
  }
  # Return output data
  return(multi_run)
}

################################################################################
################################################################################

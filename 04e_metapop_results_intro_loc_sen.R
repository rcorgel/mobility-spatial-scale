################################################################################
# File Name: 04e_metapop_results_intro_loc_sens                                #
#                                                                              #
# Purpose:   Examine differences in metapopulation model results between       #
#            different mobility spatial scale across introduction locations.   #
#            Similar to 04a but for the sensitivity analysis where the         #
#            homogeneous mixing assumption is examined.                        #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Run metapopulation model at administrative level 3             #
#            3. Run metapopulation model at administrative level 2             #
#                                                                              #
# Project:   Sri Lanka Spatial Aggregation                                     #
# Author:    Ronan Corgel                                                      #
################################################################################

####################
# 1. SET-UP SCRIPT #
####################

# Load libraries
library(tidyverse)
library(parallel)

# Set the seed
set.seed(12345)

# Set the directory
setwd('/Users/rcorgel/My Drive (rcorgel@gmail.com)/Projects/spatial-resolution-project/')

# Load metapopulation model functions
source('./mobility-spatial-scale/04_metapop_model_2.R')

#########################################################
# 2. RUN METAPOPULATION MODEL AT ADMINISTRATIVE LEVEL 3 #
#########################################################

# First, load metapopulation model data at each scale
load('./tmp/adm_3_metapop_dat.RData')
load('./tmp/adm_2_metapop_dat.RData')
load('./tmp/adm_1_metapop_dat.RData')

# Load rescaled mobility data
load('./tmp/rescale_phone_mobility_dat.RData')

adm_3_phone_mobility_mat_0 <- adm_3_phone_mobility_mat
adm_3_phone_mobility_mat_0[is.na(adm_3_phone_mobility_mat_0)] <- 0
adm_2_phone_mobility_mat_0 <- adm_2_phone_mobility_mat
adm_2_phone_mobility_mat_0[is.na(adm_2_phone_mobility_mat_0)] <- 0
adm_1_phone_mobility_mat_0 <- adm_1_phone_mobility_mat
adm_1_phone_mobility_mat_0[is.na(adm_1_phone_mobility_mat_0)] <- 0

test_1 <- adm_1_phone_mobility_mat_0 * adm_1_pop_vec
test_2 <- adm_2_phone_mobility_mat_0 * adm_2_pop_vec
test_3 <- adm_3_phone_mobility_mat_0 * adm_3_pop_vec

library(igraph)
g_adm3 <- graph_from_adjacency_matrix(test_3, mode = "directed", weighted = TRUE)
g_adm2 <- graph_from_adjacency_matrix(test_2, mode = "directed", weighted = TRUE)
g_adm1 <- graph_from_adjacency_matrix(test_1, mode = "directed", weighted = TRUE)


distances_adm3 <- distances(g_adm3, v = 41, mode = "out")
distances_adm2 <- distances(g_adm2, v = 5, mode = "out")

mean(distances_adm3[is.finite(distances_adm3)])
mean(distances_adm2[is.finite(distances_adm2)])

betweenness_adm3 <- betweenness(g_adm3, normalized = TRUE)
betweenness_adm2 <- betweenness(g_adm2, normalized = TRUE)
betweenness_adm1 <- betweenness(g_adm1, normalized = TRUE)


betweenness_adm3

# Step 1: Transform flows to effective distances
# Higher flow = shorter distance
distance_mat <- -log(test_2 + 1e-10)  # Add small constant to avoid log(0)
# OR
distance_mat <- 1 / (test_2 + 1e-10)  # Inverse

# Step 2: Set diagonal to 0 (no cost to stay in same location)
diag(test_2) <- 0

# Step 3: Create graph with distance weights
g <- graph_from_adjacency_matrix(g_adm2, 
                                 mode = "directed", 
                                 weighted = TRUE)

# Step 4: Calculate betweenness using these distance weights
betweenness_centrality <- eigen_centrality(g_adm3, directed = TRUE, weights = E(g_adm3)$weight)$vector

# Higher betweenness = node lies on many shortest paths
names(betweenness_centrality) <- adm_3_name_vec





# Observed data: loop through all introduction locations and save to object

#adm_3_at_2_mp <- NULL
adm_3_at_1_mp <- NULL

start.time <- Sys.time()

for (i in seq(1, 330, 1)) {
  print(i)
  
  # Run 100 iterations
  adm_3 <- mclapply(1:100, run_seir_model, beta = 0.2142857, gamma = 1/7, sigma = 1/2, prop_s = 0.90,
                    adm_name_vec = adm_3_name_vec, adm_level = '3',
                    pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = i,
                    adm_x_walk = adm_3_x_walk, travel_mat = as.matrix(adm_3_phone_mobility_mat_rescale_adm_1),
                    max_time = 365, time_step = 1)
  
  # Append results together
  adm_3_results <- do.call(rbind, adm_3)
  
  # Drop results list
  remove(adm_3)
  
  # # Convert to ADM 2
  # adm_3_at_2 <- adm_3_results  |> group_by(time, adm_2, run_num) |>
  #   # Calculate incidence at Admin 2
  #   mutate(incid_I_adm_2 = sum(incid_I)) |>
  #   distinct(time, run_num, adm_2, incid_I_adm_2) |> 
  #   ungroup() |>
  #   # Calculate burn out and drop runs that did not take off
  #   group_by(run_num) |>
  #   mutate(run_total = sum(incid_I_adm_2)) |>
  #   ungroup() |>
  #   mutate(burn_out_perc = sum(run_total > 100) / n()) |>
  #   filter(run_total > 100) |>
  #   # Calculate average take-off simulation across runs
  #   group_by(time, adm_2) |>
  #   mutate(avg_incid_I_adm_2 = mean(incid_I_adm_2)) |>
  #   distinct(time, adm_2, avg_incid_I_adm_2, burn_out_perc) |> 
  #   ungroup() |>
  #   # Calculate total epidemic size
  #   mutate(magnitude = sum(avg_incid_I_adm_2)) |>
  #   group_by(adm_2) |>
  #   # Calculate cumulative cases by Admin 2
  #   # Indicate when cumulative cases > 1 for each unit
  #   mutate(cum_I = cumsum(avg_incid_I_adm_2),
  #          intro = ifelse(cum_I > 1, 1, 0)) |>
  #   # Indicate the first instance when cumulative > 1
  #   mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
  #   # Filter to first instance for all Admin 2
  #   filter(intro_first == TRUE) |>
  #   ungroup() |>
  #   arrange(time) |>
  #   slice(1:13) |>
  #   mutate(intro_time = median(time),
  #          intro_loc = i) |>
  #   slice(1) |> dplyr::select(intro_loc, magnitude, intro_time, burn_out_perc) |>
  #   mutate(level = '2')
  
  # Convert to ADM 1
  adm_3_at_1 <- adm_3_results |> group_by(time, adm_1, run_num) |>
    # Calculate incidence at Admin 1
    mutate(incid_I_adm_1 = sum(incid_I)) |>
    distinct(time, run_num, adm_1, incid_I_adm_1) |> 
    ungroup() |>
    # Calculate burn out and drop runs that did not take off
    group_by(run_num) |>
    mutate(run_total = sum(incid_I_adm_1)) |>
    ungroup() |>
    mutate(burn_out_perc = sum(run_total > 100) / n()) |>
    dplyr::filter(run_total > 100) |>
    # Calculate average take-off simulation across runs
    group_by(time, adm_1) |>
    mutate(avg_incid_I_adm_1 = mean(incid_I_adm_1)) |>
    distinct(time, adm_1, avg_incid_I_adm_1, burn_out_perc) |> 
    ungroup() |>
    # Calculate total epidemic size
    mutate(magnitude = sum(avg_incid_I_adm_1)) |>
    group_by(adm_1) |>
    # Calculate cumulative cases by Admin 1
    # Indicate when cumulative cases > 1 for each unit
    mutate(cum_I = cumsum(avg_incid_I_adm_1),
           intro = ifelse(cum_I > 1, 1, 0)) |>
    # Indicate the first instance when cumulative > 1
    mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
    # Filter to first instance for all Admin 1
    dplyr::filter(intro_first == TRUE) |>
    ungroup() |>
    arrange(time) |>
    slice(1:5) |>
    mutate(intro_time = median(time),
           intro_loc = i) |>
    slice(1) |> dplyr::select(intro_loc, magnitude, intro_time, burn_out_perc) |>
    mutate(level = '1')
  
  # Remove results
  remove(adm_3_results)
  
  # Append to data
  #adm_3_at_2_mp <- rbind(adm_3_at_2_mp, adm_3_at_2)
  adm_3_at_1_mp <- rbind(adm_3_at_1_mp, adm_3_at_1)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

saveRDS(adm_3_at_1_mp, './tmp/adm_3_at_1_mp_rescale')










# Create function to run multiple simulations at admin level 3 and aggregate to
# levels 1 and 2, then run in parallel
run_seir_model_multi_adm_3 <- function(intro_num, data) {
  
  # Run model
  adm_3 <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                R_0 = 1.5, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                adm_name_vec = adm_3_name_vec, adm_level = '3', 
                                pop_vec = adm_3_pop_vec, intro_adm = 'All', intro_num = intro_num,
                                adm_x_walk = adm_3_x_walk, travel_mat = data, 
                                max_time = 365, time_step = 1, mobility = TRUE)
  
  # Aggregate to Admin 2
  adm_3_at_2 <- adm_3 |> group_by(time, adm_2, run_num) |>
    # Calculate incidence at Admin 2
    mutate(incid_I_adm_2 = sum(incid_I)) |>
    distinct(time, run_num, adm_2, incid_I_adm_2) |> 
    ungroup() |>
    # Calculate burn out and drop runs that did not take off
    group_by(run_num) |>
    mutate(run_total = sum(incid_I_adm_2)) |>
    ungroup() |>
    mutate(burn_out_perc = sum(run_total > 100) / n()) |>
    filter(run_total > 100) |>
    # Calculate average take-off simulation across runs
    group_by(time, adm_2) |>
    mutate(avg_incid_I_adm_2 = mean(incid_I_adm_2)) |>
    distinct(time, adm_2, avg_incid_I_adm_2, burn_out_perc) |> 
    ungroup() |>
    # Calculate total epidemic size
    mutate(magnitude = sum(avg_incid_I_adm_2)) |>
    group_by(adm_2) |>
    # Calculate cumulative cases by Admin 2
    # Indicate when cumulative cases > 1 for each unit
    mutate(cum_I = cumsum(avg_incid_I_adm_2),
           intro = ifelse(cum_I > 1, 1, 0)) |>
    # Indicate the first instance when cumulative > 1
    mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
    # Filter to first instance for all Admin 2
    filter(intro_first == TRUE) |>
    ungroup() |>
    arrange(time) |>
    slice_tail(n = 24) |>
    mutate(intro_time = mean(time),
           intro_loc = intro_num) |>
    slice(1) |> dplyr::select(intro_loc, magnitude, intro_time, burn_out_perc) |>
    mutate(level = '2')
  
  # Aggregate to Admin 1
  adm_3_at_1 <- adm_3 |> group_by(time, adm_1, run_num) |>
    # Calculate incidence at Admin 1
    mutate(incid_I_adm_1 = sum(incid_I)) |>
    distinct(time, run_num, adm_1, incid_I_adm_1) |> 
    ungroup() |>
    # Calculate burn out and drop runs that did not take off
    group_by(run_num) |>
    mutate(run_total = sum(incid_I_adm_1)) |>
    ungroup() |>
    mutate(burn_out_perc = sum(run_total > 100) / n()) |>
    filter(run_total > 100) |>
    # Calculate average take-off simulation across runs
    group_by(time, adm_1) |>
    mutate(avg_incid_I_adm_1 = mean(incid_I_adm_1)) |>
    distinct(time, adm_1, avg_incid_I_adm_1, burn_out_perc) |> 
    ungroup() |>
    # Calculate total epidemic size
    mutate(magnitude = sum(avg_incid_I_adm_1)) |>
    group_by(adm_1) |>
    # Calculate cumulative cases by Admin 1
    # Indicate when cumulative cases > 1 for each unit
    mutate(cum_I = cumsum(avg_incid_I_adm_1),
           intro = ifelse(cum_I > 1, 1, 0)) |>
    # Indicate the first instance when cumulative > 1
    mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
    # Filter to first instance for all Admin 1
    filter(intro_first == TRUE) |>
    ungroup() |>
    arrange(time) |>
    slice_tail(n = 8) |>
    mutate(intro_time = mean(time),
           intro_loc = intro_num) |>
    slice(1) |> dplyr::select(intro_loc, magnitude, intro_time, burn_out_perc) |>
    mutate(level = '1')
  
  # Return summarized data
  return(list(adm_3_at_2, adm_3_at_1))
}

# Observed data: loop through all introduction locations and save to object (Rescaled to adm 1)
adm_3_mp <- mclapply(1:330, run_seir_model_multi_adm_3, data = as.matrix(adm_3_phone_mobility_mat_rescale_adm_1))
adm_3_mp_df <- bind_rows(adm_3_mp)
adm_3_at_1_mp_sens_1 <- adm_3_mp_df |> filter(level == '1')
adm_3_at_2_mp_sens_1 <- adm_3_mp_df |> filter(level == '2')

# Observed Data: loop through all introduction locations and save to object (Rescaled to adm 2)
adm_3_mp <- mclapply(1:330, run_seir_model_multi_adm_3, data = as.matrix(adm_3_phone_mobility_mat_rescale_adm_2))
adm_3_mp_df <- bind_rows(adm_3_mp)
adm_3_at_1_mp_sens_2 <- adm_3_mp_df |> filter(level == '1')
adm_3_at_2_mp_sens_2 <- adm_3_mp_df |> filter(level == '2')

#########################################################
# 3. RUN METAPOPULATION MODEL AT ADMINISTRATIVE LEVEL 2 #
#########################################################

# Create empty objects to fill
adm_2_at_2_mp_sens_1 <- NULL
adm_2_at_1_mp_sens_1 <- NULL
adm_2_at_2_mp_sens_2 <- NULL
adm_2_at_1_mp_sens_2 <- NULL

# Create object for mobility data
mobility_dat_adm_2 <- list(as.matrix(adm_2_phone_mobility_mat_rescale), adm_2_phone_mobility_mat)

# Run 50 iterations for each introduction location
for (i in seq(1, 2, 1)) {
  for (j in seq(1, 25, 1)) {
    # Run model
    adm_2 <- run_seir_model_multi(n = 100, density_dep = FALSE, method = 'append',
                                  R_0 = 1.5, gamma = 1/7, sigma = 1/2, prop_s = 0.90, 
                                  adm_name_vec = adm_2_name_vec, adm_level = '2', 
                                  pop_vec = adm_2_pop_vec, intro_adm = 'All', intro_num = j,
                                  adm_x_walk = adm_2_x_walk, travel_mat = mobility_dat_adm_2[[i]], 
                                  max_time = 365, time_step = 1, mobility = TRUE)
    
    # Admin 2
    adm_2_at_2 <- adm_2 |>
      # Calculate burn out and drop runs that did not take off
      group_by(run_num) |>
      mutate(run_total = sum(incid_I)) |>
      ungroup() |>
      mutate(burn_out_perc = sum(run_total > 100) / n()) |>
      filter(run_total > 100) |>
      # Calculate average take-off simulation across runs
      group_by(time, adm_2) |>
      mutate(avg_incid_I_adm_2 = mean(incid_I)) |>
      distinct(time, adm_2, avg_incid_I_adm_2, burn_out_perc) |> 
      ungroup() |>
      # Calculate total epidemic size
      mutate(magnitude = sum(avg_incid_I_adm_2)) |>
      group_by(adm_2) |>
      # Calculate cumulative cases by Admin 2
      # Indicate when cumulative cases > 1 for each unit
      mutate(cum_I = cumsum(avg_incid_I_adm_2),
             intro = ifelse(cum_I > 1, 1, 0)) |>
      # Indicate the first instance when cumulative > 1
      mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
      # Filter to first instance for all Admin 2
      filter(intro_first == TRUE) |>
      ungroup() |>
      arrange(time) |>
      slice_tail(n = 24) |>
      mutate(intro_time = mean(time),
             intro_loc = j) |>
      slice(1) |> dplyr::select(intro_loc, magnitude, intro_time, burn_out_perc)
    
    # Aggregate to Admin 1
    adm_2_at_1 <- adm_2 |> group_by(time, adm_1, run_num) |>
      # Calculate incidence at Admin 1
      mutate(incid_I_adm_1 = sum(incid_I)) |>
      distinct(time, run_num, adm_1, incid_I_adm_1) |> 
      ungroup() |>
      # Calculate burn out and drop runs that did not take off
      group_by(run_num) |>
      mutate(run_total = sum(incid_I_adm_1)) |>
      ungroup() |>
      mutate(burn_out_perc = sum(run_total > 100) / n()) |>
      filter(run_total > 100) |>
      # Calculate average take-off simulation across runs
      group_by(time, adm_1) |>
      mutate(avg_incid_I_adm_1 = mean(incid_I_adm_1)) |>
      distinct(time, adm_1, avg_incid_I_adm_1, burn_out_perc) |> 
      ungroup() |>
      # Calculate total epidemic size
      mutate(magnitude = sum(avg_incid_I_adm_1)) |>
      group_by(adm_1) |>
      # Calculate cumulative cases by Admin 1
      # Indicate when cumulative cases > 1 for each unit
      mutate(cum_I = cumsum(avg_incid_I_adm_1),
             intro = ifelse(cum_I > 1, 1, 0)) |>
      # Indicate the first instance when cumulative > 1
      mutate(intro_first = intro == 1 & !duplicated(intro == 1)) |>
      # Filter to first instance for all Admin 1
      filter(intro_first == TRUE) |>
      ungroup() |>
      arrange(time) |>
      slice_tail(n = 8) |>
      mutate(intro_time = mean(time),
             intro_loc = j) |>
      slice(1) |> dplyr::select(intro_loc, magnitude, intro_time, burn_out_perc)
    
    # Fill objects with results
    if (i == 1) {
      adm_2_at_2_mp_sens_1 <- rbind(adm_2_at_2_mp_sens_1, adm_2_at_2)
      adm_2_at_1_mp_sens_1 <- rbind(adm_2_at_1_mp_sens_1, adm_2_at_1)
    }
    if (i == 2) {
      adm_2_at_2_mp_sens_2 <- rbind(adm_2_at_2_mp_sens_2, adm_2_at_2)
      adm_2_at_1_mp_sens_2 <- rbind(adm_2_at_1_mp_sens_2, adm_2_at_1)
    }
  } 
}

# Save results
save(list = c('adm_3_at_1_mp_sens_1',
              'adm_3_at_2_mp_sens_2', 
              'adm_2_at_1_mp_sens_1',
              'adm_2_at_1_mp_sens_2',
              'adm_2_at_2_mp_sens_2',
              'adm_2_at_2_mp_sens_1',
              'adm_3_at_1_mp_sens_2',
              'adm_3_at_2_mp_sens_1'), 
     file = './tmp/introduction_location_model_results_obs_sens.RData')

################################################################################
################################################################################

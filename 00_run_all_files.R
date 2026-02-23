################################################################################
# File Name: 00_run_all_files                                                  #
#                                                                              #
# Purpose:   Run all code files in the repository.                             #
# Steps:                                                                       # 
#            1. Set-up script                                                  #
#            2. Run all files                                                  #
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

####################
# 2. RUN ALL FILES #
####################

# Create a list of all .R files in the repository
file_list <- list.files(path = './mobility-spatial-scale', pattern = '.R$', full.names = TRUE)

# Remove the run file
file_list <- file_list[!(file_list %in% './mobility-spatial-scale/00_run_all_files.R')]

# Loop through each code file and run it
for (file in file_list) {
  print(paste("Running script:", file))
  capture.output(suppressMessages(source(file, echo = FALSE, print.eval = FALSE)))
}
paste("Complete! All files have been run :)")

################################################################################
################################################################################

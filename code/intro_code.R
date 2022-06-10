# Ronan Corgel
# April 12, 2021
# Thesis Project Introduction Script

# Clear environment
rm(list = ls())

# Load libraries
library(ggplot2)
library(tidyverse)
library(mobility)
library(DescTools)
library(coda)
library(rjags)
library(hmob)
library(geosphere)

# Set Seed
set.seed(123)

## Namibia ##

# Load Data
load("/Users/rcorgel/Dropbox/AsymmetricMobility/Data/NAM/NAM_between_day_mobility_updated_joinedAdm2.RData")
load("/Users/rcorgel/Dropbox/AsymmetricMobility/Data/NAM/NAM.ID.link.RData")
load("/Users/rcorgel/Dropbox/AsymmetricMobility/Data/NAM/NAM_adm2Joined_building_urb_binned.RData")

# Reshape data to long format
NAM <- mydata.joined
NAM <- NAM %>%
  pivot_longer(cols = contains("/"),
               names_to = "trip.date",
               values_to = "trips")

# Separate out dates to create formatted dates
dates.sep <- strsplit(as.character(NAM$trip.date), "/", perl=TRUE)          # separate date string
NAM$d <- as.integer(sapply(dates.sep, function(x) x[1]))                    # define day of date
NAM$m <- as.integer(sapply(dates.sep, function(x) x[2]))                    # month of date
NAM$y <- as.integer(sapply(dates.sep, function(x) x[3]))                    # year of date
NAM$trip.date <- as.Date(with(NAM, paste(y, m, d,sep="-")), "%Y-%m-%d")

# Merge in data on coordinates and population, as well as urbanicity
NAM <- left_join(NAM, county.file.ordered[-c(6:8)], by = c("i" = "ID_2"))
NAM <- left_join(NAM, county.file.ordered[-c(6:8)], by = c("j" = "ID_2"))
NAM <- left_join(NAM, urbanicity[c(1:2)], by = c("i" = "ID_2"))
NAM <- left_join(NAM, urbanicity[c(1:2)], by = c("j" = "ID_2"))

# Rename variables
colnames(NAM) <- c("start.adm2.code", "end.adm2.code", "trip.date", "trip.count", "d", "m", "y",
                   "start.adm1.name", "start.adm2.name", "X_start", "Y_start", "pop.start", "start.adm1.code",
                   "end.adm1.name", "end.adm2.name", "X_end", "Y_end", "pop.end", "end.adm1.code", "urb.start", "urb.end")

# Create distance measurment between nodes
NAM$link.distance.km <- distHaversine(matrix(c(NAM$X_start, NAM$Y_start), ncol =2),   # distance in km
                                      matrix(c(NAM$X_end, NAM$Y_end), ncol = 2))/1000

# Subset data to variables of interest and reorder
NAM <- NAM[ , c("start.adm1.name", "start.adm2.name", "start.adm1.code", "start.adm2.code", "end.adm1.name", "end.adm2.name", "end.adm1.code", "end.adm2.code",
                "d", "m", "y", "trip.date", "trip.count", "X_start", "Y_start", "X_end", "Y_end", "pop.start", "urb.start", "pop.end", "urb.end", "link.distance.km")]

# save(NAM, file = "../../Data/NAM/NAM.daily.trips.RData")

NAM.daily.avg <- NAM %>%
  group_by(start.adm1.name, start.adm2.name, start.adm1.code, start.adm2.code, end.adm1.name, end.adm2.name, end.adm1.code, end.adm2.code) %>%
  summarise(trip.count.avg = mean(trip.count, na.rm = T)) %>%
  ungroup()

NAM.daily.avg <- NAM.daily.avg[order(NAM.daily.avg$start.adm2.code, NAM.daily.avg$end.adm2.code),]
# save(NAM.daily.avg, file = "../../Data/NAM/NAM.daily.trips.avg.RData")

NAM.daily.avg.matrix <-  NAM.daily.avg %>% select(start.adm2.code, end.adm2.code, trip.count.avg) %>%
  pivot_wider(names_from = end.adm2.code,
              values_from = trip.count.avg) 
NAM.daily.avg.matrix <- as.matrix(NAM.daily.avg.matrix)
rownames(NAM.daily.avg.matrix) <- NAM.daily.avg.matrix[,1]
NAM.daily.avg.matrix <- NAM.daily.avg.matrix[,-1]
NAM.daily.avg.matrix[is.na(NAM.daily.avg.matrix)] <- 0
NAM.daily.avg.matrix.order <- NAM.daily.avg.matrix[order(as.integer(rownames(NAM.daily.avg.matrix))), order(as.integer(colnames(NAM.daily.avg.matrix)))]
# save(NAM.daily.avg.matrix.order, file = "../../Data/NAM/NAM.daily.avg.matrix.RData")


##########
# SET UP #
##########

# Clear environment
rm(list = ls())

# Load library and set environmental settings
library(rstan)
library(geosphere)
library(reshape)
library(ggmap)
library(tidyverse)
options(mc.cores=2)

################
# INITIAL TEST #
################

# Load data
load('./raw/gravity_mode_data.RData')

# Set the model from stan
model = stan_model('./code/gravity_model.stan')

# Fit the model
fit = sampling(model,
               list(n = n,
                    pop = pop,
                    distance_matrix = distance_mat,
                    trip_counts = trip_counts),
               iter = 2000,
               chains = 2)

#true values are gamma=2, alpha=0.1, beta=0.1
print(fit)

#############
# PREP DATA #
#############

# Load Sri Lanka mobility data
load('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution/raw/sri_lanka_mobility_dat_clean.RData')

# For now just look at November to January
mobility_dat <- mobility_dat %>% filter(month == 2 | month != 3)




# Load Sri Lanka population data (ADM 3)
sri_lanka_pop <- read_csv('/Users/rcorgel/Library/CloudStorage/OneDrive-Personal/Projects/spatial-resolution/raw/admin_3_population.csv')

## ADM 1 ##
# Month
adm1.trip.month <- trip.data.long %>%                                           # calculate number of trips between each origin/destination pair for each month/year
  group_by(start.adm1.code, end.adm1.code, m, y) %>%
  dplyr::mutate(adm1.single.trip.sum = sum(trip.count, na.rm = TRUE)) %>%
  dplyr::distinct(start.adm1.code, end.adm1.code, m, y, .keep_all=TRUE) 

# Calculate average number of trips by month
adm1.trip.month.avg <- adm1.trip.month %>%                                      # calculate average trip proportion each month between origin/destination
  group_by(start.adm1.code, end.adm1.code) %>%
  mutate(adm1.single.trip.avg = ceiling(mean(adm1.single.trip.sum, na.rm = TRUE))) %>%
  mutate(adm1.single.trip.var = var(adm1.single.trip.sum, na.rm = TRUE)) %>%
  distinct(start.adm1.code, end.adm1.code, .keep_all = TRUE)

# Matrix
trip.month.avg.summary <- adm1.trip.month.avg[,c('start.adm1.code', 'end.adm1.code', 'adm1.single.trip.avg')]
# Reshape to wide
M.monthly.avg.adm1 <- reshape::cast(trip.month.avg.summary, start.adm1.code ~ end.adm1.code)            
# Label rows with district numbers
rownames(M.monthly.avg.adm1) <- M.monthly.avg.adm1$start.adm1.code                           
# Get rid of the first column
M.monthly.avg.adm1 <- M.monthly.avg.adm1[ ,-1]
class(M.monthly.avg.adm1) <- 'data.frame'
M.monthly.avg.adm1 <- as.matrix(M.monthly.avg.adm1)
names(dimnames(M.monthly.avg.adm1)) <- c('origin', 'destination')
# Replace NAs with 0
M.monthly.avg.adm1[is.na(M.monthly.avg.adm1)] <- 0
# Replace stays with 0
diag(M.monthly.avg.adm1) <- 0

## ADM 2 ##
# Month
adm2.trip.month <- trip.data.long %>%                                           # calculate number of trips between each origin/destination pair for each month/year
  group_by(start.adm2.code, end.adm2.code, m, y) %>%
  mutate(adm2.single.trip.sum = sum(trip.count, na.rm = TRUE)) %>%
  distinct(start.adm2.code, end.adm2.code, m, y, .keep_all=TRUE) 

# Calculate average number of trips by month
adm2.trip.month.avg <- adm2.trip.month %>%                                      # calculate average trip proportion each month between origin/destination
  group_by(start.adm2.code, end.adm2.code) %>%
  mutate(adm2.single.trip.avg = ceiling(mean(adm2.single.trip.sum, na.rm = TRUE))) %>%
  mutate(adm2.single.trip.var = var(adm2.single.trip.sum, na.rm = TRUE)) %>%
  distinct(start.adm2.code, end.adm2.code, .keep_all = TRUE)

# Matrix
trip.month.avg.summary <- adm2.trip.month.avg[,c('start.adm2.code', 'end.adm2.code', 'adm2.single.trip.avg')]
# Reshape to wide
M.monthly.avg.adm2 <- reshape::cast(trip.month.avg.summary, start.adm2.code ~ end.adm2.code)            
# Label rows with district numbers
rownames(M.monthly.avg.adm2) <- M.monthly.avg.adm2$start.adm2.code                           
# Get rid of the first column
M.monthly.avg.adm2 <- M.monthly.avg.adm2[ ,-1]
class(M.monthly.avg.adm2) <- 'data.frame'
M.monthly.avg.adm2 <- as.matrix(M.monthly.avg.adm2)
names(dimnames(M.monthly.avg.adm2)) <- c('origin', 'destination')
# Replace NAs with 0
M.monthly.avg.adm2[is.na(M.monthly.avg.adm2)] <- 0
# Replace stays with 0
diag(M.monthly.avg.adm2) <- 0

#######################
# PREP INPUTS TO STAN #
#######################

## ADM 2 ##
# Create population vectors
pop.adm2.96 <- county.file.ordered$pop2010ppp
county.file.ordered.exclude <- county.file.ordered[-74,] # Exclude 82 and 84 not in CDRs
county.file.ordered.exclude <- county.file.ordered.exclude[-72,] # Exclude 82 and 84 not in CDRs
pop.adm2.94 <- county.file.ordered.exclude$pop2010ppp

# Assemble distance and calculated trips matrices
# Order data
county.file.ordered <- county.file.ordered[order(county.file.ordered$ID_2),]

# Start
adm2.start <- county.file.ordered[, c('NAME_2', 'X_coord', 'Y_coord', 'ID_2', 'pop2010ppp', 'ID_1')]
colnames(adm2.start) <- c('name_start', 'x_start', 'y_start', 'id_start', 'pop_start', 'id_1_start')
# End
adm2.end <- county.file.ordered[, c('NAME_2', 'X_coord', 'Y_coord', 'ID_2', 'pop2010ppp', 'ID_1')]
colnames(adm2.end) <- c('name_end', 'x_end', 'y_end', 'id_end', 'pop_end', 'id_1_end')

# Create all combinations
adm2 <- as.data.frame(NULL)
for (i in adm2.start$id_start) {
  print(i)
  adm2.start$id_end <- i
  adm2 <- rbind(adm2, left_join(adm2.start, adm2.end, by = c('id_end' = 'id_end')))
}

# Calculate distance (in km)
adm2$dist <- distHaversine(as.matrix(adm2[, c('y_start', 'x_start')]), as.matrix(adm2[, c('y_end', 'x_end')])) * 0.001

# Calculate trips based on gravity model
adm2$sim_trip <- ceiling((adm2$pop_start^0.8 * adm2$pop_end^0.2) / (adm2$dist^0.8))
adm2$sim_trip <- ifelse(is.infinite(adm2$sim_trip), 0, adm2$sim_trip) # replace w/ 0 if infinite
adm2$sim_trip <- as.numeric(adm2$sim_trip) # make sure it is numeric
adm2$sim_trip_pois <- unlist(lapply(adm2$sim_trip, function(x) rpois(1, x))) # calculate from poisson dist

# Make distance matrix
dist <- adm2[, c('id_start', 'id_end', 'dist')]
dist_M.adm2 <- reshape::cast(dist, id_start ~ id_end)            
# Label rows with district numbers
rownames(dist_M.adm2) <- adm2.start$id_start                         
# Get rid of the first column
dist_M.adm2 <- dist_M.adm2[ ,-1]
class(dist_M.adm2) <- 'data.frame'
dist_M.adm2 <- as.matrix(dist_M.adm2)
names(dimnames(dist_M.adm2)) <- c('origin', 'destination')
# Get rid of 82 and 84 in distance matrix
dist_M.adm2.94 <- dist_M.adm2[-74, -74] 
dist_M.adm2.94 <- dist_M.adm2.94[-72, -72] 

# Make trip matrix
trip <- adm2[, c('id_start', 'id_end', 'sim_trip_pois')]
trip_M.adm2 <- reshape::cast(trip, id_start ~ id_end)            
# Label rows with district numbers
rownames(trip_M.adm2) <- adm2.start$id_start                         
# Get rid of the first column
trip_M.adm2 <- trip_M.adm2[ ,-1]
class(trip_M.adm2) <- 'data.frame'
trip_M.adm2 <- as.matrix(trip_M.adm2)
names(dimnames(trip_M.adm2)) <- c('origin', 'destination')

## ADM 1 ##
# Create population vectors
adm1.pop <- county.file.ordered %>% group_by(ID_1) %>%
  mutate(adm1.pop = sum(pop2010ppp)) %>%
  distinct(ID_1, NAME_1, adm1.pop, .keep_all = F)
pop.adm1 <- adm1.pop$adm1.pop

# Geocode for ADM1
adm1.pop$Address <- paste(adm1.pop$NAME_1, ' Region, Namibia')
register_google(key = 'AIzaSyDmhLWCw5G-wzKS472eXaUAHU-d4RdaLAQ', write = TRUE)
adm1.pop <- mutate_geocode(adm1.pop, location = Address, output = "latlona")

# Assemble distance and calculated trips matrices
# Start
adm1.start <- adm1.pop[, c('NAME_1', 'lat', 'lon', 'ID_1', 'adm1.pop')]
colnames(adm1.start) <- c('name_start', 'x_start', 'y_start', 'id_start', 'pop_start')
# End
adm1.end <- adm1.pop[, c('NAME_1', 'lat', 'lon', 'ID_1', 'adm1.pop')]
colnames(adm1.end) <- c('name_end', 'x_end', 'y_end', 'id_end', 'pop_end')

# Create all combinations
adm1 <- as.data.frame(NULL)
for (i in adm1.start$id_start) {
  print(i)
  adm1.start$id_end <- i
  adm1 <- rbind(adm1, left_join(adm1.start, adm1.end, by = c('id_end' = 'id_end')))
}

# Calculate distance (in km)
adm1$dist <- distHaversine(as.matrix(adm1[, c('y_start', 'x_start')]), as.matrix(adm1[, c('y_end', 'x_end')])) * 0.001

# Calculate trips based on gravity model
adm1$sim_trip <- ceiling((adm1$pop_start^0.8 * adm1$pop_end^0.2) / (adm1$dist^0.8))
adm1$sim_trip <- ifelse(is.infinite(adm1$sim_trip), 0, adm1$sim_trip) # replace w/ 0 if infinite
adm1$sim_trip <- as.numeric(adm1$sim_trip) # make sure it is numeric
adm1$sim_trip_pois <- unlist(lapply(adm1$sim_trip, function(x) rpois(1, x))) # calculate from poisson dist

# Make distance matrix
dist <- adm1[, c('id_start', 'id_end', 'dist')]
dist_M.adm1 <- reshape::cast(dist, id_start ~ id_end)            
# Label rows with district numbers
rownames(dist_M.adm1) <- adm1.start$id_start                         
# Get rid of the first column
dist_M.adm1 <- dist_M.adm1[ ,-1]
class(dist_M.adm1) <- 'data.frame'
dist_M.adm1 <- as.matrix(dist_M.adm1)
names(dimnames(dist_M.adm1)) <- c('origin', 'destination')

# Create ADM1 calculated trips by summing the ADM2 calculated trips
trip <- adm2 %>% group_by(id_1_start, id_1_end) %>% 
  mutate(sim.trip.adm1 = sum(sim_trip)) %>%
  distinct(id_1_start, id_1_end, sim.trip.adm1, .keep_all = FALSE)
trip$sim_trip_pois <- unlist(lapply(trip$sim.trip.adm1, function(x) rpois(1, x))) # calculate from poisson dist

# Make trip matrix (Aggregated)
trip_M.adm1 <- reshape::cast(trip, id_1_start ~ id_1_end)            
# Label rows with district numbers
rownames(trip_M.adm1) <- adm1.start$id_start                         
# Get rid of the first column
trip_M.adm1 <- trip_M.adm1[ ,-1]
class(trip_M.adm1) <- 'data.frame'
trip_M.adm1 <- as.matrix(trip_M.adm1)
names(dimnames(trip_M.adm1)) <- c('origin', 'destination')  
# Replace stays with 0
diag(trip_M.adm1) <- 0

# Make trip matrix (Calculated)
trip <- adm1[, c('id_start', 'id_end', 'sim_trip_pois')]
trip_M.adm1.calc <- reshape::cast(trip, id_start ~ id_end)            
# Label rows with district numbers
rownames(trip_M.adm1.calc) <- adm1.start$id_start                         
# Get rid of the first column
trip_M.adm1.calc <- trip_M.adm1.calc[ ,-1]
class(trip_M.adm1.calc) <- 'data.frame'
trip_M.adm1.calc <- as.matrix(trip_M.adm1.calc)
names(dimnames(trip_M.adm1.calc)) <- c('origin', 'destination')

########################################
# FIT SOME MODELS FROM CALCULATED DATA #
########################################

## ADM 2 ##
fit.adm2.calc = sampling(model,
                         list(n = 96,
                              pop = pop.adm2.96,
                              distance_matrix = dist_M.adm2,
                              trip_counts = trip_M.adm2),
                         iter = 2000,
                         chains = 2)

# They match! a = 0.8, b = 0.2, g = 0.8
print(fit.adm2.calc)

## ADM 1 ##
# Aggregated
fit.adm1.ag = sampling(model,
                         list(n = 13,
                              pop = pop.adm1,
                              distance_matrix = dist_M.adm1,
                              trip_counts = trip_M.adm1),
                         iter = 2000,
                         chains = 2)

# They don't match! a = 0.86, b = 0.42, g = 1.05
print(fit.adm1.ag)

# Calculated
fit.adm1.calc = sampling(model,
                       list(n = 13,
                            pop = pop.adm1,
                            distance_matrix = dist_M.adm1,
                            trip_counts = trip_M.adm1.calc),
                       iter = 2000,
                       chains = 2)

# They match! a = 0.8, b = 0.2, g = 0.8
print(fit.adm1.calc)

####################################
# FIT SOME MODELS FROM ACTUAL DATA #
####################################

## ADM 2 ##
fit.adm2.real = sampling(model,
                         list(n = 94,
                              pop = pop.adm2.94,
                              distance_matrix = dist_M.adm2.94,
                              trip_counts = M.monthly.avg.adm2),
                         iter = 2000,
                         chains = 2)

# a = 0.68, b = 0.67, g = 1.46
print(fit.adm2.real)

## ADM 1 ##
fit.adm1.real = sampling(model,
                         list(n = 13,
                              pop = pop.adm1,
                              distance_matrix = dist_M.adm1,
                              trip_counts = M.monthly.avg.adm1),
                         iter = 2000,
                         chains = 2)

# a = 0.72, b = 0.72, g = 0.1.46
print(fit.adm1.real)

######################################
# CALCULATE MODELED OUTPUTS FORM FIT #
######################################

# Remove data not in the data set
adm2 <- adm2[adm2$id_start != 84 | adm2$id_start != 84, ] 
adm2 <- adm2[adm2$id_start != 82 | adm2$id_start != 82, ] 

# Merge on actual trips for each level
# ADM1
merge_data.adm1 <- adm1.trip.month.avg[, c('start.adm1.code', 'end.adm1.code', 'adm1.single.trip.avg')]
adm1 <- left_join(adm1, merge_data.adm1, by = c('id_start' = 'start.adm1.code', 'id_end' = 'end.adm1.code'))

# ADM2
merge_data.adm2 <- adm2.trip.month.avg[, c('start.adm2.code', 'end.adm2.code', 'adm2.single.trip.avg')]
adm2 <- left_join(adm2, merge_data.adm2, by = c('id_start' = 'start.adm2.code', 'id_end' = 'end.adm2.code'))

# Drop Stays
# ADM1
adm1 <- adm1[adm1$id_start != adm1$id_end, ]

# ADM2
adm2 <- adm2[adm2$id_start != adm2$id_end, ]

# For ADM2 replace NAs from merge with 0 (trips not in CDRs because they didn't happen)
adm2$adm2.single.trip.avg <- ifelse(is.na(adm2$adm2.single.trip.avg), 0, adm2$adm2.single.trip.avg)

# Calculate modeled trips from fitted values
# ADM1
adm1$mod_trip.fit.adm1 <- ceiling((adm1$pop_start^0.72 * adm1$pop_end^0.72) / (adm1$dist^1.46))
adm1$mod_trip.fit.adm1 <- ifelse(is.infinite(adm1$mod_trip.fit.adm1), 0, adm1$mod_trip.fit.adm1) # replace w/ 0 if infinite
# Using ADM2 values
adm1$mod_trip.fit.from.adm2 <- ceiling((adm1$pop_start^0.68 * adm1$pop_end^0.67) / (adm1$dist^1.46))
adm1$mod_trip.fit.from.adm2<- ifelse(is.infinite(adm1$mod_trip.fit.from.adm2), 0, adm1$mod_trip.fit.from.adm2) # replace w/ 0 if infinite

# ADM2
adm2$mod_trip.fit.adm2 <- ceiling((adm2$pop_start^0.68 * adm2$pop_end^0.67) / (adm2$dist^1.46))
adm2$mod_trip.fit.adm2 <- ifelse(is.infinite(adm2$mod_trip.fit.adm2), 0, adm2$mod_trip.fit.adm2) # replace w/ 0 if infinite
# Using ADM1 values
adm2$mod_trip.fit.from.adm1 <- ceiling((adm2$pop_start^0.72 * adm2$pop_end^0.72) / (adm2$dist^1.46))
adm2$mod_trip.fit.from.adm1 <- ifelse(is.infinite(adm2$mod_trip.fit.from.adm1), 0, adm2$mod_trip.fit.from.adm1) # replace w/ 0 if infinite

# Calculate Ratio
# ADM1
adm1$ratio <- adm1$mod_trip.fit.adm1 / adm1$mod_trip.fit.from.adm2

# ADM2
adm2$ratio <- adm2$mod_trip.fit.adm2 / adm2$mod_trip.fit.from.adm1

# Calculate MSE
# ADM1
adm1$mse.adm1 <- sum((adm1$adm1.single.trip.avg - adm1$mod_trip.fit.adm1)^2) / nrow(adm1)
adm1$mse.from.adm2 <- sum((adm1$adm1.single.trip.avg - adm1$mod_trip.fit.from.adm2)^2) / nrow(adm1)

# ADM2
adm2$mse.adm2 <- sum((adm2$adm2.single.trip.avg - adm2$mod_trip.fit.adm2)^2) / nrow(adm2)
adm2$mse.from.adm1 <- sum((adm2$adm2.single.trip.avg - adm2$mod_trip.fit.from.adm1)^2) / nrow(adm2)



# mobility-spatial-scale
Evaluating the impact of mobility data spatial aggregation on infectious disease modeling results.

Data availability:
* The observed data folder contains censored observed data of the average daily number of trips between two locations. Trips fewer than 10 are censored and the remaining values are rounded to the nearest 10.
* The simulated data folder contains simulated data of the average daily number of trips between two locations. Data is simulated from exponential gravity models fit to the uncensored observed data.

The repository structure is as follows: 
- 01: importation of data and creation of crosswalks
- 02: formatting data for analyses
- 03: data simulation with gravity models
- 04: infectious disease modeling
- 05: analysis and figure creation



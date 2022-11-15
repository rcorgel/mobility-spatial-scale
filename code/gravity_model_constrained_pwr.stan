////////////////////////////////////////////////////////////////////////////////
// File Name: gravity_model_constrained_pwr                                   //
//                                                                            //
// Purpose:   Create a gravity model with a power decay distance function.    // 
//                                                                            //
// Steps:     1. Define data inputs                                           //
//            2. Define Parameters                                            //
//            3. Create the model                                             //
//                                                                            //
// Project:   Sri Lanka Spatial Aggregation                                   //
// Author:    Ronan Corgel                                                    //
////////////////////////////////////////////////////////////////////////////////


///////////////////////////
// 1. DEFINE DATA INPUTS //
///////////////////////////

data {
  int <lower=0> n;                // number of administrative units
  real pop[n];                    // administrative units population vector
  real distance_matrix[n, n];     // origin destination distance matrix
  int trip_counts[n, n];          // observed trip count matrix
}

//////////////////////////
// 2. DEFINE PARAMETERS //
//////////////////////////

parameters {
  real <lower=0> alpha_param;     // origin city population parameter
  real <lower=0> beta_param;      // destination city population parameter
  real <lower=0> gamma_param;     // origin destination distance parameter
  real <lower=0> theta_param;     // scale parameter
}

/////////////////////////
// 3. CREATE THE MODEL //
/////////////////////////

model {
  real pwr_counts[n, n];          // expected counts matrix
  
  // Set priors for each parameter
  gamma_param ~ gamma(5, 1);
  beta_param ~ gamma(1, 2);
  theta_param ~ uniform(1, 10000);

  // Loop through all origin destination combinations and fill in the expected 
  // counts matrix
  for (i in 1:n) {
    for (j in 1:n) {
      if (!(i == j)) {
        pwr_counts[i, j] = theta_param * pop[j] ^ beta_param * distance_matrix[i, j] ^ -gamma_param;
      }
      if ((i == j)) {
        pwr_counts[i, j] = 0;
      }
    }
  }
  
  // Loop through all origin destination combinations and fit singly constrained 
  // gravity model to the observed data
  for (i in 1:n) {
    for (j in 1:n) {
      if (!(i == j)) {
        trip_counts[i, j] ~ poisson(sum(trip_counts[i, ]) * pwr_counts[i, j] / sum(pwr_counts[i, ]));
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

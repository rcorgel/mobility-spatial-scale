////////////////////////////////////////////////////////////////////////////////
// File Name: gravity_model_constrained_exp                                   //
//                                                                            //
// Purpose:   Create a gravity model with an exponential decay distance       // 
//            function.                                                       //
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
  real <lower=0> beta_param;      // destination city population parameter
  real <lower=0> gamma_param;     // origin destination distance parameter
}

/////////////////////////
// 3. CREATE THE MODEL //
/////////////////////////

model {
  real exp_counts[n, n];          // expected counts matrix
  
  // Set priors for each parameter
  gamma_param ~ gamma(5, 1);
  beta_param ~ gamma(1, 2);

  // Loop through all origin destination combinations and fill in the expected 
  // counts matrix
  for (i in 1:n) {
    for (j in 1:n) {
      if (!(i == j)) {
        exp_counts[i, j] = pop[j] ^ beta_param * exp(-gamma_param * distance_matrix[i, j]);
      }
      if ((i == j)) {
        exp_counts[i, j] = 0;
      }
    }
  }

  // Loop through all origin destination combinations and fit singly constrained 
  // gravity model to the observed data
  for (i in 1:n) {
    for (j in 1:n) {
      if (!(i == j)) {
        trip_counts[i, j] ~ poisson(sum(trip_counts[i, ]) * exp_counts[i, j] / sum(exp_counts[i, ]));
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

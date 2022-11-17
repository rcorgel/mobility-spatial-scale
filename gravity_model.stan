
data {
  int <lower=0> n;
  real pop[n];
  real distance_matrix[n, n];
  int trip_counts[n, n];
}

parameters {
  real <lower=0> alpha_param;
  real <lower=0> beta_param;
  real <lower=0> gamma_param;
}

model {
  
  gamma_param ~ gamma(5, 1);
  beta_param ~ gamma(1, 2);
  alpha_param ~ gamma(1, 2);
  
  for(i in 1:n){
    for(j in 1:n){
      if(!(i == j)){
        trip_counts[i, j] ~ poisson(pop[i] ^ alpha_param * pop[j] ^ beta_param / (distance_matrix[i, j]^gamma_param));
      }
    }
  }
}

// stan program for AR(K) probability model
// saved as ARModel.stan
// Author: Nan Wu

data{
  int<lower=1> N; // number of observations
  int<lower=1> K; // order number of AR model
  vector[N] y; // outcome vector
}
parameters{
  real alpha; // constant of AR model
  row_vector[K] beta; // coefficient vector of AR model
  real<lower = 0> sigma; // scale parameter for white noise
}
// This model has improper priors for the parameters.
model{
  matrix[K, N-K] z;
  for(k in 1:K){
    z[k] = y[(K-k+1):(N-k)]';
  }
  y[(K+1):N] ~ normal(alpha + beta * z, sigma); // vectorized for efficiency
}

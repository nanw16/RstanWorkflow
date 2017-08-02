// stan program for ARCH(K) probability model
// saved as ARCHModel.stan
// Author: Nan Wu

data{
  int<lower=1> T; // number of time points
  int<lower=1> K; // order number of ARCH model
  vector[T] r; // asset returns
}
parameters{
  real mu; // average return
  real<lower=0> alpha0; // intercept for volatility model
  simplex[K] alpha_unscaled; // sums to 1
  real<lower=0, upper=1> sum_alpha; // sum of alpha1 to alphaK
}
transformed parameters{
  vector[K] alpha;
  alpha = sum_alpha * alpha_unscaled; // alphas sum to sum_alpha in (0,1)
}
// This model has improper priors for the parameters.
model{
  matrix[K, T-K] z;
  row_vector[T-K] sigma;
  for(k in 1:K){
    z[k] = r[(K-k+1):(T-k)]' - mu; // elementwise squared
  }
  sigma = sqrt(alpha0 + alpha' * (z .* z));
  r[(K+1):T] ~ normal(mu, sigma); // vectorized for efficiency
}

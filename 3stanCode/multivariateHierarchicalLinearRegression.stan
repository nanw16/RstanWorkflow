// stan program for a two level multivariate hierarchical linear regression probability model
// saved as multivariateHierarchicalLinearRegression.stan
// Author: Nan Wu

data{
  int<lower=1> N; // number of individual observations
  int<lower=2> K; // number of individual predictors
  int<lower=1> J; // number of groups
  int<lower=1> L; // number of group predictors
  int<lower=1, upper=J> jj[N]; // vector of group index for each individual obs.
  matrix[N, K] x; // individual predictors
  matrix[L, J] u; // group predictors
  vector[N] y; // outcomes
}
parameters{
  real<lower=0> sigma; // individual prediction error scale
  matrix[K, J] z; // auxiliary parameter for individual coefficicents by group
  matrix[K, L] gamma; // group coefficients
  // auxiliary parameter for scale vector for covariance matrix of beta
  vector<lower=0, upper=pi()/2>[K] tau_unif;
  cholesky_factor_corr[K] L_Omega; // Cholesky factor of correlation matrix Omega
}
transformed parameters{
  matrix[K, J] beta; // individual coefficicents by group
  vector<lower=0>[K] tau; // scale vector for covariance matrix of beta
  tau = 2.5 * tan(tau_unif);
  beta = gamma * u + diag_pre_multiply(tau, L_Omega) * z;
}
model{
  vector[N] mu_y;
  // L_Sigma = diag_pre_multiply(tau, L_Omega);
  // Sigma = L_Sigma * L_Sigma';
  to_vector(z) ~ normal(0, 1); // implies beta ~ multinormal(gamma * u, Sigma)
  L_Omega ~ lkj_corr_cholesky(2.0); // implies L_Omega * L_Omega' ~ lkj_corr(2.0)
  to_vector(gamma) ~ normal(0, 5);
  // tau_unif ~ uniform(0, pi()/2);
  for (n in 1:N){
    mu_y[n] = x[n] * beta[,jj[n]];
  }
  y ~ normal(mu_y,sigma);
}

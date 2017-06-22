// stan program for a two level hierarchical logistic regression probability model
// saved as hierarchicalLogisticRegression2.stan
// Author: Nan Wu

data{
  int<lower=1> N; // number of observations
  int<lower=1> D; // number of predictors
  int<lower=1> L; // number of groups for the data
  matrix[N, D] x; // covariate matrix
  int<lower=0, upper=1> y[N]; // outcome vector
  int<lower=1, upper=L> ll[N]; // group index 
}
parameters{
  vector[D] mu;
  vector<lower=0>[D] sigma;
  vector[D] beta_raw[L];
}
transformed parameters{
  vector[D] beta[L];
  for (l in 1:L){
    beta[l] = mu + beta_raw[l].*sigma; // elementwise product
  }
}
model{
  vector[N] z;
  for (n in 1:N){
    z[n] = x[n] * beta[ll[n]];
  }
  mu ~ normal(0, 100); // prior for mu
  sigma ~ gamma(2, 1.0/10); // prior for sigma
  // This is for convergence
  for (l in 1:L){
    beta_raw[l] ~ normal(0, 1); // implies beta[l] ~ normal(mu, sigma)
  }
  y ~ bernoulli_logit(z);
  // for (n in 1:N){
  //   y[n] ~ bernoulli_logit(z[n]);
  // }
}

// stan program for the one predictor linear regression probability model
// saved as linearRegreesionOnePredictor.stan
// Author: Nan Wu

data{
  int<lower=1> N; // number of observations
  vector[N] x; // covariate vector
  vector[N] y; // outcome vector
}
parameters{
  real alpha; // the intercept
  real beta; // the regression coefficient
  real<lower=0> sigma; // the residual standard deviation
}
// This model has improper priors for the parameters.
model{
  vector[N] mu; // local to the model block
  //alpha ~ normal();
  //beta ~ normal();
  //sigma ~ uniform();
  mu = alpha + x*beta;
  y ~ normal(mu, sigma);
}

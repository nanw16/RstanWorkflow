// stan program for the multi-predictor linear regression probability model
// saved as linearRegreesionMultPredictor.stan
// Author: Nan Wu

data{
  int<lower=1> N; // number of observations
  int<lower=1> P; // number of predictors
  matrix[N, P] x; // covariate matrix
  vector[N] y; // outcome vector
}
parameters{
  real alpha; // the intercept
  vector[P] beta; // the regression coefficient vector
  real<lower=0> sigma; // the residual standard deviation
}
// This model has improper priors for the parameters.
model{
  vector[N] mu; // local to the model block
  //alpha ~ normal();
  //beta ~ normal();
  //sigma ~ uniform();
  mu = alpha + x*beta;
  y ~ normal(mu, sigma); // likelihood
}

// stan program for the multi-predictor linear robust regression probability model
// saved as linearRobustRegreesionMultPredictor.stan
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
  real<lower=0> nu; // the degree of freedom for t distribution
}
// This model has improper priors for the parameters.
model{
  vector[N] mu; // local to the model block
  mu = alpha + x*beta;
  nu ~ cauchy(7, 5); // improper prior doesn't work for nu
  y ~ student_t(nu, mu, sigma); // likelihood
}

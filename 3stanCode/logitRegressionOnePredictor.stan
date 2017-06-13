// stan program for the one predictor logit regression probability model
// saved as logitRegreesionOnePredictor.stan
// Author: Nan Wu

data{
  int<lower=1> N; // number of observations
  vector[N] x; // covariate vector
  int<lower = 0, upper = 1> y[N]; // outcome vector
}
parameters{
  real alpha; // the intercept
  real beta; // the regression coefficient
}
// This model has improper priors for the parameters.
model{
  y ~ bernoulli_logit(alpha + x*beta); // likelihood
}

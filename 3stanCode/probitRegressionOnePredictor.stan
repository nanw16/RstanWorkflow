// stan program for the one predictor probit regression probability model
// saved as probitRegreesionOnePredictor.stan
// Author: Nan Wu

data{
  int<lower=1> N; // number of observations
  vector[N] x; // predictor vector
  int<lower=0,upper=1> y[N]; // outcome vector
}
parameters{
  real alpha; // the intercept
  real beta; // the regression coefficient
}
// improper priors for alpha and beta
model{
  y ~ bernoulli(Phi(alpha + x*beta)); //likelihood
}

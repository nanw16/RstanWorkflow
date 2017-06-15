// stan program for the ordered logistic regression probability model
// saved as orderedLogisticRegression.stan
// Author: Nan Wu

data{
  int<lower=1> N; // number of observations
  int<lower=1> P; // number of predictors
  int<lower=2> K; // number of classes for outcome
  matrix[N,P] x; // covariate matrix
  int<lower=1,upper=K> y[N]; // outcome vector
}
parameters{
  vector[P] beta; // the regression coefficient vector
  ordered[K-1] c; // cutpoints
}
// This model has improper priors for the parameters.
model{
  for (n in 1:N){
    y[n] ~ ordered_logistic(x[n]*beta, c); //likelihood
  }
}

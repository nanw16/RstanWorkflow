// stan program for the multi-predictor logit regression probability model
// saved as multLogitRegression.stan
// Author: Nan Wu

data{
  int<lower=1> N; // number of observations
  int<lower=1> P; // number of predictors
  int<lower=1> K; // number of classes for outcome
  matrix[N,P] x; // covariate matrix
  //vector[P] x[N]; // covariate matrix, might be confusing when supply data in R
  int y[N]; // outcome vector
}
parameters{
  matrix[P,K] beta; // the regression coefficient
  //matrix[K,P] beta; // the regression coefficients
}
// This model has improper priors for the parameters.
model{
  for (k in 1:K){
    beta[, k] ~ normal(0, 5);
    //beta[k] ~ normal(0, 5);
  }
  for (n in 1:N){
    y[n] ~ categorical_logit((x[n]*beta)'); //likelihood
    //y[n] ~ categorical_logit(beta * x[n]); //likelihood
    # categorical_logit(vector), row_vector wouldn't work
  }
}

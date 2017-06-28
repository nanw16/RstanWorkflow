// stan program for the ordered probit regression probability model
// saved as orderedProbitRegression.stan
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
  vector[K] theta; // probability vector for y
  for (n in 1:N){
    real z; // local variable
    z = x[n] * beta;
    theta[1] = Phi(c[1]-z);
    for (k in 2:(K-1)){
      theta[k]= Phi(c[k] - z) - Phi(c[k-1] - z);
    }
    theta[K] = 1 - Phi(c[K-1] - z);
    y[n] ~ categorical(theta); //likelihood
  }
}

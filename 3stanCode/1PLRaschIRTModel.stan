// stan program for a 1PL Item-response model
// saved as 1PLRaschIRTModel.stan
// Author: Nan Wu

data{
  int<lower=1> N; // number of observations
  int<lower=1> J; // number of students
  int<lower=1> K; // number of questions
  int<lower=1, upper=J> jj[N]; // student ID for observation n
  int<lower=1, upper=K> kk[N]; // question no. for observation n
  int<lower=0, upper=1> y[N]; // correctness for observation n
}
parameters{
  vector[J] alpha_raw; // student ability
  real mu_alpha; // mean student ability
  vector[K] beta; // question difficuty
}
transformed parameters{
  vector[J] alpha;
  alpha = mu_alpha + alpha_raw;
}
model{
  vector[N] z;
  alpha_raw ~ normal(0, 1); // implies alpha ~ normal(mu_alpha, 1)
  mu_alpha ~ normal(0.75, 1);
  beta ~ normal(0, 1);
  for (n in 1:N){
    z[n] = alpha[jj[n]] - beta[kk[n]]; // for efficiency
  }
  y ~ bernoulli_logit(z);
}

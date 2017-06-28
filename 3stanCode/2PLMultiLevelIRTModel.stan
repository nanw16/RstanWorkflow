// stan program for a Multilevel 2PL Item-response model
// saved as 2PLMultiLevelIRTModel.stan
// reparameterized for convergence(normal, lognormal, cauchy)
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
  vector[J] alpha; // student ability
  vector[K] beta_raw; // question difficulty
  vector[K] log_gamma_raw; //discrimination for question
  real<lower = -pi()/2, upper = pi()/2> mu_beta_unif; // mean question difficulty
  real<lower = 0, upper = pi()/2> sigma_beta_unif; // sd of question diffculties
  real<lower = 0, upper = pi()/2> sigma_gamma_unif; // sd of question discriminations
}
transformed parameters{
  vector[K] beta;  // question difficulty
  vector[K] log_gamma; // discrimination for question
  real mu_beta; // mean question difficulty
  real<lower = 0> sigma_beta; // sd of question diffculties
  real<lower = 0> sigma_gamma; // sd of question discriminations
  mu_beta = 5 * tan(mu_beta_unif); // mean question difficulty
  sigma_beta = 5 * tan(sigma_beta_unif); // sd of question diffculties
  sigma_gamma = 5 * tan(sigma_gamma_unif); // sd of question discriminations
  beta = mu_beta + sigma_beta*beta_raw;
  log_gamma = sigma_gamma * log_gamma_raw;
}
model{
  vector[N] z;
  alpha ~ normal(0, 1);
  beta_raw ~ normal(0, 1); // implies beta ~ normal(0, sigma_beta)
  log_gamma_raw ~ normal(0, 1); // implies gamma ~ lognormal(0, sigma_gamma)
  mu_beta_unif ~ uniform(-pi()/2, pi()/2); // implies mu_beta ~ cauchy(0, 5)
  sigma_beta_unif ~ uniform(0, pi()/2); // implies sigma_beta ~ cauchy(0, 5) T[0,]
  sigma_gamma_unif ~ uniform(0, pi()/2); // implies sigma_gamma ~ cauchy(0, 5) T[0,]
  for (n in 1:N){
    z[n] = exp(log_gamma[kk[n]]) * (alpha[jj[n]] - beta[kk[n]]); // for efficiency
  }
  y ~ bernoulli_logit(z);
}


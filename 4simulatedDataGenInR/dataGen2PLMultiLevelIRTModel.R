# Simulated Data Generation for 2PL Multi-level IRT Model
# Author: Nan Wu

library("LaplacesDemon")
set.seed(123)
N <- 1000 # number of observations
J <- 4 # number of students
K <- 6 # number of questions

jj <- sample(1:J, N, replace = TRUE)

kk <- sample(1:K, N, replace = TRUE)

mu_beta <- rcauchy(1, 0, 5)
sigma_beta <- rhalfcauchy(1, scale = 5)
sigma_gamma <- rhalfcauchy(1, scale = 5)

alpha <- rnorm(J, 0, 1)
beta <- rnorm(K, mu_beta, sigma_beta)
gamma <- rlnorm(K, 0, sigma_gamma)
# log_gamma <- rnorm(K, 0, sigma_gamma)
# gamma <- exp(log_gamma)

y <- vector(mode = "numeric", length = N)
for(n in 1:N){
  y[n] <- rbern(1, invlogit(gamma[kk[n]] * (alpha[jj[n]] - beta[kk[n]])))
}

simData <- data.frame(observation = 1:N, jj = jj, kk = kk, y = y)

varName <- c(paste0("alpha[",1:J,"]"), paste0("beta[", 1:K, "]"),
             paste0("gamma[", 1:K, "]"), "mu_beta",
             "sigma_beta", "sigma_gamma")
knownPar <- data.frame(varName = varName,
                       true_value = c(alpha, beta, gamma, mu_beta,
                                      sigma_beta, sigma_gamma))

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write data to data2PLMultiLevelIRTModel.csv
write.csv(simData, file.path(directory,"data2PLMultiLevelIRTModel.csv"),
          row.names = FALSE)

# Write parameter to par2PLMultiLevelIRTModel.csv
write.csv(knownPar, file.path(directory,"par2PLMultiLevelIRTModel.csv"),
          row.names = FALSE)

# Simulated Data Generation for 1PL IRT Model
# Author: Nan Wu

library("LaplacesDemon")
set.seed(666)
N <- 1000 # number of observations
J <- 4 # number of students
K <- 6 # number of questions

jj <- sample(1:J, N, replace = TRUE)

kk <- sample(1:K, N, replace = TRUE)

mu_alpha <- rnorm(1, 0.75, 1)

alpha <- rnorm(J, mu_alpha, 1)

beta <- rnorm(K, 0, 1)

y <- vector(mode = "numeric", length = N)
for(n in 1:N){
  y[n] <- rbern(1, invlogit(alpha[jj[n]] - beta[kk[n]]))
}

simData <- data.frame(observation = 1:N, jj = jj, kk = kk, y = y)


varName <- c(paste0("alpha[",1:J,"]"), paste0("beta[", 1:K, "]"))

knownPar <- data.frame(varName = varName, true_value = c(alpha, beta))

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write data to data1PLRaschIRTModel.csv
write.csv(simData, file.path(directory,"data1PLRaschIRTModel.csv"),
          row.names = FALSE)

# Write parameter to par1PLRaschIRTModel.csv
write.csv(knownPar, file.path(directory,"par1PLRaschIRTModel.csv"),
          row.names = FALSE)

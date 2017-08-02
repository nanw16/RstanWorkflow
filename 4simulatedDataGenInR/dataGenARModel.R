# Simulated Data Generation for an AR(K) Model
# Author: Nan Wu

set.seed(111)
N <- 100 # number of observations
K <- 2 # order number of AR model
alpha <- rnorm(1, 0.5, 2) # generate a random constant parameter to be recovered
beta <- rnorm(K, 0.5, 2) # generate a random coefficient vector to be recovered
sigma <- 2 # scale parameter for the noise term
# e <- rnorm(N, 0, sigma)
y <- vector(mode = "numeric", length = N)
y[1:K] <- rnorm(K, 1.5, 2) # initialize y(1) to y(K)
for(n in (K+1):N){
  y[n] <- rnorm(1, alpha + beta %*% y[(n-1):(n-K)], sigma)
  # y[n] <- alpha + beta %*% y[(n-1):(n-K)] + rnorm(1, 0, sigma)
}

simData <- data.frame(observation = 1:N, K = K, y = y)

knownPar <- data.frame(varName = c("alpha", paste0("beta[",1:K,"]"), "sigma"), 
                       true_value = c(alpha, beta, sigma))

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write data to dataARModel.csv
write.csv(simData, file.path(directory,"dataARModel.csv"),
          row.names = FALSE)

# Write parameter to parARMode.csv
write.csv(knownPar, file.path(directory,"parARModel.csv"),
          row.names = FALSE)


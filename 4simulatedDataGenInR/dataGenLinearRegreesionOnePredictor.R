# Simulated Data Generation for One Predictor Linear Regression Model
# Author: Nan Wu

set.seed(66)
N <- 1000 # number of observations
x <- rnorm(N) # generate a convariate vector of length N
alpha <- rnorm(1, 0.5, 2) # generate a random intercept that we will try to recover
beta <- rnorm(1, 0.5, 2) # generate a random coefficient that we will try to recover
sigma <- 4 # scale parameter for the residual
y <- vector(mode = "numeric", length = N)
for(n in 1:N){
  y[n] <- rnorm(1, alpha + X[n]*beta, sigma)
}

simData <- data.frame(observation = 1:N, x = x, y = y)

knownPar <- data.frame(varName = c("alpha", "beta", "sigma"), 
                       true_value = c(alpha, beta, sigma))

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write data to dataLinearRegressionOnePredictor.csv
write.csv(simData, file.path(directory,"dataLinearRegressionOnePredictor.csv"),
          row.names = FALSE)

# Write parameter to parLinearRegressionOnePredictor.csv
write.csv(knownPar, file.path(directory,"parLinearRegressionOnePredictor.csv"),
          row.names = FALSE)


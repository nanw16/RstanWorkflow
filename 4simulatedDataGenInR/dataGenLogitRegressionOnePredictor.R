# Simulated Data Generation for One Predictor Logistic Regression Model
# Author: Nan Wu

library("LaplacesDemon")
set.seed(66)
N <- 1000 # number of observations
x <- rnorm(N) # generate a convariate vector of length N
alpha <- rnorm(1, 0.5, 2) # generate a random intercept that we will try to recover
beta <- rnorm(1, 0.5, 2) # generate a random coefficient that we will try to recover
y <- vector(mode = "numeric", length = N)

# draw outcome samples from bernoulli distribution with p=invlogit(alpha + x[n]*beta)
for(n in 1:N){
  y[n] <- rbern(1, invlogit(alpha + x[n]*beta))
}

simData <- data.frame(observation = 1:N, x = x, y = y)

knownPar <- data.frame(varName = c("alpha", "beta"), 
                       true_value = c(alpha, beta))

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write data to dataLogitRegressionOnePredictor.csv
write.csv(simData, file.path(directory,"dataLogitRegressionOnePredictor.csv"),
          row.names = FALSE)

# Write parameter to parLogitRegressionOnePredictor.csv
write.csv(knownPar, file.path(directory,"parLogitRegressionOnePredictor.csv"),
          row.names = FALSE)
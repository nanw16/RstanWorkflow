# Simulated Data Generation for Multi-Predictor Linear Regression Model
# Author: Nan Wu
set.seed(66)
N <- 1000 # number of observations
P <- 5 # number of predictors
x <- matrix(rnorm(N*P), nrow = N, ncol = P, byrow = FALSE) # generate a convariate matrix

alpha <- rnorm(1, 0.5, 2) # generate a random intercept that we will try to recover
beta <- rnorm(P, 0.5, 2) # generate a random coefficient vector that we will try to recover
sigma <- 4 # scale parameter for the residual
y <- vector(mode = "numeric", length = N)

for(n in 1:N){
  y[n] <- rnorm(1, alpha + x[n,]%*%beta, sigma)
}

simData <- data.frame(observation = 1:N, x = x, y = y)
colnames(simData) <- c("observation", paste0("x",1:P), "y")

knownPar <- data.frame(varName = c("alpha", paste0("beta[",1:P,"]"), "sigma"), 
                       true_value = c(alpha, beta, sigma))

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write data to dataLinearRegressionMultPredictor.csv
write.csv(simData, file.path(directory,"dataLinearRegressionMultPredictor.csv"),
          row.names = FALSE)

# Write parameter to parLinearRegressionMultPredictor.csv
write.csv(knownPar, file.path(directory,"parLinearRegressionMultPredictor.csv"),
          row.names = FALSE)

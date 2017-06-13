# Simulated Data Generation for Multi-Predictor Robust Linear Regression Model
# Author: Nan Wu

set.seed(66)
N <- 1000 # number of observations
P <- 5 # number of predictors
x <- matrix(rnorm(N*P), nrow = N, ncol = P, byrow = FALSE) # generate a convariate matrix

# Parameters to be recovered later from the model
alpha <- rnorm(1, 0.5, 2) # generate a random intercept
beta <- rnorm(P, 0.5, 2) # generate a random coefficient vector
sigma <- 4 # scale parameter for the residual
nu <- 5 # set degree of freedom
y <- vector(mode = "numeric", length = N)

# draw samples from a linear regression with data matrix X, intercept alpha, coefficients beta, and student-t noise with degrees of freedom nu
for(n in 1:N){
  y[n] <- alpha + x[n,]%*%beta + sigma * rt(1, nu)
}

simData <- data.frame(observation = 1:N, x = x, y = y)
colnames(simData) <- c("observation", paste0("x", 1:P), "y")

knownPar <- data.frame(varName = c("alpha", paste0("beta[",1:P,"]"), "sigma", "nu"),
                       true_value = c(alpha, beta, sigma, nu))

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write data to dataLinearRobustRegressionMultPredictor.csv
write.csv(simData, 
          file.path(directory,"dataLinearRobustRegressionMultPredictor.csv"),
          row.names = FALSE)

# Write parameter to parLinearRobustRegressionMultPredictor.csv
write.csv(knownPar, 
          file.path(directory,"parLinearRobustRegressionMultPredictor.csv"),
          row.names = FALSE)

# Simulated Data Generation for Ordered Probit Regression Model
# Author: Nan Wu

library("LaplacesDemon")
set.seed(66)
N <- 1000 # number of observations
P <- 5 # number of predictors
K <- 3 # number of classes for the outcomes
x <- matrix(rnorm(N*P), nrow = N, ncol = P, byrow = FALSE) # generate a convariate matrix
beta <- rnorm(P, 0, 5) # generate a random coefficient vector that we will try to recover
z <- x%*%beta
c <- sort(runif(K-1, min = min(z), max = max(z))) #cutpoints that we will try to recover

# Probability Matrix
pr <- matrix(data = NA, nrow = N, ncol = K)
pr[,1] <- pnorm(c[1] - z)
pr[,K] <- 1- pnorm(c[K-1] - z)

if(K > 2){
  for(k in 2:(K-1)){
    pr[,k] <- pnorm(c[k] - z) - pnorm(c[k-1] - z)
  }
}

# draw samples from categorical distribution with probability pr
y <- vector(mode = "numeric", length = N)

# draw samples from categorical distribution with probability pr
for(n in 1:N){
  y[n] <- rcat(1, pr[n,])
}

simData <- data.frame(observation = 1:N, x = x, y = y)
colnames(simData) <- c("observation", paste0("x",1:P), "y")

knownPar <- data.frame(varName = c(paste0("beta[",1:P,"]"),paste0("c[",1:(K-1),"]")),
                       true_value = c(beta, c))

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write data to dataOrderedProbitRegression.csv
write.csv(simData, file.path(directory,"dataOrderedProbitRegression.csv"),
          row.names = FALSE)

# Write parameter to parOrderedProbitRegression.csv
write.csv(knownPar, file.path(directory,"parOrderedProbitRegression.csv"),
          row.names = FALSE)

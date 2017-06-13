# Simulated Data Generation for multi-Predictor Logistic Regression Model
# Author: Nan Wu

library("LaplacesDemon")
set.seed(66)
N <- 1000 # number of observations
P <- 5 # number of predictors
K <- 3 # number of classes for the outcomes
x <- matrix(c(rep(1, N),rnorm(N*P)), nrow = N, ncol = P+1, byrow = FALSE) # generate a convariate matrix
beta <- matrix(rnorm((P+1)*K, 0, 5), nrow = P+1, ncol = K, byrow = FALSE) # generate a random coefficient matrix that we will try to recover

# Probability Matrix
p <- exp(x%*%beta)
p <- p / rowSums(p)
# y <- vector(mode = "numeric", length = N)

# draw samples from categorical distribution with probability p
y <- rcat(N, p)
# for(n in 1:N){
#   y[n] <- rcat(1, p[n,])
# }

simData <- data.frame(observation = 1:N, x = x, y = y)
colnames(simData) <- c("observation", paste0("x",0:P), "y")

beta <- c(beta)

varName <- vector()
for (k in 1:K){
  varName <- append(varName, paste0("beta[",1:(P+1),",", k,"]"))
}
knownPar <- data.frame(varName = varName, true_value = beta)

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write data to dataMultLogitRegression.csv.csv
write.csv(simData, file.path(directory,"dataMultLogitRegression.csv"),
          row.names = FALSE)

# Write parameter to parMultLogitRegression.csv
write.csv(knownPar, file.path(directory,"parMultLogitRegression.csv"),
          row.names = FALSE)

# Simulated Data Generation for an ARCH(K) Model
# Author: Nan Wu
library("LaplacesDemon")
set.seed(123)
T <- 100 # number of time points
K <- 2 # order number of ARCH model

scale <- 0.7
mu <- rnorm(1, 0.5, 2) # generate a random average return to be recovered
alpha0 <- rhalfnorm(1, scale = 2) + 0.5
alpha <- rhalfnorm(K, scale = 2) + 0.5

# alpha0 <- -0.5
# while(alpha0 <= 0){
#   alpha0 <- rnorm(1, 0.5, 2)
# }
# alpha <- rep(-1, K)
# for(k in 1:K){
#   while(alpha[k] < 0){
#     alpha[k] <- rnorm(1, 0.5, 2)
#   }
# }
alpha <- alpha / sum(alpha) * scale

r <- vector(mode = "numeric", length = T)
r[1:K] <- rnorm(K, 1.5, 2) # initialize r(1) to r(K)
for(t in (K+1):T){
  sigma <- sqrt(alpha0 + alpha %*% ((r[(t-1):(t-K)] - mu)^2))[1,1]
  r[t] <- mu + sigma * rnorm(1, 0, 1)
}

simData <- data.frame(observation = 1:T, K = K, r = r)

knownPar <- data.frame(varName = c("mu","alpha0", paste0("alpha[",1:K,"]")), 
                       true_value = c(mu, alpha0, alpha))

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write data to dataARCHModel.csv
write.csv(simData, file.path(directory,"dataARCHModel.csv"),
          row.names = FALSE)

# Write parameter to parARCHMode.csv
write.csv(knownPar, file.path(directory,"parARCHModel.csv"),
          row.names = FALSE)


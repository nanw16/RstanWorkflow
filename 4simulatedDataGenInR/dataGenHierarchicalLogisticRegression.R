# Simulated Data Generation for Hierarchical Logistic Regression Model
# Author: Nan Wu

library("LaplacesDemon")
set.seed(66)
N <- 1000 # number of observations
D <- 5 # number of predictors
L <- 4 # number of groups for the data

x <- matrix(c(rep(1, N),rnorm(N*D)), nrow = N, ncol = D+1, byrow = FALSE) # generate a convariate matrix

mu <- rnorm(D+1, 0, 100)

# sigma <- runif(D+1, 0, 6)
sigma <- rgamma(D+1, shape = 2, rate = 1/10)

beta <- matrix(data = NA, nrow = L, ncol = D+1)

for(d in 1:(D+1)){
  beta[, d] <- rnorm(L, mu[d], sigma[d])
}
# for(l in 1:L){
#   for(d in 1:(D+1)){
#     beta[l,d] <- rnorm(1, mu[d], sigma[d])
#   }
# }

ll <- sample(1:L, N, replace = TRUE)

# draw outcome samples from bernoulli distribution with 
# p=invlogit(x[n,]%*%beta[ll[n],])

y <- vector(mode = "numeric", length = N)
for(n in 1:N){
  y[n] <- rbern(1, invlogit(x[n,]%*%beta[ll[n],]))
}

simData <- data.frame(observation = 1:N, x = x, ll = ll, y = y)
colnames(simData) <- c("observation", paste0("x",0:D),"ll", "y")

beta <- c(beta)

varName_mu <- paste0("mu[", 1:(D+1), "]")
varName_sigma <- paste0("sigma[", 1:(D+1),"]")

varName_beta <- vector()
for (d in 1:(D+1)){
  varName_beta <- append(varName_beta, paste0("beta[",1:L,",", d,"]"))
}
knownPar <- data.frame(varName = c(varName_mu, varName_sigma, varName_beta), true_value = c(mu, sigma, beta))

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write data to dataMultLogitRegression.csv.csv
write.csv(simData, file.path(directory,"dataHierarchicalLogisticRegression.csv"),
          row.names = FALSE)

# Write parameter to parMultLogitRegression.csv
write.csv(knownPar, file.path(directory,"parHierarchicalLogisticRegression.csv"),
          row.names = FALSE)

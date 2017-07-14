# Simulated Data Generation for A Multivariate Hierarchical Linear Regression Model
# Author: Nan Wu

library("LaplacesDemon")
library("MASS")
library("rethinking")
set.seed(123)
N <- 100 # number of observations
K <- 3 # number of individual predictors
J <- 2 # number of groups
L <- 4 # number of group predictors

jj <- sample(1:J, N, replace = TRUE) # vector of group index for each obs.

x <- matrix(c(rep(1, N), rnorm(N*(K-1))), nrow = N, ncol = K, byrow = FALSE) # individual predictors
u <- matrix(c(rep(1, J), rnorm(J*(L-1))), nrow = L, ncol = J, byrow = TRUE) # group predictors

sigma <- rhalfcauchy(1, scale = 5) # scale for individual prediction error

gamma <- matrix(rnorm(K*L, 0, 5), nrow = K, ncol = L, byrow = FALSE) # group coeff

tau <- rhalfcauchy(K, scale = 2.5)
Omega <- rlkjcorr(1, K, eta = 2)
Sigma <- tau %*% t(tau) * Omega
L_Omega <- t(chol(Omega))

# tmp <- matrix(runif(K*K, -2, 2), nrow = K, ncol = K)
# tmp[upper.tri(tmp, diag = FALSE)] <- 0
# Sigma <- tmp %*% t(tmp)
# 
# tau <- sqrt(diag(Sigma))
# Omega <- cov2cor(Sigma) # correlation matrix of beta

beta <- matrix(data = NA, nrow = K, ncol = J)

for (j in 1:J){
  beta[ ,j] <- mvrnorm(1, gamma %*% u[ ,j], Sigma)
}

y <- vector(mode = "numeric", length = N)
for(n in 1:N){
  y[n] <- rnorm(1, x[n, ]%*%beta[ ,jj[n]], sigma)
}

# data for level 1
simData1 <- data.frame(observation = 1:N, jj = jj, x = x, y = y)
colnames(simData1) <- c("observation", "jj", paste0("x", 1:K), "y")

# data for level 2
simData2 <- data.frame(u = u)
colnames(simData2) <- c(paste0("u", 1:J))

beta <- c(beta)
varName_beta <- vector()
for(j in 1:J){
  varName_beta <- append(varName_beta, paste0("beta[", 1:K, ",", j,"]"))
}

gamma <- c(gamma)
varName_gamma <- vector()
for(l in 1:L){
  varName_gamma <- append(varName_gamma, paste0("gamma[", 1:K, ",", l, "]"))
}

varName_L_Omega <- vector()
for(k in 1:K){
  varName_L_Omega <- append(varName_L_Omega, paste0("L_Omega[", k:K, ",", k, "]"))
} # only keep the elements in lower triangular

varName <- c("sigma", paste0("tau[", 1:K, "]"), varName_beta, 
             varName_gamma, varName_L_Omega)

knownPar <- data.frame(varName = varName,
                       true_value = c(sigma, tau, beta, gamma, 
                                      L_Omega[lower.tri(L_Omega, diag = TRUE)]))

# specify the directory to store the data file and parameter file
directory <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Write level 1 data to dataMultivariateHierarchicalLinearRegression1.csv
write.csv(simData1, file.path(directory,"dataMultivariateHierarchicalLinearRegression1.csv"), row.names = FALSE)

# Write level 2 data to dataMultivariateHierarchicalLinearRegression2.csv
write.csv(simData2, file.path(directory,"dataMultivariateHierarchicalLinearRegression2.csv"), row.names = FALSE)

# Write parameter to parMultivariateHierarchicalLinearRegression.csv
write.csv(knownPar, file.path(directory,"parMultivariateHierarchicalLinearRegression.csv"), row.names = FALSE)

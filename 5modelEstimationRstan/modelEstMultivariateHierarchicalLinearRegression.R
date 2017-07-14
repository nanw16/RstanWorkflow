# Estimation of the probability model for a multivariate hierarchical linear regression model
# Author: Nan Wu

library("rethinking")
library("tidyverse")

# Specify the directory for the data file
dataDir <-"C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"

# Specify the directory for the stan code
stanDir <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/3stanCode"

# Read simulated data to data frame simData1 (level 1) and simData2 (level 2)
simData1 <- read.csv(file.path(dataDir, "dataMultivariateHierarchicalLinearRegression1.csv"), stringsAsFactors = FALSE) %>%
  tbl_df()

simData2 <- read.csv(file.path(dataDir, "dataMultivariateHierarchicalLinearRegression2.csv"), stringsAsFactors = FALSE) %>%
  tbl_df()

reg_dat <- list(N = nrow(simData1), L = nrow(simData2), jj = simData1$jj,
                x = as.matrix(dplyr::select(simData1,contains("x"))), 
                y = simData1$y,
                u = as.matrix(simData2))

K <- ncol(reg_dat$x) # number of individual predictors
J <- ncol(reg_dat$u) # number of groups

fitModel = stan(
  file = file.path(stanDir, "multivariateHierarchicalLinearRegression.stan"), 
  data = reg_dat, iter = 2000, chains = 4, seed = 123, control = list(adapt_delta = 0.99))

# Check the summary for the parameters of the model and the log-posterior
print(fitModel)

# Extract samples for each parameter from posterior estimations
postSamples = as.data.frame(fitModel) %>%
  select(-lp__, -contains("tau_unif"), -starts_with("z")) %>%
  dplyr::mutate(sampleNum = row_number()) %>%
  tbl_df() %>%
  gather("varName", "value", -sampleNum)

modelDir <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/5modelEstimationRstan"

# Save the posterior samples for the parameters to the .csv file
write.csv(postSamples, 
          file.path(modelDir, "postMultivariateHierarchicalLinearRegression.csv"),
          row.names = FALSE)

# Plot the estimated distribution for all parameters
ggplot(subset(postSamples, grepl("sigma", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x") +
  ggtitle("Estimated posterior distribution for sigma") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(postSamples, grepl("beta", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 2) +
  ggtitle("Estimated posterior distribution for eta") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(postSamples, grepl("gamma", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 4) +
  ggtitle("Estimated posterior distribution for gamma") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(postSamples, grepl("tau", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 3) +
  ggtitle("Estimated posterior distribution for tau") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(postSamples, grepl("L_Omega", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 3) +
  ggtitle("Estimated posterior distribution for L_Omega") +
  theme(plot.title = element_text(hjust = 0.5))
# We may plot smoothed density function as well using geom_desity()

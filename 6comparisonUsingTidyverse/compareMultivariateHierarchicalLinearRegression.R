# Comparing the estiamted parameter distribution with the known parameter
# for the multivariate hierarchical linear regression model
# Author: Nan Wu

library("tidyverse")
# Specify the directory for the known parameter file
parDir <-"C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"

# Specify the directory for the estimation of the model
modelDir <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/5modelEstimationRstan"

# Read known parameters to data frame knowPar
knownPar <- read.csv(file.path(parDir, "parMultivariateHierarchicalLinearRegression.csv"),
                     stringsAsFactors = FALSE) %>%
  tbl_df()

# Read posterior samples to data frame postSamples
postSamples <- read.csv(
  file.path(modelDir, "postMultivariateHierarchicalLinearRegression.csv"),
  stringsAsFactors = FALSE) %>%
  tbl_df() %>%
  right_join(knownPar, by = "varName")

# Plot the estimated distribution for all parameters
ggplot(subset(postSamples, grepl("sigma", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x") +
  geom_vline(aes(xintercept = true_value), color = "red", size = 1.5) +
  ggtitle("Estimated posterior distribution for sigma") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(postSamples, grepl("beta", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 2) +
  geom_vline(aes(xintercept = true_value), color = "red", size = 1.5) +
  ggtitle("Estimated posterior distribution for eta") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(postSamples, grepl("gamma", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 4) +
  geom_vline(aes(xintercept = true_value), color = "red", size = 1.5) +
  ggtitle("Estimated posterior distribution for gamma") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(postSamples, grepl("tau", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 3) +
  geom_vline(aes(xintercept = true_value), color = "red", size = 1.5) +
  ggtitle("Estimated posterior distribution for tau") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(postSamples, grepl("L_Omega", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 3) +
  geom_vline(aes(xintercept = true_value), color = "red", size = 1.5) +
  ggtitle("Estimated posterior distribution for L_Omega") +
  theme(plot.title = element_text(hjust = 0.5))


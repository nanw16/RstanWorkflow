# Comparing the estiamted parameter distribution with the known parameter
# for the multi-predictor logit regression model
# Author: Nan Wu

library("tidyverse")
# Specify the directory for the known parameter file
parDir <-"C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"

# Specify the directory for the estimation of the model
modelDir <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/5modelEstimationRstan"

# Read known parameters to data frame knowPar
knownPar <- read.csv(file.path(parDir, "parHierarchicalLogisticRegression.csv"),
                     stringsAsFactors = FALSE) %>%
  tbl_df()

# Read posterior samples to data frame postSamples
postSamples <- 
  read.csv(file.path(modelDir, "postHierarchicalLogisticRegression.csv"),
           stringsAsFactors = FALSE) %>%
  tbl_df() %>%
  left_join(knownPar, by = "varName")

ggplot(subset(postSamples, grepl("mu", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 3) +
  geom_vline(aes(xintercept = true_value), color = "red", size = 1.5) +
  ggtitle("Comparison of model estimation for mu and true values for mu") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(postSamples, grepl("sigma", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 3) +
  geom_vline(aes(xintercept = true_value), color = "red", size = 1.5) +
  ggtitle("Comparison of model estimation for sigma and true values for sigma") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(postSamples, grepl("beta", postSamples$varName)), aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 3) +
  geom_vline(aes(xintercept = true_value), color = "red", size = 1.5) +
  ggtitle("Comparison of model estimation for beta and true values for beta") +
  theme(plot.title = element_text(hjust = 0.5))

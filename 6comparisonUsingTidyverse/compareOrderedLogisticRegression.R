# Comparing the estiamted parameter distribution with the known parameter
# for the ordered logistic regression model
# Author: Nan Wu

library("tidyverse")

# Specify the directory for the known parameter file
parDir <-"C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"

# Specify the directory for the estimation of the model
modelDir <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/5modelEstimationRstan"

# Read known parameters to data frame knowPar
knownPar <- read.csv(file.path(parDir, "parOrderedLogisticRegression.csv"),
                     stringsAsFactors = FALSE) %>%
  tbl_df()

# Read posterior samples to data frame postSamples
postSamples <- read.csv(file.path(modelDir, "postOrderedLogisticRegression.csv"),
                        stringsAsFactors = FALSE) %>%
  tbl_df() %>%
  left_join(knownPar, by = "varName")

ggplot(data = postSamples, aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 3) +
  geom_vline(aes(xintercept = true_value), color = "red", size = 1.5) +
  ggtitle("Comparison of model estimation and true parameters") +
  theme(plot.title = element_text(hjust = 0.5))

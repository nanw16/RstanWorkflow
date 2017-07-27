# Estimation of the probability model for an AR(K) model
# Author: Nan Wu

library("rstan")
library("tidyverse")

# Specify the directory for the data file
dataDir <-"C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"

# Specify the directory for the stan code
stanDir <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/3stanCode"

# Read simulated data to data frame simData
simData <- read.csv(file.path(dataDir, "dataARModel.csv"),
                    stringsAsFactors = FALSE) %>%
  tbl_df()


reg_dat <- list(N = nrow(simData), K = simData$K[1], y = simData$y)

fitARModel = stan(
  file = file.path(stanDir, "ARModel.stan"), data = reg_dat,
  iter = 20000, chains = 4, thin = 10, seed = 123)

# Check the summary for the parameters of the model and the log-posterior
print(fitARModel)
# traceplot(fitARModel, pars = c("alpha", "beta", "sigma"))

# Extract samples for each parameter from posterior estimations
postSamples = as.data.frame(fitARModel) %>%
  select(-lp__) %>%
  dplyr::mutate(sampleNum = row_number()) %>%
  tbl_df() %>%
  gather("varName", "value", -sampleNum)

modelDir <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/5modelEstimationRstan"

# Save the posterior samples for the parameters to the .csv file
write.csv(postSamples, file.path(modelDir, "postARModel.csv"),
          row.names = FALSE)

# Plot the estimated distribution for all parameters
ggplot(data = postSamples, aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x") +
  ggtitle("Estimated posterior distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# We may plot smoothed density function as well using geom_desity()

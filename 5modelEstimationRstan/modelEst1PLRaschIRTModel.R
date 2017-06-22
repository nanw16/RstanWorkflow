# Estimation of the probability model for 1PL IRT model
# Author: Nan Wu

library("rstan")
library("tidyverse")

# Specify the directory for the data file
dataDir <-"C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"

# Specify the directory for the stan code
stanDir <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/3stanCode"

# Read simulated data to data frame simData
simData <- read.csv(file.path(dataDir, "data1PLRaschIRTModel.csv"),
                    stringsAsFactors = FALSE) %>%
  tbl_df()

reg_dat <- list(N = nrow(simData), jj = simData$jj, kk = simData$kk, y = simData$y)

J <- length(unique(reg_dat$jj)) # number of students
K <- length(unique(reg_dat$kk)) # number of questions

fit1PLModel = stan(
  file = file.path(stanDir, "1PLRaschIRTModel.stan"), data = reg_dat,
  iter = 6000, chains = 4, control = list(adapt_delta = 0.8))

# Check the summary for the parameters of the model and the log-posterior
print(fit1PLModel)
# traceplot(fitLogitRegModel, pars = c("alpha", "beta", "sigma"))

# Extract samples for each parameter from posterior estimations
postSamples = as.data.frame(fit1PLModel) %>%
  select(-lp__, -contains("alpha_raw"), -mu_alpha) %>%
  dplyr::mutate(sampleNum = row_number()) %>%
  tbl_df() %>%
  gather("varName", "value", -sampleNum)

modelDir <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/5modelEstimationRstan"

# Save the posterior samples for the parameters to the .csv file
write.csv(postSamples, file.path(modelDir, "post1PLRaschIRTModel.csv"),
          row.names = FALSE)

# Plot the estimated distribution for all parameters
ggplot(data = postSamples, aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 4) +
  ggtitle("Estimated posterior distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# ggplot(subset(postSamples, grepl("sigma", postSamples$varName)), aes(x = value)) + 
#   geom_histogram(color = "black", fill = "sky blue") + 
#   facet_wrap(~varName, scales = "free_x", ncol = 3) +
#   ggtitle("Estimated posterior distribution for sigma") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ggplot(subset(postSamples, grepl("beta", postSamples$varName)), aes(x = value)) + 
#   geom_histogram(color = "black", fill = "sky blue") + 
#   facet_wrap(~varName, scales = "free_x", ncol = 6) +
#   ggtitle("Estimated posterior distribution for beta") +
#   theme(plot.title = element_text(hjust = 0.5))

# We may plot smoothed density function as well using geom_desity()

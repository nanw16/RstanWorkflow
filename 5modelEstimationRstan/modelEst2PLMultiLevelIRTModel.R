# Estimation of the probability model for MultiLevel 2PL IRT Model
# Author: Nan Wu

library("rstan")
library("tidyverse")

set.seed(123)

# Specify the directory for the data file
dataDir <-"C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/4simulatedDataGenInR"
# Specify the directory for the stan code
stanDir <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/3stanCode"

# Read simulated data to data frame simData
simData <- read.csv(file.path(dataDir, "data2PLMultiLevelIRTModel.csv"),
                    stringsAsFactors = FALSE) %>%
  tbl_df()

reg_dat <- list(N = nrow(simData), jj = simData$jj, kk = simData$kk, y = simData$y)

J <- length(unique(reg_dat$jj)) # number of students
K <- length(unique(reg_dat$kk)) # number of questions

initf <- function(chain_id = 1){
  list(mu_beta = rcauchy(1, 0, 5), sigma_beta = rhalfcauchy(1, scale = 5),
       sigma_gamma = rhalfcauchy(1, scale = 5))
}

n_chains <- 4
init_ll <- lapply(1:n_chains, function(id) initf(chain_id = id))

fit2PLModel = stan(
  file = file.path(stanDir, "2PLMultiLevelIRTModel.stan"), data = reg_dat,
  iter = 2000, chains = 4, init = init_ll, control = list(adapt_delta = 0.99))

# Check the summary for the parameters of the model and the log-posterior
print(fit2PLModel)

# Extract samples for each parameter from posterior estimations
postSamples = as.data.frame(fit2PLModel) %>%
  select(-lp__, -contains("_raw"), -contains("_unif"), -contains("log_")) %>%
  dplyr::mutate(sampleNum = row_number()) %>%
  tbl_df() %>%
  gather("varName", "value", -sampleNum)

modelDir <- "C:/Nan Wu/study/Udel/PHD/summer paper/RstanWorkflow/5modelEstimationRstan"

# Save the posterior samples for the parameters to the .csv file
write.csv(postSamples, file.path(modelDir, "post2PLMultiLevelIRTModel.csv"),
          row.names = FALSE)

# Plot the estimated distribution for all parameters
ggplot(data = postSamples, aes(x = value)) + 
  geom_histogram(color = "black", fill = "sky blue") + 
  facet_wrap(~varName, scales = "free_x", ncol = 4) +
  ggtitle("Estimated posterior distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# We may plot smoothed density function as well using geom_desity()

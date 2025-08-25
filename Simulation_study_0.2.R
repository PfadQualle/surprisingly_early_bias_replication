## --------------------------- ##########################
##
## Script name: Simulation_study_0.2.R
##
## Purpose of script: Simulate a perfect forecasters calibration for the research project 'OptimalCalibration'
##
## Author: Niklas Valentin Lehmann
##
## Date Created: 2025-06-17
##
## Copyright (c) Niklas Valentin Lehmann, 2025
## Email: Niklas-Valentin.Lehmann@vwl.tu-freiberg.de
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------- #########################



### Load Library packages ---------------------------

library(tidyverse)



### Contents ---------------------------

# 1. Create events 

# 2. Get predictions 

# 3. Simulate event history

# 4. Calculate calibration/empirical reliability

###############################################
### ########## 1. Create events ######## #######
###############################################

set.seed(142857)  # For reproducibility

# Set the number of events and final time T
num_distributions <- 100000
T <- 1


# Sample random means and variances as specified
means <- runif(num_distributions, min = 0, max = T)
variances <- runif(num_distributions, min = 0, max = 0.5 * T)
sds <- sqrt(variances)


# #OPTIONAL ------------------------------------------------------
# Plot a sample of 10 distributions
# plot(NULL, xlim = range(x), ylim = c(0, 2), xlab = "x", ylab = "Density",
#      main = "Sample of Random Normal Distributions")
# 
# for (i in 1:10) {
#   y <- dnorm(x, mean = means[i], sd = sds[i])
#   lines(x, y, col = rgb(0, 0, 1, 0.2))
# }
# #----------------------------------------------------------------



###############################################
### ########## 2. Get predictions ######## #######
###############################################


# initialize dataframe with predictions and outcomes
t0 <- tibble(
            censoring_date = runif(num_distributions, min = 0, max = T), #Define the question, probability that X=1 until t?
            prediction = NA_real_ #perfect prediction will be stored here
            ) # outcome (to be added later): resolved questions will be (X=1 OR X=0) depending on whether or not the event occurred; NA otherwise


# calculate predictions 

for (i in c(1:num_distributions)) {
  
  t0$prediction[i] <- pnorm(t0$censoring_date[i], mean = means[i], sd = sds[i]) - pnorm(0, mean = means[i], sd = sds[i])
  
}


# #OPTIONAL ------------------------------------------------------
# Plot predictions
# hist(t0$prediction, breaks = seq(0, 1, by = 0.01), main = "Histogram of Predictions",
#      xlab = "Prediction", ylab = "Count", col = "skyblue", border = "white", xlim = c(0, 1))
# #----------------------------------------------------------------


###############################################
### ########### 3. Simulate event history ######## #######
###############################################
  
#We create a function that simulates outcomes at an independent point in time t between 0 and 1 
simulate_outcomes <- function(t){

  #initialize the outcome vectors
  Expectation_X <- rep(NA,num_distributions)
  X_t <- rep(NA,num_distributions)
  
  
for (i in c(1:num_distributions)) { 
  
  # calculate the probability that an event has occurred at this time
  Expectation_X[i] <- pnorm(min(t,t0$censoring_date[i]), mean = means[i], sd = sds[i]) - pnorm(0, mean = means[i], sd = sds[i])
  
  # mark all events that have been closed/censored
  X_t[i] <- ifelse(t0$censoring_date[i] < t, 0, NA)  
  
  # mark all events that have happened - otherwise do nothing
  X_t[i] <- ifelse(runif(1,0,1) < Expectation_X[i], 1, X_t[i]) 
  
}

#return the outcome vector
return(X_t)
}


## ----------------------------------------------------

### Simulate 10 glimpses in time and store them

for (t in seq(0,1,by=0.1)) {
  
  #Get outcomes and store as new column
  t0[[paste0("X_", t * 10)]] <- simulate_outcomes(t)
  
}


###############################################
### ########### # 4. Calculate calibration/empirical reliability ######## #######
###############################################


# build and initialize the dataset for the calibration
calibration <- list(X_1 = data.frame(prediction = seq(0.01,0.99, by = 0.01), frequency= rep(NA,99)),
                    X_2 = data.frame(prediction = seq(0.01,0.99, by = 0.01), frequency= rep(NA,99)),
                    X_3 = data.frame(prediction = seq(0.01,0.99, by = 0.01), frequency= rep(NA,99)),
                    X_4 = data.frame(prediction = seq(0.01,0.99, by = 0.01), frequency= rep(NA,99)),
                    X_5 = data.frame(prediction = seq(0.01,0.99, by = 0.01), frequency= rep(NA,99)),
                    X_6 = data.frame(prediction = seq(0.01,0.99, by = 0.01), frequency= rep(NA,99)),
                    X_7 = data.frame(prediction = seq(0.01,0.99, by = 0.01), frequency= rep(NA,99)),
                    X_8 = data.frame(prediction = seq(0.01,0.99, by = 0.01), frequency= rep(NA,99)),
                    X_9 = data.frame(prediction = seq(0.01,0.99, by = 0.01), frequency= rep(NA,99)),
                    X_10 = data.frame(prediction = seq(0.01,0.99, by = 0.01), frequency= rep(NA,99))
)

# We need to categorize, i.e. round the predictions to make them work for our purposes
t0$prediction <- round(t0$prediction,2)

# Predictions cannot be definite, so we need to change them too

t0$prediction <- ifelse(t0$prediction == 0, 0.01,t0$prediction)
t0$prediction <- ifelse(t0$prediction == 1, 0.99,t0$prediction)


for (k in 1:99){
  
  ##filter by prediction 
  temp <- t0 |> 
    filter(prediction == k/100) 
  
  # Count the number of ones/zeros across these columns whilst ignoring NAs
  temp_1 <- temp |> summarise(across(c(X_1,X_2,X_3,X_4,X_5,X_6,X_7,X_8,X_9, X_10),  ~sum(. == 1, na.rm = TRUE)))
  temp_0 <- temp |> summarise(across(c(X_1,X_2,X_3,X_4,X_5,X_6,X_7,X_8,X_9,X_10),  ~sum(. == 0, na.rm = TRUE)))
  
  
  # divides the number of positive outcomes with the total number of outcomes
  calibration$X_1$frequency[k] <- temp_1$X_1 / (temp_1$X_1 + temp_0$X_1)
  calibration$X_2$frequency[k] <- temp_1$X_2 / (temp_1$X_2 + temp_0$X_2) 
  calibration$X_3$frequency[k] <- temp_1$X_3 / (temp_1$X_3 + temp_0$X_3) 
  calibration$X_4$frequency[k] <- temp_1$X_4 / (temp_1$X_4 + temp_0$X_4) 
  calibration$X_5$frequency[k] <- temp_1$X_5 / (temp_1$X_5 + temp_0$X_5) 
  calibration$X_6$frequency[k] <- temp_1$X_6 / (temp_1$X_6 + temp_0$X_6) 
  calibration$X_7$frequency[k] <- temp_1$X_7 / (temp_1$X_7 + temp_0$X_7) 
  calibration$X_8$frequency[k] <- temp_1$X_8 / (temp_1$X_8 + temp_0$X_8) 
  calibration$X_9$frequency[k] <- temp_1$X_9 / (temp_1$X_9 + temp_0$X_9) 
  calibration$X_10$frequency[k] <- temp_1$X_10 / (temp_1$X_10 + temp_0$X_10) 
  
  
} 


### Make nice plots ------------------------------------------------------------

plot <- ggplot() +
  geom_point(data = calibration$X_10, aes(x = prediction, y = frequency, color = "t=10"), size = 1.5, alpha = 0.5) +
  geom_smooth(data = calibration$X_10, aes(x = prediction, y = frequency, color = "t=10"), linewidth = 0.5, se=FALSE) +
  geom_point(data = calibration$X_8, aes(x = prediction, y = frequency, color = "t=8"), size = 1.5, alpha = 0.6) +
  #geom_smooth(data = calibration$X_8, aes(x = prediction, y = frequency, color = "t=8"), linewidth = 0.5, se=FALSE) +
  geom_point(data = calibration$X_6, aes(x = prediction, y = frequency, color = "t=6"), size = 1.5, alpha = 0.75) +
  #geom_smooth(data = calibration$X_6, aes(x = prediction, y = frequency, color = "t=6"), linewidth = 0.5, se=FALSE) +
  geom_point(data = calibration$X_4, aes(x = prediction, y = frequency, color = "t=4"), size = 1.5, alpha = 0.8) +
  #geom_smooth(data = calibration$X_4, aes(x = prediction, y = frequency, color = "t=4"), linewidth = 0.5, se=FALSE) +
  geom_point(data = calibration$X_2, aes(x = prediction, y = frequency, color = "t=2"), size = 1.5, alpha = 1) +
  #geom_smooth(data = calibration$X_2, aes(x = prediction, y = frequency, color = "t=2"), linewidth = 0.5, se=FALSE) +
  scale_color_manual(name = "Point in time", 
                     values = c("t=10" = "#BD0026",
                                #"t=9" = "red",
                                "t=8" = "#F03B20",
                                #"t=7" = "blue",
                                "t=6" = "#FD8D3C",
                                #"t=5" = "blue",
                                "t=4" = "#FECC5C",
                                #"t=3" = "blue",
                                "t=2" = "#FFFFB2"
                                #"t=1" = "blue"
                                )) +
  xlab("Prediction") +
  ylab("Frequency") +
  ggtitle("Calibration by time of observation") +
  theme_minimal()






### End of script ---------------------------------------------------









